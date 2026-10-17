{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Match where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, freshKirName, freshKirLocalId)
import Xast.Lowerer.Types
import Xast.Lowerer.DecisionTree
import Xast.Lowerer.Mono (monoType, mangleCtor)
import Control.Monad (forM, foldM)
import Control.Monad.State (gets)
import Xast.Utils.Generic (todo__)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as M
import Xast.Lowerer.Expr (lowerExpr)

patTy :: Pattern Desugared -> Type
patTy = (.ty) . patAnnotation

lowerMatchTo
   :: M.Map LocalId KirValue
   -> Type
   -> KirDest
   -> Match Desugared
   -> Lowerer [KirInstruct]
lowerMatchTo env ty dest match = do
   let (rootExprs, tupled) = case match.baseExpr of
         ExpTuple _ es -> (es, True)
         e             -> ([e], False)
   let n = length rootExprs

   lowered <- mapM (lowerExpr env) rootExprs
   let scrutInstrs = concatMap fst lowered
   let rootVals = map snd lowered

   spans <- gets (.ctorSpans)
   let rows =
         [ Row (map (toPat spans) (rowPatterns tupled n pat)) [] i
         | (i, MatchWing pat _) <- zip [0 ..] match.matches
         ]
   let occs = [Occ i [] | i <- [0 .. n - 1]]
   let tree = compilePatterns occs rows

   let ctx = DTCtx
         { ctxEnv    = env
         , ctxTy     = ty
         , ctxBodies = [body | MatchWing _ body <- match.matches]
         }
   let st0 = OccState
         { vals   = M.fromList (zip occs rootVals)
         , fields = M.empty
         }

   treeInstrs <- lowerTreeInto ctx st0 dest tree
   pure (scrutInstrs ++ treeInstrs)

rowPatterns :: Bool -> Int -> Pattern Desugared -> [Pattern Desugared]
rowPatterns tupled n = \case
   PatTuple _ ps | tupled, length ps == n -> ps
   PatTuple _ [p] | n == 1 -> [p]
   PatTuple {} -> todo__ "tuple patterns are not supported by the lowerer yet"
   p | n == 1 -> [p]
   _ -> todo__ "pattern does not match the scrutinee columns"

toPat :: M.Map ConstructorId Int -> Pattern Desugared -> Pat
toPat spans = \case
   PatVar info ident ->
      case info.res of
         Just (ResLocal lid) -> PVar lid
         _ -> todo__ ("unresolved variable pattern " ++ show ident ++ " in match arm")

   PatWildcard _ -> PWildCard

   -- literals never form a complete set, 
   -- so they get an unbounded span
   PatLit _ lit -> PCon (Con (TagLit lit) 0 maxBound []) []

   PatCon info ident args ->
      case info.res of
         Just (ResConstructor cid) ->
            let span' = M.findWithDefault maxBound cid spans
                con = Con (TagCtor (mangleCtor info.ty ident)) (length args) span' (map patTy args)
            in PCon con (map (toPat spans) args)
         _ -> todo__ ("unresolved constructor pattern " ++ show ident)

   PatTuple _ [p] -> toPat spans p
   _ -> todo__ "only literal, variable, wildcard and constructor patterns are supported in match arms yet"

data DTCtx = DTCtx
   { ctxEnv    :: M.Map LocalId KirValue
   , ctxTy     :: Type
   , ctxBodies :: [Expr Desugared]
   }

data OccState = OccState
   { vals   :: M.Map Occ KirValue
   , fields :: M.Map Occ (Type, Ident) 
   }

resolveOcc :: OccState -> Occ -> Lowerer ([KirInstruct], OccState, KirValue)
resolveOcc st occ = case M.lookup occ st.vals of
   Just v -> pure ([], st, v)
   Nothing -> case (occ, M.lookup occ st.fields) of
      (Occ root path@(_ : _), Just (fieldTy, ctor)) -> do
         (pre, st1, parentVal) <- resolveOcc st (Occ root (init path))
         out <- freshKirName
         fieldTy' <- monoType fieldTy
         let instr = KirField fieldTy' parentVal ctor (last path) out
         let v = KirVar out
         pure (pre ++ [instr], st1 { vals = M.insert occ v st1.vals }, v)
      _ -> todo__ ("occurrence has no known type: " ++ show occ)

lowerTreeInto :: DTCtx -> OccState -> KirDest -> DecisionTree -> Lowerer [KirInstruct]
lowerTreeInto ctx st dest = \case
   DTSwap _ t -> lowerTreeInto ctx st dest t

   -- no arm can match
   DTFail -> pure []

   -- undoubted match
   leaf@(DTLeaf {}) -> do
      branch <- lowerBranch ctx st leaf
      pure $ case branch of
         Just b  -> b.instructs ++ [KirStore ctx.ctxTy dest b.value]
         Nothing -> []

   DTSwitch occ cases def -> do
      (pre, st1, scrut) <- resolveOcc st occ

      armBranches <- forM cases $ \(con, sub) -> do
         let childFields = M.fromList
               [ (occField occ k, (fty, ctor))
               | TagCtor ctor <- [con.tag]
               , (k, fty) <- zip [0 ..] con.fieldTys
               ]
         let st2 = OccState { vals = st1.vals, fields = M.union childFields st1.fields }
         branch <- lowerBranch ctx st2 sub
         pure (con, branch)

      fallback <- case def of
         Nothing -> pure Nothing
         Just t -> lowerBranch ctx st1 t

      -- arms that can never succeed are dropped
      let arms = [KirMatchArm { tag = con.tag, branch = b } | (con, Just b) <- armBranches]
      let exhaustive = isNothing def && all (isJust . snd) armBranches

      pure (pre ++ [KirMatch
         { ty = ctx.ctxTy
         , scrutinee = scrut
         , arms = arms
         , fallback = fallback
         , dest = dest
         , exhaustive = exhaustive
         }])

lowerBranch :: DTCtx -> OccState -> DecisionTree -> Lowerer (Maybe KirBranch)
lowerBranch ctx st = \case
   DTSwap _ t -> lowerBranch ctx st t
   DTFail -> pure Nothing

   DTLeaf n binds -> do
      (pre, _, env') <- foldM bindOcc ([], st, ctx.ctxEnv) binds
      (instrs, val) <- lowerExpr env' (ctx.ctxBodies !! n)
      pure (Just KirBranch { instructs = pre ++ instrs, value = val })

   sw@(DTSwitch {}) -> do
      result <- freshKirLocalId
      instrs <- lowerTreeInto ctx st (DestLocal result) sw
      pure (Just KirBranch { instructs = instrs, value = KirLocalRef result })
   where
      bindOcc (pre, st', env) (lid, occ) = do
         (instrs, st'', v) <- resolveOcc st' occ
         pure (pre ++ instrs, st'', M.insert lid v env)
