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
lowerMatchTo env resultType dest match = do
   let (rootExprs, tupled) = case match.baseExpr of
         ExpTuple _ elements -> (elements, True)
         singleExpr          -> ([singleExpr], False)
   let columnCount = length rootExprs

   lowered <- mapM (lowerExpr env) rootExprs
   let scrutInstrs = concatMap fst lowered
   let rootVals = map snd lowered

   spans <- gets (.ctorSpans)
   fieldNamesOfCtor <- gets (.ctorFieldNames)
   let rows =
         [ Row (map (toPat spans fieldNamesOfCtor) (rowPatterns tupled columnCount pattern')) [] armIndex
         | (armIndex, MatchWing pattern' _) <- zip [0 ..] match.matches
         ]
   let occs = [Occ column [] | column <- [0 .. columnCount - 1]]
   let tree = compilePatterns occs rows

   let context = DTCtx
         { ctxEnv    = env
         , ctxTy     = resultType
         , ctxBodies = [body | MatchWing _ body <- match.matches]
         }
   let initialState = OccState
         { vals   = M.fromList (zip occs rootVals)
         , fields = M.empty
         }

   treeInstrs <- lowerTreeInto context initialState dest tree
   pure (scrutInstrs ++ treeInstrs)

rowPatterns :: Bool -> Int -> Pattern Desugared -> [Pattern Desugared]
rowPatterns tupled columnCount = \case
   PatTuple _ subPatterns | tupled, length subPatterns == columnCount -> subPatterns
   PatTuple _ [single] | columnCount == 1 -> [single]
   PatTuple {} -> todo__ "tuple patterns are not supported by the lowerer yet"
   other | columnCount == 1 -> [other]
   _ -> todo__ "pattern does not match the scrutinee columns"

toPat
   :: M.Map ConstructorId Int  -- ^ constructor id -> number of constructors of its type
   -> M.Map Ident [Ident]      -- ^ constructor name -> its declared field names
   -> Pattern Desugared
   -> Pat
toPat spans fieldNamesOfCtor = \case
   PatVar info ident ->
      case info.res of
         Just (ResLocal localId) -> PVar localId
         _ -> todo__ ("unresolved variable pattern " ++ show ident ++ " in match arm")

   PatWildcard _ -> PWildCard

   -- literals never form a complete set, 
   -- so they get an unbounded span
   PatLit _ lit -> PCon (Con (TagLit lit) 0 maxBound [] []) []

   PatCon info ident args ->
      case info.res of
         Just (ResConstructor cid) ->
            let span' = M.findWithDefault maxBound cid spans
                declaredFieldNames = M.findWithDefault [] ident fieldNamesOfCtor
                constructor = Con (TagCtor (mangleCtor info.ty ident)) (length args) span' (map patTy args) declaredFieldNames
            in PCon constructor (map (toPat spans fieldNamesOfCtor) args)
         _ -> todo__ ("unresolved constructor pattern " ++ show ident)

   PatTuple _ [single] -> toPat spans fieldNamesOfCtor single
   _ -> todo__ "only literal, variable, wildcard and constructor patterns are supported in match arms yet"

data DTCtx = DTCtx
   { ctxEnv    :: M.Map LocalId KirValue
   , ctxTy     :: Type
   , ctxBodies :: [Expr Desugared]
   }

data OccState = OccState
   { vals   :: M.Map Occ KirValue
   , fields :: M.Map Occ OccField
   }

-- | What is known about a field occurrence once its parent was matched.
data OccField = OccField
   { fieldType :: Type
   , ctor      :: Ident   -- ^ constructor the field belongs to
   , fieldName :: Ident
   }

resolveOcc :: OccState -> Occ -> Lowerer ([KirInstruct], OccState, KirValue)
resolveOcc state occ = case M.lookup occ state.vals of
   Just value -> pure ([], state, value)
   Nothing -> case (occ, M.lookup occ state.fields) of
      (Occ root path@(_ : _), Just occField') -> do
         (prefix, parentState, parentVal) <- resolveOcc state (Occ root (init path))
         dst <- freshKirName
         monoFieldType <- monoType occField'.fieldType
         let instr = KirField
               { ty        = monoFieldType
               , src       = parentVal
               , ctor      = occField'.ctor
               , fieldName = occField'.fieldName
               , dst       = dst
               }
         let fieldValue = KirVar dst
         pure (prefix ++ [instr], parentState { vals = M.insert occ fieldValue parentState.vals }, fieldValue)
      _ -> todo__ ("occurrence has no known type: " ++ show occ)

lowerTreeInto :: DTCtx -> OccState -> KirDest -> DecisionTree -> Lowerer [KirInstruct]
lowerTreeInto context state dest = \case
   DTSwap _ subTree -> lowerTreeInto context state dest subTree

   -- no arm can match
   DTFail -> pure []

   -- undoubted match
   leaf@(DTLeaf {}) -> do
      branch <- lowerBranch context state leaf
      pure $ case branch of
         Just found -> found.instructs ++ [KirStore context.ctxTy dest found.value]
         Nothing -> []

   DTSwitch occ cases def -> do
      (prefix, scrutState, scrut) <- resolveOcc state occ

      armBranches <- forM cases $ \(constructor, subTree) -> do
         let childFields = M.fromList
               [ (occField occ fieldIndex, OccField { fieldType = fieldType, ctor = ctor, fieldName = fieldName })
               | TagCtor ctor <- [constructor.tag]
               , (fieldIndex, fieldType, fieldName) <- zip3 [0 ..] constructor.fieldTys constructor.fieldNames
               ]
         let armState = OccState { vals = scrutState.vals, fields = M.union childFields scrutState.fields }
         branch <- lowerBranch context armState subTree
         pure (constructor, branch)

      fallback <- case def of
         Nothing -> pure Nothing
         Just defaultTree -> lowerBranch context scrutState defaultTree

      -- arms that can never succeed are dropped
      let arms = [KirMatchArm { tag = constructor.tag, branch = armBranch } | (constructor, Just armBranch) <- armBranches]
      let exhaustive = isNothing def && all (isJust . snd) armBranches

      pure (prefix ++ [KirMatch
         { ty = context.ctxTy
         , scrutinee = scrut
         , arms = arms
         , fallback = fallback
         , dest = dest
         , exhaustive = exhaustive
         }])

lowerBranch :: DTCtx -> OccState -> DecisionTree -> Lowerer (Maybe KirBranch)
lowerBranch context state = \case
   DTSwap _ subTree -> lowerBranch context state subTree
   DTFail -> pure Nothing

   DTLeaf armIndex binds -> do
      (prefix, _, env') <- foldM bindOcc ([], state, context.ctxEnv) binds
      (instrs, val) <- lowerExpr env' (context.ctxBodies !! armIndex)
      pure (Just KirBranch { instructs = prefix ++ instrs, value = val })

   switch@(DTSwitch {}) -> do
      result <- freshKirLocalId
      instrs <- lowerTreeInto context state (DestLocal result) switch
      pure (Just KirBranch { instructs = instrs, value = KirLocalRef result })
   where
      bindOcc (prefix, currentState, env) (localId, occ) = do
         (instrs, nextState, value) <- resolveOcc currentState occ
         pure (prefix ++ instrs, nextState, M.insert localId value env)
