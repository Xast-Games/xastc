{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, freshKirName, freshKirLocalId, runLowerer)
import Xast.Lowerer.Types
import Xast.Lowerer.DecisionTree
import Xast.Lowerer.Mono (monoType, drainTypes)
import Control.Monad (forM, when, foldM)
import Control.Monad.State (gets)
import Xast.Utils.Generic (todo__)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Map as M
import Control.Monad.Identity (Identity(runIdentity))

lowerPrograms :: M.Map ConstructorId Int -> [Program Desugared] -> ([Kira], [KirTypeDef])
lowerPrograms spans progs =
   let lowered = forM progs lowerProgram <* drainTypes
       ((irs, typeDefs), _) = runIdentity $
         runLowerer (newLowerState spans progs) lowered
   in (irs, typeDefs)

lowerProgram :: Program Desugared -> Lowerer Kira
lowerProgram prog = do
   let systemImpls = [x | StmtSystem (SysImpl x) <- prog.stmts]
   let funcImpls   = [x | StmtFunc (FnImpl x) <- prog.stmts]

   systems   <- forM systemImpls (lowerSystem prog.moduleDef.name)
   functions <- forM funcImpls (lowerFunction prog.moduleDef.name)

   return $ Kira
      prog.moduleDef.name
      systems
      functions

lowerSystem :: Module -> SystemImpl Desugared -> Lowerer KirSystem
lowerSystem module' impl = do
   when (isJust impl.with) $
      todo__ "`with` entities are not supported yet"

   when (length impl.entities /= 1) $
      todo__ "0 or 2+ entities are not supported yet"

   let EntityPattern rawBindings = head impl.entities
   let mkBinding bid (EntPatBinding pat access) =
         (bid, KirBinding
            { bindType   = patTy pat
            , bindSrc    = SrcEntity
            , bindAccess = access
            })

   bs <- forM (zipWith mkBinding (map KirBindingId [0..]) rawBindings) $ \(bid, b) -> do
      ty <- monoType b.bindType
      pure (bid, b { bindType = ty })
   let env = M.fromList
         [ (lid, KirBindingRef bid)
         | (bid, (pat, _)) <- zip (map fst bs) (map (\(EntPatBinding p a) -> (p, a)) rawBindings)
         , Just (ResLocal lid) <- [(patAnnotation pat).res]
         ]

   (target, targetTy) <- case [(bid, b.bindType) | (bid, b) <- bs, b.bindAccess == AccessWrite] of
      [pair] -> pure pair
      _      -> todo__ "a system must write exactly one binding for now"

   bodyInstrs <- case impl.body of
      ExpMatch _ match -> lowerMatchTo env targetTy (DestBinding target) match
      _ -> todo__ "only a top-level `match` system body is supported yet"

   let name = KirName (namespacedName module' impl.name)
   let bindings = map snd bs
   let body = KirBlock { instructs = bodyInstrs, term = KirReturn Nothing }

   return KirSystem {..}

patTy :: Pattern Desugared -> Type
patTy = (.ty) . patAnnotation

lowerFunction :: Module -> FuncImpl Desugared -> Lowerer KirFunction
lowerFunction module' impl = do
   match <- case impl.body of
      ExpMatch _ m -> pure m
      _ -> todo__ "only a top-level `match` function body is supported yet"

   paramExprs <- case match.baseExpr of
      ExpTuple _ es -> pure es
      _ -> todo__ "function bodies must match on their argument tuple"

   paramInfos <- forM paramExprs $ \case
      ExpVar info _ ident ->
         case info.res of
            Just (ResLocal l) -> pure (l, ident, info.ty)
            _ -> todo__ ("unresolved function parameter " ++ show ident)
      _ -> todo__ "function parameters must be plain variables"

   let env = M.fromList [(lid, KirVar (KirName ident.inner)) | (lid, ident, _) <- paramInfos]
   retTy <- monoType (exprAnnotation impl.body).ty

   resultId <- freshKirLocalId
   bodyInstrs <- lowerMatchTo env retTy (DestLocal resultId) match

   let name = KirName (namespacedName module' impl.name)
   params <- forM paramInfos $ \(_, ident, pty) -> do
      ty <- monoType pty
      pure KirParam { ty = ty, name = KirName ident.inner }
   let body = KirBlock { instructs = bodyInstrs, term = KirReturn (Just (KirLocalRef resultId)) }

   return KirFunction {..}

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
                con = Con (TagCtor ident) (length args) span' (map patTy args)
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
         let instr = KirField fieldTy parentVal ctor (last path) out
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

lowerExpr :: M.Map LocalId KirValue -> Expr Desugared -> Lowerer ([KirInstruct], KirValue)
lowerExpr env expr = case expr of
   ExpLit _ lit -> pure ([], KirConst (TagLit lit))

   ExpCon info _ ident ->
      case info.res of
         Just (ResConstructor _) -> pure ([], KirConst (TagCtor ident))
         _ -> todo__ ("unresolved constructor: " ++ show ident)

   ExpTuple _ [e] -> lowerExpr env e
   ExpTuple {} -> todo__ "tuples with more than one element are not supported by the lowerer yet"

   ExpVar info _ ident ->
      case info.res of
         Just (ResLocal lid) ->
            case M.lookup lid env of
               Just v  -> pure ([], v)
               Nothing -> todo__ ("unbound local variable " ++ show ident ++ " in lowering")
         _ -> todo__ ("only resolved local variable references are supported in the lowerer yet, got " ++ show ident)

   ExpApp {} -> do
      let spine (ExpApp (DesugaredInfo ty _) f x) acc Nothing = spine f (x : acc) (Just ty)
          spine (ExpApp _ f x) acc (Just inferred) = spine f (x : acc) (Just inferred)
          spine (ExpVar _ _ ident) acc accTy = (fromJust accTy, ident, acc)
          spine _ _ _ = todo__ "only direct function application is supported by the lowerer yet"
          (retType, fnIdent, args) = spine expr [] Nothing

      lowered <- mapM (lowerExpr env) args
      resultName <- freshKirName
      let instrs = concatMap fst lowered ++ [KirCall retType (KirName fnIdent.inner) (map snd lowered) resultName]
      pure (instrs, KirVar resultName)

   ExpLetIn _ letIn -> do
      (bindInstrs, env') <- lowerLetBindings env letIn.bindings
      (bodyInstrs, bodyVal) <- lowerExpr env' letIn.bindExpr
      pure (bindInstrs ++ bodyInstrs, bodyVal)

   _ -> todo__ "this expression form is not supported by the lowerer yet"

lowerLetBindings
   :: M.Map LocalId KirValue
   -> [Let Desugared]
   -> Lowerer ([KirInstruct], M.Map LocalId KirValue)
lowerLetBindings env [] = pure ([], env)
lowerLetBindings env (Let pat value : rest) = do
   (valInstrs, val) <- lowerExpr env value
   
   case pat of
      PatVar info ident ->
         case info.res of
            Just (ResLocal lid) -> do
               (restInstrs, env') <- lowerLetBindings (M.insert lid val env) rest
               pure (valInstrs ++ restInstrs, env')
            _ -> todo__ ("unresolved let binding " ++ show ident)

      PatWildcard _ -> do
         (restInstrs, env') <- lowerLetBindings env rest
         pure (valInstrs ++ restInstrs, env')

      _ -> todo__ "only variable and wildcard patterns are supported in let bindings yet"
