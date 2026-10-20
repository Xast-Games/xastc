{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Unify where

import Control.Monad.State
import Control.Monad (zipWithM_, forM)
import qualified Data.Set as S
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Control.Monad.RWS (asks)

withType :: WithType -> Type
withType (WithEvent lt) = lt.node
withType (WithRes lt)   = lt.node

freshTyVar :: SemAnalyzer Type
freshTyVar = do
   currentState <- get
   let nextId = currentState.tyVarSupply
   put currentState { tyVarSupply = nextId + 1 }
   return $ TyVar nextId

freeTyVars :: Type -> S.Set Int
freeTyVars ty = go [ty] S.empty
   where
      go :: [Type] -> S.Set Int -> S.Set Int
      go [] accumulator = accumulator
      go (current:rest) accumulator = case current of
         TyVar varId      -> go rest (S.insert varId accumulator)
         TyApp func arg   -> go (func : arg : rest) accumulator
         TyTuple elements -> go (elements ++ rest) accumulator
         TyFn args ret    -> go (ret : args ++ rest) accumulator
         _                -> go rest accumulator

generalize :: Type -> SemAnalyzer [Int]
generalize ty = do
   resolvedType <- resolve ty
   vars <- asks (.vars)
   envFree <- fmap S.unions . forM (M.elems vars) $ \varInfo -> do
      varType <- resolve varInfo.ty
      pure (freeTyVars varType S.\\ S.fromList varInfo.quantified)

   return $ S.toList (freeTyVars resolvedType S.\\ envFree)

freshVarId :: SemAnalyzer VarId
freshVarId = do
   currentState <- get
   let nextId = currentState.varIdSupply
   put currentState { varIdSupply = nextId + 1 }
   return $ VarId nextId

freshLocalId :: SemAnalyzer LocalId
freshLocalId = do
   currentState <- get
   let nextId = currentState.localIdSupply
   put currentState { localIdSupply = nextId + 1 }
   return $ LocalId nextId

freshFunctionId :: SemAnalyzer FunctionId
freshFunctionId = do
   currentState <- get
   let nextId = currentState.fnIdSupply
   put currentState { fnIdSupply = nextId + 1 }
   return $ FunctionId nextId

freshConstructorId :: SemAnalyzer ConstructorId
freshConstructorId = do
   currentState <- get
   let nextId = currentState.ctorIdSupply
   put currentState { ctorIdSupply = nextId + 1 }
   return $ ConstructorId nextId

freshExternId :: SemAnalyzer ExternId
freshExternId = do
   currentState <- get
   let nextId = currentState.externIdSupply
   put currentState { externIdSupply = nextId + 1 }
   return $ ExternId nextId

-- | Assigns a fresh LocalId to every name, e.g. for a pattern's bound vars.
freshLocalScope :: [(Ident, Location)] -> SemAnalyzer (M.Map Ident (LocalId, Location))
freshLocalScope bindings = M.fromList <$> mapM (\(name, location) -> (\localId -> (name, (localId, location))) <$> freshLocalId) bindings

resolve :: Type -> SemAnalyzer Type
resolve t = do
   substitution <- gets (.tySubst)
   pure (go substitution t)
   where
      go substitution (TyVar varId) = maybe (TyVar varId) (go substitution) (M.lookup varId substitution)
      go substitution (TyApp func arg) = TyApp (go substitution func) (go substitution arg)
      go substitution (TyTuple elements) = TyTuple (map (go substitution) elements)
      go substitution (TyFn args result) = TyFn (map (go substitution) args) (go substitution result)
      go _ other = other

bindVar :: Int -> Type -> SemAnalyzer ()
bindVar varId boundType = modify $ \currentState -> currentState { tySubst = M.insert varId boundType currentState.tySubst }

zonkType :: M.Map Int Type -> Type -> Type
zonkType substitution = go
   where
      go (TyVar varId) = maybe (TyVar varId) go (M.lookup varId substitution)
      go (TyApp func arg) = TyApp (go func) (go arg)
      go (TyTuple elements) = TyTuple (map go elements)
      go (TyFn args result) = TyFn (map go args) (go result)
      go other = other

zonkTyped :: M.Map Int Type -> Typed -> Typed
zonkTyped substitution (TypedInfo location ty res) = TypedInfo location (zonkType substitution ty) res

zonkProgram :: M.Map Int Type -> Program Typed -> Program Typed
zonkProgram substitution = fmap (zonkTyped substitution)

unify :: Location -> Type -> Type -> SemAnalyzer ()
unify location left right = do
   left' <- resolve left
   right' <- resolve right
   case (left', right') of
      (TyVar n, TyVar m) | n == m -> pure ()
      (TyVar n, _) -> bindOrFail location n right'
      (_, TyVar m) -> bindOrFail location m left'

      (TyCon a, TyCon b) | a == b -> pure ()
      (TyGnr a, TyGnr b) | a == b -> pure ()

      (TyApp a1 b1, TyApp a2 b2) -> unify location a1 a2 >> unify location b1 b2
      (TyTuple xs, TyTuple ys) | length xs == length ys ->
         zipWithM_ (unify location) xs ys

      (TyFn args1 r1, TyFn args2 r2) | length args1 == length args2 -> do
         zipWithM_ (unify location) args1 args2
         unify location r1 r2

      (TyInvalid, _) -> pure ()
      (_, TyInvalid) -> pure ()

      _ -> errSem (SETypeError location left' right')

bindOrFail :: Location -> Int -> Type -> SemAnalyzer ()
bindOrFail location varId boundType = do
   occurs <- occursCheckType varId boundType
   if occurs then errSem (SEInfiniteType location varId boundType) else bindVar varId boundType

occursCheckType :: Int -> Type -> SemAnalyzer Bool
occursCheckType varId t = do
   resolvedType <- resolve t
   case resolvedType of
      TyVar otherId    -> pure (varId == otherId)
      TyApp func arg   -> (||) <$> occursCheckType varId func <*> occursCheckType varId arg
      TyTuple elements -> or <$> mapM (occursCheckType varId) elements
      TyFn args ret    -> ((||) . or <$> mapM (occursCheckType varId) args) <*> occursCheckType varId ret
      _                -> pure False

instantiate :: FuncSig -> SemAnalyzer Type
instantiate (FuncSig args ret) = do
   let generics = S.toList (foldMap collectGnrs args <> collectGnrs ret)
   fresh <- mapM (const freshTyVar) generics
   let substitution = M.fromList (zip generics fresh)
   pure $ TyFn (map (substGnr substitution) args) (substGnr substitution ret)

collectGnrs :: Type -> S.Set Ident
collectGnrs = \case
   TyGnr name       -> S.singleton name
   TyApp func arg   -> collectGnrs func <> collectGnrs arg
   TyTuple elements -> foldMap collectGnrs elements
   TyFn args result -> foldMap collectGnrs args <> collectGnrs result
   _                -> S.empty

substGnr :: M.Map Ident Type -> Type -> Type
substGnr substitution = \case
   TyGnr name       -> M.findWithDefault (TyGnr name) name substitution
   TyApp func arg   -> TyApp (substGnr substitution func) (substGnr substitution arg)
   TyTuple elements -> TyTuple (map (substGnr substitution) elements)
   TyFn args result -> TyFn (map (substGnr substitution) args) (substGnr substitution result)
   other            -> other

substTyVars :: M.Map Int Type -> Type -> Type
substTyVars substitution = \case
   TyVar varId      -> M.findWithDefault (TyVar varId) varId substitution
   TyApp func arg   -> TyApp (substTyVars substitution func) (substTyVars substitution arg)
   TyTuple elements -> TyTuple (map (substTyVars substitution) elements)
   TyFn args result -> TyFn (map (substTyVars substitution) args) (substTyVars substitution result)
   other            -> other

typeOf :: Expr Typed -> Type
typeOf = (.ty) . exprAnnotation
