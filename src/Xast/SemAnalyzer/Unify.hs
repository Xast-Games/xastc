{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Unify where

import Control.Monad.State
import Control.Monad.Reader (asks)
import Control.Monad (zipWithM_, forM, forM_, unless, filterM)
import Data.List (nubBy)
import Data.Function (on)
import qualified Data.Set as S
import qualified Data.Map as M

import Xast.Utils.Generic ((|>))
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types

withType :: WithType -> Type
withType (WithEvent lt) = lt.node
withType (WithRes lt)   = lt.node

freshTyVar :: SemAnalyzer Type
freshTyVar = TyVar <$> freshTyVarId

freshTyVarId :: SemAnalyzer Int
freshTyVarId = do
   currentState <- get
   let nextId = currentState.tyVarSupply
   put currentState { tyVarSupply = nextId + 1 }
   return nextId

-- | A fresh type variable limited to the given kind.
freshConstrainedVar
   :: Location     -- ^ the expression that introduces it
   -> TyIntrinsic
   -> SemAnalyzer Type
freshConstrainedVar origin kind = do
   varId <- freshTyVarId
   constrainVar varId TyVarConstraint { kind = kind, origin = origin }
   return $ TyVar varId

constrainVar :: Int -> TyVarConstraint -> SemAnalyzer ()
constrainVar varId constraint = modify $ \currentState -> currentState
   { tyVarConstraints = M.insert varId constraint currentState.tyVarConstraints }

lookupConstraint :: Int -> SemAnalyzer (Maybe TyVarConstraint)
lookupConstraint varId = gets (M.lookup varId . (.tyVarConstraints))

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

   -- Constrained variables stay monomorphic: each needs one concrete type.
   constrained <- gets (M.keysSet . (.tyVarConstraints))
   return $ S.toList (freeTyVars resolvedType S.\\ envFree S.\\ constrained)

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
freshLocalScope bindings = do
   scopeEntries <- forM bindings $ \(name, location) -> do
      localId <- freshLocalId
      pure (name, (localId, location))

   return $ M.fromList scopeEntries

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
      (TyInt a, TyInt b) | a == b -> pure ()

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
   if occurs
      then errSem (SEInfiniteType location varId boundType)
      else do
         checkConstraint location varId boundType
         bindVar varId boundType

-- | Checks a binding against the variable's constraint.
--   Binding to another variable passes the constraint on.
checkConstraint :: Location -> Int -> Type -> SemAnalyzer ()
checkConstraint location varId boundType = lookupConstraint varId >>= \case
   Nothing -> pure ()
   Just constraint -> case boundType of
      TyVar otherId -> passConstraint location constraint otherId
      _ -> unless (satisfiesConstraint constraint.kind boundType) $
         errSem (unsatisfiedConstraint location constraint boundType)

-- | Passes a constraint to another variable. Two different constraints
--   conflict: no type is both a number and concatenative.
passConstraint :: Location -> TyVarConstraint -> Int -> SemAnalyzer ()
passConstraint location constraint otherId = lookupConstraint otherId >>= \case
   Nothing -> constrainVar otherId constraint
   Just existing -> unless (existing.kind == constraint.kind) $
      errSem (unsatisfiedConstraint location constraint (TyInt existing.kind))

unsatisfiedConstraint :: Location -> TyVarConstraint -> Type -> TypeCheckError
unsatisfiedConstraint location constraint actual = SEUnsatisfiedConstraint
   { location   = location
   , constraint = constraint.kind
   , origin     = constraint.origin
   , actual     = actual
   }

-- | Whether a non-variable type satisfies the constraint.
satisfiesConstraint :: TyIntrinsic -> Type -> Bool
satisfiesConstraint kind type' = case (kind, type') of
   -- `number` in a Std signature satisfies itself
   (_, TyInt other)                  -> other == kind
   (_, TyInvalid)                    -> True
   (TyNumber, TyCon qualified)       -> qualified.name `elem` numberTypeNames
   (TyConcat, TyCon qualified)       -> qualified.name == stringTypeName
   (TyConcat, TyApp (TyCon list) _)  -> list.name == listTypeName
   _                                 -> False

-- | Reports constrained variables that never got a concrete type.
--   Run after type checking.
reportAmbiguousConstraints :: SemAnalyzer ()
reportAmbiguousConstraints = do
   constraints <- gets (M.toList . (.tyVarConstraints))
   unresolved <- filterM (isUnresolvedVar . fst) constraints
   -- one error per expression, even if it introduced several variables
   let byOrigin = unresolved
         |> map snd
         |> nubBy ((==) `on` (.origin))
   forM_ byOrigin $ \constraint ->
      errSem SEAmbiguousConstraint
         { location   = constraint.origin
         , constraint = constraint.kind
         }

-- | Whether a type variable is unbound.
isUnresolvedVar :: Int -> SemAnalyzer Bool
isUnresolvedVar varId = do
   resolved <- resolve (TyVar varId)
   pure (resolved == TyVar varId)

occursCheckType :: Int -> Type -> SemAnalyzer Bool
occursCheckType varId t = do
   resolvedType <- resolve t
   case resolvedType of
      TyVar otherId    -> pure (varId == otherId)
      TyApp func arg   -> (||) <$> occursCheckType varId func <*> occursCheckType varId arg
      TyTuple elements -> or <$> mapM (occursCheckType varId) elements
      TyFn args ret    -> ((||) . or <$> mapM (occursCheckType varId) args) <*> occursCheckType varId ret
      _                -> pure False

-- | Instantiates a signature: one fresh variable per generic and per intrinsic
--   kind, so all @number@s in a signature are the same type.
instantiate
   :: Location  -- ^ the expression that uses the function
   -> FuncSig
   -> SemAnalyzer Type
instantiate location (FuncSig args ret) = do
   let generics = S.toList (foldMap collectGnrs args <> collectGnrs ret)
   fresh <- mapM (const freshTyVar) generics
   let substitution = M.fromList (zip generics fresh)

   let kinds = S.toList (foldMap collectIntrinsics args <> collectIntrinsics ret)
   constrained <- mapM (freshConstrainedVar location) kinds
   let intrinsicSubstitution = M.fromList (zip kinds constrained)

   let instantiateType type' = type'
         |> substGnr substitution
         |> substIntrinsic intrinsicSubstitution
   pure $ TyFn (map instantiateType args) (instantiateType ret)

collectIntrinsics :: Type -> S.Set TyIntrinsic
collectIntrinsics = \case
   TyInt kind       -> S.singleton kind
   TyApp func arg   -> collectIntrinsics func <> collectIntrinsics arg
   TyTuple elements -> foldMap collectIntrinsics elements
   TyFn args result -> foldMap collectIntrinsics args <> collectIntrinsics result
   _                -> S.empty

substIntrinsic :: M.Map TyIntrinsic Type -> Type -> Type
substIntrinsic substitution = \case
   TyInt kind       -> M.findWithDefault (TyInt kind) kind substitution
   TyApp func arg   -> TyApp (substIntrinsic substitution func) (substIntrinsic substitution arg)
   TyTuple elements -> TyTuple (map (substIntrinsic substitution) elements)
   TyFn args result -> TyFn (map (substIntrinsic substitution) args) (substIntrinsic substitution result)
   other            -> other

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
