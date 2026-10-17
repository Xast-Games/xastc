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
   st <- get
   let n = st.tyVarSupply
   put st { tyVarSupply = n + 1 }
   return $ TyVar n

freeTyVars :: Type -> S.Set Int
freeTyVars ty = go [ty] S.empty
   where
      go :: [Type] -> S.Set Int -> S.Set Int
      go [] acc = acc
      go (t:rest) acc = case t of
         TyVar n        -> go rest (S.insert n acc)
         TyApp a b      -> go (a : b : rest) acc
         TyTuple xs     -> go (xs ++ rest) acc
         TyFn args ret  -> go (ret : args ++ rest) acc
         _ -> go rest acc

generalize :: Type -> SemAnalyzer [Int]
generalize ty = do
   ty'  <- resolve ty
   vars <- asks (.vars)
   envFree <- fmap S.unions . forM (M.elems vars) $ \vi -> do
      t <- resolve vi.ty
      pure (freeTyVars t S.\\ S.fromList vi.quantified)

   return $ S.toList (freeTyVars ty' S.\\ envFree)

freshVarId :: SemAnalyzer VarId
freshVarId = do
   st <- get
   let n = st.varIdSupply
   put st { varIdSupply = n + 1 }
   return $ VarId n

freshLocalId :: SemAnalyzer LocalId
freshLocalId = do
   st <- get
   let n = st.localIdSupply
   put st { localIdSupply = n + 1 }
   return $ LocalId n

freshFunctionId :: SemAnalyzer FunctionId
freshFunctionId = do
   st <- get
   let n = st.fnIdSupply
   put st { fnIdSupply = n + 1 }
   return $ FunctionId n

freshConstructorId :: SemAnalyzer ConstructorId
freshConstructorId = do
   st <- get
   let n = st.ctorIdSupply
   put st { ctorIdSupply = n + 1 }
   return $ ConstructorId n

freshExternId :: SemAnalyzer ExternId
freshExternId = do
   st <- get
   let n = st.externIdSupply
   put st { externIdSupply = n + 1 }
   return $ ExternId n

-- | Assigns a fresh LocalId to every name, e.g. for a pattern's bound vars.
freshLocalScope :: [(Ident, Location)] -> SemAnalyzer (M.Map Ident (LocalId, Location))
freshLocalScope xs = M.fromList <$> mapM (\(x, loc) -> (\lid -> (x, (lid, loc))) <$> freshLocalId) xs

resolve :: Type -> SemAnalyzer Type
resolve t = do
   s <- gets (.tySubst)
   pure (go s t)
   where
      go s (TyVar n) = maybe (TyVar n) (go s) (M.lookup n s)
      go s (TyApp a b) = TyApp (go s a) (go s b)
      go s (TyTuple xs) = TyTuple (map (go s) xs)
      go s (TyFn args r) = TyFn (map (go s) args) (go s r)
      go _ t' = t'

bindVar :: Int -> Type -> SemAnalyzer ()
bindVar n t = modify $ \st -> st { tySubst = M.insert n t st.tySubst }

zonkType :: M.Map Int Type -> Type -> Type
zonkType s = go
   where
      go (TyVar n) = maybe (TyVar n) go (M.lookup n s)
      go (TyApp a b) = TyApp (go a) (go b)
      go (TyTuple xs) = TyTuple (map go xs)
      go (TyFn args r) = TyFn (map go args) (go r)
      go t = t

zonkTyped :: M.Map Int Type -> Typed -> Typed
zonkTyped s (TypedInfo loc ty res) = TypedInfo loc (zonkType s ty) res

zonkProgram :: M.Map Int Type -> Program Typed -> Program Typed
zonkProgram s = fmap (zonkTyped s)

unify :: Location -> Type -> Type -> SemAnalyzer ()
unify loc t1 t2 = do
   t1' <- resolve t1
   t2' <- resolve t2
   case (t1', t2') of
      (TyVar n, TyVar m) | n == m -> pure ()
      (TyVar n, _) -> bindOrFail loc n t2'
      (_, TyVar m) -> bindOrFail loc m t1'

      (TyCon a, TyCon b) | a == b -> pure ()
      (TyGnr a, TyGnr b) | a == b -> pure ()

      (TyApp a1 b1, TyApp a2 b2) -> unify loc a1 a2 >> unify loc b1 b2
      (TyTuple xs, TyTuple ys) | length xs == length ys ->
         zipWithM_ (unify loc) xs ys

      (TyFn args1 r1, TyFn args2 r2) | length args1 == length args2 -> do
         zipWithM_ (unify loc) args1 args2
         unify loc r1 r2

      (TyInvalid, _) -> pure ()
      (_, TyInvalid) -> pure ()

      _ -> errSem (SETypeError loc t1' t2')

bindOrFail :: Location -> Int -> Type -> SemAnalyzer ()
bindOrFail loc n t = do
   occ <- occursCheckType n t
   if occ then errSem (SEInfiniteType loc n t) else bindVar n t

occursCheckType :: Int -> Type -> SemAnalyzer Bool
occursCheckType n t = do
   t' <- resolve t
   case t' of
      TyVar m       -> pure (n == m)
      TyApp a b     -> (||) <$> occursCheckType n a <*> occursCheckType n b
      TyTuple xs    -> or <$> mapM (occursCheckType n) xs
      TyFn args ret -> ((||) . or <$> mapM (occursCheckType n) args) <*> occursCheckType n ret
      _             -> pure False

instantiate :: FuncSig -> SemAnalyzer Type
instantiate (FuncSig args ret) = do
   let gnrs = S.toList (foldMap collectGnrs args <> collectGnrs ret)
   fresh <- mapM (const freshTyVar) gnrs
   let subst = M.fromList (zip gnrs fresh)
   pure $ TyFn (map (substGnr subst) args) (substGnr subst ret)

collectGnrs :: Type -> S.Set Ident
collectGnrs = \case
   TyGnr i       -> S.singleton i
   TyApp a b     -> collectGnrs a <> collectGnrs b
   TyTuple xs    -> foldMap collectGnrs xs
   TyFn args r   -> foldMap collectGnrs args <> collectGnrs r
   _             -> S.empty

substGnr :: M.Map Ident Type -> Type -> Type
substGnr m = \case
   TyGnr i       -> M.findWithDefault (TyGnr i) i m
   TyApp a b     -> TyApp (substGnr m a) (substGnr m b)
   TyTuple xs    -> TyTuple (map (substGnr m) xs)
   TyFn args r   -> TyFn (map (substGnr m) args) (substGnr m r)
   t             -> t

substTyVars :: M.Map Int Type -> Type -> Type
substTyVars m = \case
   TyVar i       -> M.findWithDefault (TyVar i) i m
   TyApp a b     -> TyApp (substTyVars m a) (substTyVars m b)
   TyTuple xs    -> TyTuple (map (substTyVars m) xs)
   TyFn args r   -> TyFn (map (substTyVars m) args) (substTyVars m r)
   t             -> t

typeOf :: Expr Typed -> Type
typeOf = (.ty) . exprAnnotation
