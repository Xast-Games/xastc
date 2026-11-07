{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Expr where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, freshKirName, freshKirLocalId)
import {-# SOURCE #-} Xast.Lowerer.Match (lowerMatchTo)
import Xast.Lowerer.Types
import Xast.Lowerer.Mono (monoType, mangleCtor, ctorResultTy, requestIntrinsic, requestFunction)
import Xast.Utils.Generic (todo__)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Control.Monad (when, unless)
import Control.Monad.State (gets)

lowerExpr :: M.Map LocalId KirValue -> Expr Desugared -> Lowerer ([KirInstruct], KirValue)
lowerExpr env expr = case expr of
   ExpLit _ lit -> pure ([], KirConst (TagLit lit))

   ExpCon info _ ident -> do
      resultType <- monoType info.ty
      lowerConstruct resultType info ident []

   ExpTuple _ [single] -> lowerExpr env single
   ExpTuple {} -> todo__ "tuples with more than one element are not supported by the lowerer yet"

   ExpVar info _ ident ->
      case info.res of
         Just (ResLocal localId) ->
            case M.lookup localId env of
               Just value -> pure ([], value)
               Nothing -> todo__ ("unbound local variable " ++ show ident ++ " in lowering")
         _ -> todo__ ("only resolved local variable references are supported in the lowerer yet, got " ++ show ident)

   ExpApp info _ _ -> do
      let (callee, args) = applicationSpine expr []
      lowered <- mapM (lowerExpr env) args
      let argInstrs = concatMap fst lowered
      let argValues = map snd lowered
      resultType <- monoType info.ty

      (instrs, value) <- case callee of
         ExpCon calleeInfo _ ident -> lowerConstruct resultType calleeInfo ident argValues
         ExpVar calleeInfo _ ident -> lowerCall resultType calleeInfo ident argValues
         _ -> todo__ "only direct function application is supported by the lowerer yet"
      pure (argInstrs ++ instrs, value)

   ExpMatch info match -> do
      resultType <- monoType info.ty
      result <- freshKirLocalId
      instrs <- lowerMatchTo env resultType (DestLocal result) match
      pure (instrs, KirLocalRef result)

   ExpLetIn _ letIn -> do
      (bindInstrs, env') <- lowerLetBindings env letIn.bindings
      (bodyInstrs, bodyVal) <- lowerExpr env' letIn.bindExpr
      pure (bindInstrs ++ bodyInstrs, bodyVal)

   unsupportedExpr -> todo__ $ 
      "this expression form is not supported by the lowerer yet:\n" ++
      show unsupportedExpr

-- | The head of an application and its arguments: @f a b@ is @(f, [a, b])@.
applicationSpine :: Expr Desugared -> [Expr Desugared] -> (Expr Desugared, [Expr Desugared])
applicationSpine expr accumulator = case expr of
   ExpApp _ func arg -> applicationSpine func (arg : accumulator)
   callee            -> (callee, accumulator)

-- | A constructor applied to all its fields.
lowerConstruct
   :: Type          -- ^ the monomorphic type built
   -> Desugared     -- ^ of the constructor
   -> CtorName
   -> [KirValue]    -- ^ the fields
   -> Lowerer ([KirInstruct], KirValue)
lowerConstruct builtType info name fields = do
   when (isFunctionType builtType) $
      todo__ ("partially applied constructor `" ++ show name ++ "` is not supported by the lowerer yet")
   unless (isJust info.res) $
      todo__ ("unresolved constructor: " ++ show name)
   dst <- freshKirName
   let construct = KirConstruct
         { ty   = builtType
         , ctor = mangleCtor (ctorResultTy info.ty) name
         , args = fields
         , dst  = dst
         }
   pure ([construct], KirVar dst)

-- | A function call, or an intrinsic call.
lowerCall
   :: Type          -- ^ the monomorphic result type
   -> Desugared     -- ^ of the callee
   -> FunctionName
   -> [KirValue]    -- ^ the arguments
   -> Lowerer ([KirInstruct], KirValue)
lowerCall resultType info name args = do
   resultName <- freshKirName
   intrinsic <- lookupIntrinsic info.res
   callee <- case intrinsic of
      Just decl -> intrinsicCallee decl info.ty resultType
      Nothing   -> functionCallee info.res name info.ty
   let call = KirCall resultType callee args (Just resultName)
   pure ([call], KirVar resultName)

-- | The name of a called function: prefixed with its declaring module, like
--   its definition, and for a generic function, with its types here.
--   Extern functions keep their own name.
functionCallee
   :: Maybe Resolution
   -> FunctionName
   -> Type  -- ^ callee type here, e.g. @fn(Option Int) -> Bool@
   -> Lowerer KirName
functionCallee resolution name calleeType = case resolution of
   Just (ResFunction functionId) -> gets (M.lookup functionId . (.functionModules)) >>= \case
      Just module' ->
         let qualified = QualifiedIdent { moduleName = Just module', name = name }
         in requestFunction qualified calleeType
      Nothing -> todo__ ("no module for function " ++ show name)
   _ -> pure (KirName name.inner)

-- | The callee's @Intrinsic declaration, if it has one.
lookupIntrinsic :: Maybe Resolution -> Lowerer (Maybe IntrinsicDecl)
lookupIntrinsic = \case
   Just (ResExternFunction externId) -> gets (M.lookup externId . (.intrinsics))
   _                                 -> pure Nothing

-- | The instance name for a fully applied intrinsic call.
intrinsicCallee
   :: IntrinsicDecl
   -> Type           -- ^ callee type here, e.g. @fn(Int, Int) -> Int@
   -> Type           -- ^ monomorphic result type
   -> Lowerer KirName
intrinsicCallee decl calleeType resultType = do
   when (isFunctionType resultType) $
      todo__ ("partially applied intrinsic `" ++ show decl.name.name ++ "` is not supported by the lowerer yet")
   requestIntrinsic decl calleeType

isFunctionType :: Type -> Bool
isFunctionType (TyFn _ _) = True
isFunctionType _          = False

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
            Just (ResLocal localId) -> do
               (restInstrs, env') <- lowerLetBindings (M.insert localId val env) rest
               pure (valInstrs ++ restInstrs, env')
            _ -> todo__ ("unresolved let binding " ++ show ident)

      PatWildcard _ -> do
         (restInstrs, env') <- lowerLetBindings env rest
         pure (valInstrs ++ restInstrs, env')

      _ -> todo__ "only variable and wildcard patterns are supported in let bindings yet"
