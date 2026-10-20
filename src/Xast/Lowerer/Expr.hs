module Xast.Lowerer.Expr where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, freshKirName)
import Xast.Lowerer.Types
import Xast.Lowerer.Mono (monoType, mangleCtor, ctorResultTy)
import Xast.Utils.Generic (todo__)
import Data.Maybe (fromJust)
import qualified Data.Map as M

lowerExpr :: M.Map LocalId KirValue -> Expr Desugared -> Lowerer ([KirInstruct], KirValue)
lowerExpr env expr = case expr of
   ExpLit _ lit -> pure ([], KirConst (TagLit lit))

   ExpCon info _ ident ->
      case info.res of
         Just (ResConstructor _) -> pure ([], KirConst (TagCtor (mangleCtor (ctorResultTy info.ty) ident)))
         _ -> todo__ ("unresolved constructor: " ++ show ident)

   ExpTuple _ [single] -> lowerExpr env single
   ExpTuple {} -> todo__ "tuples with more than one element are not supported by the lowerer yet"

   ExpVar info _ ident ->
      case info.res of
         Just (ResLocal localId) ->
            case M.lookup localId env of
               Just value -> pure ([], value)
               Nothing -> todo__ ("unbound local variable " ++ show ident ++ " in lowering")
         _ -> todo__ ("only resolved local variable references are supported in the lowerer yet, got " ++ show ident)

   ExpApp {} -> do
      let spine (ExpApp (DesugaredInfo exprType _) func arg) accumulator Nothing = spine func (arg : accumulator) (Just exprType)
          spine (ExpApp _ func arg) accumulator (Just inferred) = spine func (arg : accumulator) (Just inferred)
          spine (ExpVar _ _ ident) accumulator accumulatedType = (fromJust accumulatedType, ident, accumulator)
          spine _ _ _ = todo__ "only direct function application is supported by the lowerer yet"
          (retType, functionIdent, args) = spine expr [] Nothing

      lowered <- mapM (lowerExpr env) args
      resultName <- freshKirName
      retType' <- monoType retType
      let instrs = concatMap fst lowered ++ [KirCall retType' (KirName functionIdent.inner) (map snd lowered) resultName]
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
            Just (ResLocal localId) -> do
               (restInstrs, env') <- lowerLetBindings (M.insert localId val env) rest
               pure (valInstrs ++ restInstrs, env')
            _ -> todo__ ("unresolved let binding " ++ show ident)

      PatWildcard _ -> do
         (restInstrs, env') <- lowerLetBindings env rest
         pure (valInstrs ++ restInstrs, env')

      _ -> todo__ "only variable and wildcard patterns are supported in let bindings yet"
