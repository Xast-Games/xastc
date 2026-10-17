{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.TypeUsages where

import Control.Monad.State
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query
import Control.Applicative ((<|>))

-- #### RESOLVE TYPE USAGES ####

resolveTypeUsages :: Program Parsed -> SemAnalyzer ()
resolveTypeUsages (Program (ModuleDef _ m _) imps stmts _) = do
   modify $ \st -> st { currentModule = m }
   forM_ stmts (resolveStmtTypeUsages imps)

resolveStmtTypeUsages :: [Located ImportDef] -> Stmt Parsed -> SemAnalyzer ()
resolveStmtTypeUsages imps stmt =
   let checkLocatedType (Located loc ty) = checkType imps loc ty
   in case stmt of
      StmtTypeDef (TypeDef _ _ _ _ ctors) ->
         forM_ ctors $ \ctor ->
            forM_ (payloadTypes ctor.payload) checkLocatedType

      StmtFunc (FnDef (FuncDef _ _ _ args retType)) ->
         forM_ (retType : args) checkLocatedType

      StmtFunc (FnImpl _) -> pure ()

      StmtExtern (ExtFunc (ExternFunc _ _ _ args retType)) ->
         forM_ (retType : args) checkLocatedType

      StmtExtern (ExtType _) -> pure ()

      StmtSystem (SysDef (SystemDef _ _ _ entities retType with)) -> do
         forM_ entities $ \(QueriedEntity tys) -> forM_ tys checkLocatedType
         checkLocatedType retType
         forM_ (fromMaybe [] with) (checkLocatedType . withTypeLoc)

      StmtSystem (SysImpl _) -> pure ()

payloadTypes :: Payload -> [Located Type]
payloadTypes = \case
   PUnit        -> []
   PTuple tys   -> tys
   PRecord fields -> map (.ty) fields

withTypeLoc :: WithType -> Located Type
withTypeLoc (WithEvent lt) = lt
withTypeLoc (WithRes lt)   = lt

-- | Recursively resolves every `TyCon` occurring in a type against the
-- current module's declarations and its unqualified imports.
checkType :: [Located ImportDef] -> Location -> Type -> SemAnalyzer ()
checkType imps loc = \case
   TyCon ident -> do
      modSym <- lookupCurrentTypeName ident
      impSym <- lookupUnqualifiedTypeName imps ident
      case modSym <|> impSym of
         Just _  -> pure ()
         Nothing -> errSem (SEUndefinedType loc ident)
   TyApp a b     -> checkType imps loc a >> checkType imps loc b
   TyTuple xs    -> forM_ xs (checkType imps loc)
   TyFn args ret -> forM_ args (checkType imps loc) >> checkType imps loc ret
   TyGnr _       -> pure ()
   TyVar _       -> pure ()
   TyInt _       -> pure ()
   TyInvalid     -> pure ()
