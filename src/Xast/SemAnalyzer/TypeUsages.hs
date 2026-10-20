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
resolveTypeUsages (Program (ModuleDef _ moduleName _) imports stmts _) = do
   modify $ \symTable -> symTable { currentModule = moduleName }
   forM_ stmts (resolveStmtTypeUsages imports)

resolveStmtTypeUsages :: [Located ImportDef] -> Stmt Parsed -> SemAnalyzer ()
resolveStmtTypeUsages imports stmt =
   let checkLocatedType (Located loc typeOfNode) = checkType imports loc typeOfNode
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
         forM_ entities $ \(QueriedEntity entityTypes) -> forM_ entityTypes checkLocatedType
         checkLocatedType retType
         forM_ (fromMaybe [] with) (checkLocatedType . withTypeLoc)

      StmtSystem (SysImpl _) -> pure ()

payloadTypes :: Payload -> [Located Type]
payloadTypes = \case
   PUnit          -> []
   PTuple types   -> types
   PRecord fields -> map (.ty) fields

withTypeLoc :: WithType -> Located Type
withTypeLoc (WithEvent locatedType) = locatedType
withTypeLoc (WithRes locatedType)   = locatedType

-- | Recursively resolves every `TyCon` occurring in a type against the
-- current module's declarations and its unqualified imports.
checkType :: [Located ImportDef] -> Location -> Type -> SemAnalyzer ()
checkType imports loc = \case
   TyCon ident -> do
      currentModuleSymbol <- lookupCurrentTypeName ident
      importedSymbol <- lookupUnqualifiedTypeName imports ident
      case currentModuleSymbol <|> importedSymbol of
         Just _  -> pure ()
         Nothing -> errSem (SEUndefinedType loc ident)
   TyApp func arg       -> checkType imports loc func >> checkType imports loc arg
   TyTuple elementTypes -> forM_ elementTypes (checkType imports loc)
   TyFn args ret        -> forM_ args (checkType imports loc) >> checkType imports loc ret
   TyGnr _              -> pure ()
   TyVar _              -> pure ()
   TyInt _              -> pure ()
   TyInvalid            -> pure ()
