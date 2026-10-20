{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Declare where

import Control.Monad.State
import Control.Monad (forM_, unless, when)
import qualified Data.Set as S
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Unify (freshConstructorId, freshExternId, freshFunctionId)

-- #### DECLARE STATEMENTS ####

qualify :: Ident -> SemAnalyzer QualifiedName
qualify ident = do
   module_ <- gets (.currentModule)
   return (QualifiedName module_ ident)

enterModule :: ModuleDef -> SemAnalyzer ()
enterModule (ModuleDef _ moduleName _) = do
   symTable <- get

   case M.lookup moduleName symTable.modules of
      Just _ -> pure ()
      Nothing ->
         put symTable
            { modules = M.insert moduleName emptyModuleInfo symTable.modules
            }

declareStmts :: Program Parsed -> SemAnalyzer ()
declareStmts (Program moduleDef@(ModuleDef _ moduleName _) _ stmts _) = do
   enterModule moduleDef
   modify $ \symTable -> symTable { currentModule = moduleName }
   forM_ stmts declareStmt

declareStmt :: Stmt Parsed -> SemAnalyzer ()
declareStmt = \case
   StmtFunc (FnDef funcDef@(FuncDef _ _ ident _ _)) ->
      declareFn ident funcDef

   StmtTypeDef typeDef@(TypeDef _ _ ident _ _) ->
      declareType ident typeDef

   StmtExtern (ExtFunc externFunc@(ExternFunc _ _ ident _ _)) ->
      declareExternFn ident externFunc

   StmtExtern (ExtType externType@(ExternType _ ident _)) ->
      declareExternType ident externType

   StmtSystem (SysDef systemDef@(SystemDef _ _ ident _ _ _)) ->
      declareSystem ident systemDef

   _ -> return ()

type RedeclarationError = (Ident -> Location -> Location -> SemError)

declareSymbol :: Ident -> SymbolInfo -> RedeclarationError -> SemAnalyzer ()
declareSymbol ident symbol redeclarationError = do
   QualifiedName moduleName _ <- qualify ident
   symTable <- get
   let moduleInfo = M.findWithDefault emptyModuleInfo moduleName symTable.modules

   case M.lookup ident moduleInfo.symbols of
      Just oldSymbol ->
         errSem (redeclarationError ident (symbolLoc oldSymbol) (symbolLoc symbol))

      Nothing ->
         put symTable
            { modules =
                  M.insert moduleName
                     moduleInfo { symbols = M.insert ident symbol moduleInfo.symbols }
                     symTable.modules
            }

declareFn :: Ident -> FuncDef -> SemAnalyzer ()
declareFn ident funcDef@(FuncDef loc _ fnIdent fnArgs _) = do
   -- Report function gayness
   let argsCount = length fnArgs
   when (argsCount > 6) $
      warnSem (SWFunctionGayness loc fnIdent argsCount)

   functionId <- freshFunctionId
   declareSymbol ident (SymbolFn loc functionId (funcSig funcDef)) SEFnRedeclaration

declareType :: Ident -> TypeDef -> SemAnalyzer ()
declareType ident (TypeDef loc modifiers _ generics ctors) = do
   let ctorNames = S.fromList [ctor.name | ctor <- ctors]
       typeSig = TypeSig ctorNames generics (TypeMod ModNonExhaustive `elem` modifiers)

   case [ctor | ctor <- ctors, ctor.name == ident] of
      [selfCtor] -> do
         let (fieldNames, fieldTypes) = payloadFields selfCtor.payload
         constructorId <- freshConstructorId
         declareSymbol ident
               (SymbolTypeCtor loc typeSig constructorId (CtorSig ident generics fieldNames fieldTypes))
               SETypeRedeclaration
      _ ->
         declareSymbol ident (SymbolType loc typeSig) SETypeRedeclaration

   forM_ ctors $ \ctor ->
      unless (ctor.name == ident) $ do
         let (fieldNames, fieldTypes) = payloadFields ctor.payload
         constructorId <- freshConstructorId
         declareSymbol ctor.name
               (SymbolCtor ctor.location constructorId (CtorSig ident generics fieldNames fieldTypes))
               SECtorRedeclaration
   where
      payloadFields = \case
         PUnit -> (Nothing, [])
         (PTuple types) -> (Nothing, map (.node) types)
         (PRecord fields) -> (Just (map (.name) fields), map ((.node) . (.ty)) fields)

declareExternFn :: Ident -> ExternFunc -> SemAnalyzer ()
declareExternFn ident externFunc@(ExternFunc loc _ _ _ _) = do
   externId <- freshExternId
   declareSymbol ident (SymbolExternFn loc externId (externFuncSig externFunc)) SEExternFnRedeclaration

declareExternType :: Ident -> ExternType -> SemAnalyzer ()
declareExternType ident (ExternType loc _ _) =
   declareSymbol ident (SymbolExternType loc) SEExternTypeRedeclaration

declareSystem :: Ident -> SystemDef -> SemAnalyzer ()
declareSystem ident systemDef@(SystemDef loc _ _ _ _ _) =
   declareSymbol ident (SymbolSystem loc (systemSig systemDef)) SESystemRedeclaration
