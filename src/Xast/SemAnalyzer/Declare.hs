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
enterModule (ModuleDef _ m _) = do
   st <- get

   case M.lookup m st.modules of
      Just _ -> pure ()
      Nothing ->
         put st
            { modules = M.insert m emptyModuleInfo st.modules
            }

declareStmts :: Program Parsed -> SemAnalyzer ()
declareStmts (Program md@(ModuleDef _ m _) _ stmts _) = do
   enterModule md
   modify $ \st -> st { currentModule = m }
   forM_ stmts declareStmt

declareStmt :: Stmt Parsed -> SemAnalyzer ()
declareStmt = \case
   StmtFunc (FnDef fd@(FuncDef _ _ ident _ _)) ->
      declareFn ident fd

   StmtTypeDef td@(TypeDef _ _ ident _ _) ->
      declareType ident td

   StmtExtern (ExtFunc ef@(ExternFunc _ _ ident _ _)) ->
      declareExternFn ident ef

   StmtExtern (ExtType et@(ExternType _ ident _)) ->
      declareExternType ident et

   StmtSystem (SysDef sd@(SystemDef _ _ ident _ _ _)) ->
      declareSystem ident sd

   _ -> return ()

type RedeclarationError = (Ident -> Location -> Location -> SemError)

declareSymbol :: Ident -> SymbolInfo -> RedeclarationError -> SemAnalyzer ()
declareSymbol ident sym re = do
   QualifiedName m _ <- qualify ident
   st <- get
   let mi = M.findWithDefault emptyModuleInfo m st.modules

   case M.lookup ident mi.symbols of
      Just old ->
         errSem (re ident (symbolLoc old) (symbolLoc sym))

      Nothing ->
         put st
            { modules =
                  M.insert m
                     mi { symbols = M.insert ident sym mi.symbols }
                     st.modules
            }

declareFn :: Ident -> FuncDef -> SemAnalyzer ()
declareFn ident fd@(FuncDef loc _ fnIdent fnArgs _) = do
   -- Report function gayness
   let args = length fnArgs
   when (args > 6) $
      warnSem (SWFunctionGayness loc fnIdent args)

   fid <- freshFunctionId
   declareSymbol ident (SymbolFn loc fid (funcSig fd)) SEFnRedeclaration

declareType :: Ident -> TypeDef -> SemAnalyzer ()
declareType ident (TypeDef loc modifiers _ generics ctors) = do
   let ctorNames = S.fromList [ctor.name | ctor <- ctors]
       typeSig = TypeSig ctorNames generics (TypeMod ModNonExhaustive `elem` modifiers)

   case [ctor | ctor <- ctors, ctor.name == ident] of
      [selfCtor] -> do
         let (fieldNames, fieldTys) = payloadFields selfCtor.payload
         cid <- freshConstructorId
         declareSymbol ident
               (SymbolTypeCtor loc typeSig cid (CtorSig ident generics fieldNames fieldTys))
               SETypeRedeclaration
      _ ->
         declareSymbol ident (SymbolType loc typeSig) SETypeRedeclaration

   forM_ ctors $ \ctor ->
      unless (ctor.name == ident) $ do
         let (fieldNames, fieldTys) = payloadFields ctor.payload
         cid <- freshConstructorId
         declareSymbol ctor.name
               (SymbolCtor ctor.location cid (CtorSig ident generics fieldNames fieldTys))
               SECtorRedeclaration
   where
      payloadFields = \case
         PUnit -> (Nothing, [])
         (PTuple tys) -> (Nothing, map (.node) tys)
         (PRecord fs) -> (Just (map (.name) fs), map ((.node) . (.ty)) fs)

declareExternFn :: Ident -> ExternFunc -> SemAnalyzer ()
declareExternFn ident ef@(ExternFunc loc _ _ _ _) = do
   eid <- freshExternId
   declareSymbol ident (SymbolExternFn loc eid (externFuncSig ef)) SEExternFnRedeclaration

declareExternType :: Ident -> ExternType -> SemAnalyzer ()
declareExternType ident (ExternType loc _ _) =
   declareSymbol ident (SymbolExternType loc) SEExternTypeRedeclaration

declareSystem :: Ident -> SystemDef -> SemAnalyzer ()
declareSystem ident sd@(SystemDef loc _ _ _ _ _) =
   declareSymbol ident (SymbolSystem loc (systemSig sd)) SESystemRedeclaration
