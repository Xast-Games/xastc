{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.TypeUsages where

import Control.Monad.State
import Control.Monad (forM, forM_)
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query
import qualified Data.Map as M
import Xast.Utils.Generic ((|>))

-- #### RESOLVE TYPE USAGES ####

-- | Fills in the declaring module of every type written in the program and in
--   the signatures of its symbols, which were declared before imports were known.
resolveTypeUsages :: Program Parsed -> SemAnalyzer (Program Parsed)
resolveTypeUsages (Program moduleDef imports stmts source) = do
   modify $ \symTable -> symTable { currentModule = moduleDef.name }
   qualifiedStmts <- forM stmts (qualifyStmt imports)
   refreshSignatures qualifiedStmts
   pure (Program moduleDef imports qualifiedStmts source)

qualifyStmt :: [Located ImportDef] -> Stmt Parsed -> SemAnalyzer (Stmt Parsed)
qualifyStmt imports stmt =
   let qualify = qualifyLocated imports
   in case stmt of
      StmtTypeDef (TypeDef location modifiers name generics ctors) -> do
         qualifiedCtors <- forM ctors (qualifyCtor imports)
         pure $ StmtTypeDef (TypeDef location modifiers name generics qualifiedCtors)

      StmtFunc (FnDef (FuncDef location modifiers name args retType)) -> do
         qualifiedArgs <- forM args qualify
         qualifiedRetType <- qualify retType
         pure $ StmtFunc (FnDef (FuncDef location modifiers name qualifiedArgs qualifiedRetType))

      StmtExtern (ExtFunc (ExternFunc location modifiers name args retType)) -> do
         qualifiedArgs <- forM args qualify
         qualifiedRetType <- qualify retType
         pure $ StmtExtern (ExtFunc (ExternFunc location modifiers name qualifiedArgs qualifiedRetType))

      StmtSystem (SysDef (SystemDef location modifiers name entities retType with)) -> do
         qualifiedEntities <- forM entities $ \(QueriedEntity entityTypes) ->
            QueriedEntity <$> forM entityTypes qualify
         qualifiedRetType <- qualify retType
         qualifiedWith <- forM with (mapM (qualifyWithType imports))
         pure $ StmtSystem (SysDef (SystemDef location modifiers name qualifiedEntities qualifiedRetType qualifiedWith))

      other -> pure other

qualifyCtor :: [Located ImportDef] -> Ctor -> SemAnalyzer Ctor
qualifyCtor imports (Ctor location name payload) = do
   qualifiedPayload <- case payload of
      PUnit -> pure PUnit
      PTuple types -> PTuple <$> forM types (qualifyLocated imports)
      PRecord fields -> PRecord <$> forM fields (qualifyField imports)
   pure (Ctor location name qualifiedPayload)

qualifyField :: [Located ImportDef] -> Field -> SemAnalyzer Field
qualifyField imports (Field name fieldType) =
   Field name <$> qualifyLocated imports fieldType

qualifyWithType :: [Located ImportDef] -> WithType -> SemAnalyzer WithType
qualifyWithType imports = \case
   WithEvent eventType  -> WithEvent <$> qualifyLocated imports eventType
   WithRes resourceType -> WithRes <$> qualifyLocated imports resourceType

-- | Qualifies a type written in the source, reporting the names that are not in scope.
qualifyLocated :: [Located ImportDef] -> Located Type -> SemAnalyzer (Located Type)
qualifyLocated imports (Located location writtenType) = do
   let reportUnknown name = errSem (SEUndefinedType location name)
   qualifiedType <- qualifyType imports reportUnknown writtenType
   pure (Located location qualifiedType)

-- | Fills in the declaring module of every named type in a type.
qualifyType
   :: [Located ImportDef]
   -> (TypeName -> SemAnalyzer ())  -- ^ what to do with a name that is not in scope
   -> Type
   -> SemAnalyzer Type
qualifyType imports reportUnknown type' =
   let qualifyInner = qualifyType imports reportUnknown
   in case type' of
      TyCon (QualifiedIdent (Just _) _) -> pure type'
      TyCon (QualifiedIdent Nothing name) -> do
         declaringModule <- findTypeModule imports name
         case declaringModule of
            Just module' -> pure (typeIn module' name)
            Nothing      -> reportUnknown name >> pure type'
      TyApp func arg       -> TyApp <$> qualifyInner func <*> qualifyInner arg
      TyTuple elementTypes -> TyTuple <$> mapM qualifyInner elementTypes
      TyFn args ret        -> TyFn <$> mapM qualifyInner args <*> qualifyInner ret
      TyGnr _              -> pure type'
      TyVar _              -> pure type'
      TyInt _              -> pure type'
      TyInvalid            -> pure type'

-- | Rebuilds the signatures of the current module's symbols from the qualified
--   declarations.
refreshSignatures :: [Stmt Parsed] -> SemAnalyzer ()
refreshSignatures stmts = forM_ stmts $ \case
   StmtFunc (FnDef funcDef) -> updateSymbol funcDef.name $ \case
      SymbolFn location functionId _ -> SymbolFn location functionId (funcSig funcDef)
      other                          -> other

   StmtExtern (ExtFunc externFunc) -> updateSymbol externFunc.name $ \case
      SymbolExternFn location externId _ -> SymbolExternFn location externId (externFuncSig externFunc)
      other                              -> other

   StmtSystem (SysDef systemDef) -> updateSymbol systemDef.name $ \case
      SymbolSystem location _ -> SymbolSystem location (systemSig systemDef)
      other                   -> other

   StmtTypeDef typeDef -> forM_ typeDef.ctors $ \ctor ->
      updateSymbol ctor.name (withCtorFieldTypes ctor)

   _ -> pure ()

-- | Replaces the field types in a constructor's signature with the ones of its declaration.
withCtorFieldTypes :: Ctor -> SymbolInfo -> SymbolInfo
withCtorFieldTypes ctor symbol =
   let fieldTypes = payloadTypes ctor.payload |> map (.node)
       withFields (CtorSig owner generics fieldNames _) = CtorSig owner generics fieldNames fieldTypes
   in case symbol of
      SymbolCtor location constructorId signature ->
         SymbolCtor location constructorId (withFields signature)
      SymbolTypeCtor location typeSignature constructorId signature ->
         SymbolTypeCtor location typeSignature constructorId (withFields signature)
      other -> other

-- | Changes a symbol of the current module in place.
updateSymbol :: Ident -> (SymbolInfo -> SymbolInfo) -> SemAnalyzer ()
updateSymbol ident change = modify $ \symTable ->
   let changeModule moduleInfo = moduleInfo { symbols = M.adjust change ident moduleInfo.symbols }
   in symTable { modules = M.adjust changeModule symTable.currentModule symTable.modules }

payloadTypes :: Payload -> [Located Type]
payloadTypes = \case
   PUnit          -> []
   PTuple types   -> types
   PRecord fields -> map (.ty) fields
