{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Query where

import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Monad (SemAnalyzer)
import Xast.AST
import Control.Monad.Reader (MonadReader(local), asks)
import qualified Data.Map as M
import Control.Monad.State (gets)
import qualified Data.Set as S
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Xast.Utils.Generic ((|>))

lookupLocal :: Ident -> SemAnalyzer (Maybe VarInfo)
lookupLocal ident = asks (M.lookup ident . (.vars))

withVars :: M.Map Ident VarInfo -> SemAnalyzer a -> SemAnalyzer a
withVars newVars =
   local (\env -> env { vars = M.union newVars env.vars })

-- | Look up a symbol by name in a specific module's symbol table
lookupInModule :: Module -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupInModule moduleName ident = gets $ \symTable ->
   M.lookup moduleName symTable.modules >>= \moduleInfo -> M.lookup ident moduleInfo.symbols

-- | Where an exported name is declared.
exportOrigin :: M.Map Module ModuleInfo -> Module -> Ident -> Maybe Module
exportOrigin modules module' ident =
   M.lookup module' modules >>= \moduleInfo -> M.lookup ident moduleInfo.exports

-- | Where a name is declared: its re-export origin, or the module itself.
declaringModuleThrough :: M.Map Module ModuleInfo -> Module -> Ident -> Module
declaringModuleThrough modules module' ident =
   fromMaybe module' (exportOrigin modules module' ident)

-- | An exported symbol, looked up in its declaring module.
lookupExported :: Module -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupExported module' ident = do
   modules <- gets (.modules)
   case exportOrigin modules module' ident of
      Just origin -> lookupInModule origin ident
      Nothing     -> pure Nothing

-- | A symbol a module declares or re-exports.
lookupThrough :: Module -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupThrough module' ident = do
   declared <- lookupInModule module' ident
   exported <- lookupExported module' ident
   pure (declared <|> exported)

-- | Look up a symbol in the current module
lookupCurrentModule :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentModule ident = do
   moduleName <- gets (.currentModule)
   lookupInModule moduleName ident

-- | Look up a symbol brought in by unqualified imports (ImpFull or ImpSelect)
lookupUnqualifiedSymbol :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedSymbol imports ident = do
   modules <- gets (.modules)
   let go [] = pure Nothing
       go (Located _ (ImportDef moduleName payload) : rest) =
         case payload of
            ImpAlias _ -> go rest
            ImpSelect identifiers ->
               if any ((== ident) . (.node)) identifiers
                  then lookupThrough moduleName ident >>= \case
                     Just symbol -> pure (Just symbol)
                     Nothing     -> go rest
                  else go rest
            ImpFull ->
               case M.lookup moduleName modules of
                  Nothing -> go rest
                  Just moduleInfo ->
                     if M.member ident moduleInfo.exports
                        then lookupExported moduleName ident
                        else go rest
   go imports

-- | Look up a symbol via a module alias: alias.ident
lookupQualifiedSymbol :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedSymbol imports alias ident = do
   let aliased = [ moduleName | Located _ (ImportDef moduleName (ImpAlias (Located _ importAlias))) <- imports, importAlias == alias ]
   case aliased of
      []             -> pure Nothing
      (moduleName:_) -> lookupThrough moduleName ident

-- | Normalizes a looked-up symbol into constructor shape, unwrapping the merged
-- `SymbolTypeCtor` entry used when a type shares its name with its sole constructor.
asConstructor :: SymbolInfo -> Maybe SymbolInfo
asConstructor symbol@(SymbolCtor {}) = Just symbol
asConstructor (SymbolTypeCtor loc _ constructorId ctorSignature) = Just (SymbolCtor loc constructorId ctorSignature)
asConstructor _ = Nothing

-- | Normalizes a looked-up symbol into type shape, unwrapping `SymbolTypeCtor`.
asType :: SymbolInfo -> Maybe SymbolInfo
asType symbol@(SymbolType {}) = Just symbol
asType (SymbolTypeCtor loc typeSignature _ _) = Just (SymbolType loc typeSignature)
asType _ = Nothing

lookupCurrentConstructor :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentConstructor ident = do
   symbol <- lookupCurrentModule ident
   pure (symbol >>= asConstructor)

lookupUnqualifiedConstructor :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedConstructor imports ident = do
   symbol <- lookupUnqualifiedSymbol imports ident
   pure (symbol >>= asConstructor)

lookupQualifiedConstructor :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedConstructor imports alias ident = do
   symbol <- lookupQualifiedSymbol imports alias ident
   pure (symbol >>= asConstructor)

lookupCurrentConType :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentConType ident = do
   symbol <- lookupCurrentModule ident
   pure $ case symbol >>= asType of
      Just typeSymbol@(SymbolType _ (TypeSig ctors _ _)) | S.member ident ctors -> Just typeSymbol
      _ -> Nothing

lookupUnqualifiedConType :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedConType imports ident = do
   symbol <- lookupUnqualifiedSymbol imports ident
   pure $ case symbol >>= asType of
      Just typeSymbol@(SymbolType _ (TypeSig ctors _ _)) | S.member ident ctors -> Just typeSymbol
      _ -> Nothing

lookupQualifiedConType :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedConType imports alias ident = do
   symbol <- lookupQualifiedSymbol imports alias ident
   pure $ case symbol >>= asType of
      Just typeSymbol@(SymbolType _ (TypeSig ctors _ _)) | S.member ident ctors -> Just typeSymbol
      _ -> Nothing

isTypeSymbol :: SymbolInfo -> Bool
isTypeSymbol = \case
   SymbolType {}       -> True
   SymbolTypeCtor {}   -> True
   SymbolExternType {} -> True
   _                   -> False

lookupCurrentTypeName :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentTypeName ident = do
   symbol <- lookupCurrentModule ident
   pure (symbol >>= \candidate -> if isTypeSymbol candidate then Just candidate else Nothing)

lookupUnqualifiedTypeName :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedTypeName imports ident = do
   symbol <- lookupUnqualifiedSymbol imports ident
   pure (symbol >>= \candidate -> if isTypeSymbol candidate then Just candidate else Nothing)

-- | The module that declares the type visible under this name: the current
--   module first, then the unqualified imports. Re-exports resolve to the
--   declaring module, so a type is the same however it was imported.
findTypeModule :: [Located ImportDef] -> TypeName -> SemAnalyzer (Maybe Module)
findTypeModule imports name = do
   currentModule <- gets (.currentModule)
   modules <- gets (.modules)
   let importedModules = imports
         |> map (.node)
         |> filter (\importDef -> importBringsIn modules importDef name)
         |> map (\importDef -> declaringModuleThrough modules importDef.importModule name)
   let candidates = currentModule : importedModules
   pure (find (\module' -> declaresType modules module' name) candidates)

-- | Any module that declares a type with this name. Used for builtin types such
--   as the type of an integer literal, which may be used without being imported.
findAnyTypeModule :: TypeName -> SemAnalyzer (Maybe Module)
findAnyTypeModule name = do
   modules <- gets (.modules)
   pure $ M.keys modules
      |> find (\module' -> declaresType modules module' name)

-- | Whether an unqualified import makes this name visible without a module alias.
importBringsIn :: M.Map Module ModuleInfo -> ImportDef -> Ident -> Bool
importBringsIn modules importDef ident = case importDef.payload of
   ImpAlias _ -> False
   ImpSelect identifiers -> identifiers
      |> any (\identifier -> identifier.node == ident)
   ImpFull -> case M.lookup importDef.importModule modules of
      Just moduleInfo -> M.member ident moduleInfo.exports
      Nothing         -> False

-- | Whether the module declares a type (or an extern type) with this name.
declaresType :: M.Map Module ModuleInfo -> Module -> TypeName -> Bool
declaresType modules module' name =
   let symbol = M.lookup module' modules >>= \moduleInfo -> M.lookup name moduleInfo.symbols
   in maybe False isTypeSymbol symbol

lookupCurrentFunction :: Ident -> SemAnalyzer (Maybe FuncSig)
lookupCurrentFunction ident = do
   symbol <- lookupCurrentModule ident
   pure $ case symbol of
      Just (SymbolFn _ _ signature)       -> Just signature
      Just (SymbolExternFn _ _ signature) -> Just signature
      _                                   -> Nothing

lookupUnqualifiedFunction :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe FuncSig)
lookupUnqualifiedFunction imports ident = do
   symbol <- lookupUnqualifiedSymbol imports ident
   pure $ case symbol of
      Just (SymbolFn _ _ signature)       -> Just signature
      Just (SymbolExternFn _ _ signature) -> Just signature
      _                                   -> Nothing

lookupQualifiedFunction :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe FuncSig)
lookupQualifiedFunction imports alias ident = do
   symbol <- lookupQualifiedSymbol imports alias ident
   pure $ case symbol of
      Just (SymbolFn _ _ signature)       -> Just signature
      Just (SymbolExternFn _ _ signature) -> Just signature
      _                                   -> Nothing

lookupCurrentSystem :: Ident -> SemAnalyzer (Maybe SystemSig)
lookupCurrentSystem ident = do
   symbol <- lookupCurrentModule ident
   pure $ case symbol of
      Just (SymbolSystem _ signature) -> Just signature
      _ -> Nothing