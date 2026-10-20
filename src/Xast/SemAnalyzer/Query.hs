{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Query where

import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Monad (SemAnalyzer)
import Xast.AST
import Control.Monad.Reader (MonadReader(local), asks)
import qualified Data.Map as M
import Control.Monad.State (gets)
import qualified Data.Set as S

lookupLocal :: Ident -> SemAnalyzer (Maybe VarInfo)
lookupLocal ident = asks (M.lookup ident . (.vars))

withVars :: M.Map Ident VarInfo -> SemAnalyzer a -> SemAnalyzer a
withVars newVars =
   local (\env -> env { vars = M.union newVars env.vars })

-- | Look up a symbol by name in a specific module's symbol table
lookupInModule :: Module -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupInModule moduleName ident = gets $ \symTable ->
   M.lookup moduleName symTable.modules >>= \moduleInfo -> M.lookup ident moduleInfo.symbols

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
                  then lookupInModule moduleName ident >>= \case
                     Just symbol -> pure (Just symbol)
                     Nothing     -> go rest
                  else go rest
            ImpFull ->
               case M.lookup moduleName modules of
                  Nothing -> go rest
                  Just moduleInfo ->
                     if S.member ident moduleInfo.exports
                        then pure (M.lookup ident moduleInfo.symbols)
                        else go rest
   go imports

-- | Look up a symbol via a module alias: alias.ident
lookupQualifiedSymbol :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedSymbol imports alias ident = do
   let aliased = [ moduleName | Located _ (ImportDef moduleName (ImpAlias (Located _ importAlias))) <- imports, importAlias == alias ]
   case aliased of
      []             -> pure Nothing
      (moduleName:_) -> lookupInModule moduleName ident

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