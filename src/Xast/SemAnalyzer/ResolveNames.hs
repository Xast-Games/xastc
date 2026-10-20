{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.ResolveNames where

import Control.Monad.State
import Control.Monad (forM_, forM)
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query
import Text.Megaparsec (SourcePos(sourceName))
import Control.Applicative ((<|>))
import Xast.SemAnalyzer.Unify (freshLocalScope)
import qualified Xast.Utils.List as L

-- #### RESOLVE NAMES ####
resolveNames :: Program Parsed -> SemAnalyzer (Program Resolved)
resolveNames (Program moduleDef@(ModuleDef _ moduleName _) imports stmts src) = do
   resolveDefImplMatches stmts

   modify $ \symTable -> symTable { currentModule = moduleName }
   stmts' <- forM stmts $ \case
      StmtFunc (FnImpl (FuncImpl implLoc fnIdent args body)) -> do
         scope <- freshCheckedScope M.empty imports (concatMap patternVarLocs args)
         body' <- resolveExprAt scope imports body
         let args' = map (resolvePattern scope) args
         pure $ StmtFunc (FnImpl (FuncImpl implLoc fnIdent args' body'))

      StmtSystem (SysImpl (SystemImpl implLoc sysIdent entityPatterns maybeWith body)) -> do
         let entityPatternVars =
               L.forFlat entityPatterns $ \(EntityPattern bindings) ->
                  L.forFlat bindings $ \binding -> patternVarLocs binding.pat
         entScope  <- freshCheckedScope M.empty imports entityPatternVars
         withScope <- freshCheckedScope entScope imports (maybe [] (concatMap patternVarLocs) maybeWith)
         body' <- resolveExprAt (M.union entScope withScope) imports body

         signature <- lookupCurrentSystem sysIdent
         let signatureEntities = maybe [] (.entities) signature
             signatureRetType   = maybe TyInvalid (.retType) signature
             signatureEntities' = map Just signatureEntities ++ repeat Nothing
         let entityPatterns' = zipWith (resolveEntityPattern entScope signatureRetType) signatureEntities' entityPatterns
         let maybeWith' = fmap (map (resolvePattern withScope)) maybeWith
         pure $ StmtSystem (SysImpl (SystemImpl implLoc sysIdent entityPatterns' maybeWith' body'))

      StmtSystem (SysDef systemDef) -> pure $ StmtSystem (SysDef systemDef)

      StmtFunc (FnDef funcDef) -> pure $ StmtFunc (FnDef funcDef)

      StmtTypeDef typeDef -> pure $ StmtTypeDef typeDef

      StmtExtern externDef -> pure $ StmtExtern externDef

   pure $ Program moduleDef imports stmts' src

resolveDefImplMatches :: [Stmt Parsed] -> SemAnalyzer ()
resolveDefImplMatches stmts = go stmts stmts
   where
      go (stmt:rest) allStmts = case stmt of
         StmtFunc (FnImpl (FuncImpl impLoc impIdent _ _)) ->
            let matching = L.forMaybe allStmts $ \case
                  StmtFunc (FnDef (FuncDef defLoc _ defIdent _ _)) ->
                     if defIdent == impIdent then
                        Just defLoc
                     else
                        Nothing
                  _ -> Nothing
                defCount = length matching
            in if defCount == 0 then do
               errSem (SEMissingFnDef impLoc impIdent)
               go rest allStmts
            else if defCount > 1 then do
               errSem (SEExtraFnDef impLoc impIdent matching)
               go rest allStmts
            else
               go rest allStmts

         StmtFunc (FnDef (FuncDef defLoc _ defIdent _ _)) ->
            let matching = flip filter allStmts $ \case
                  StmtFunc (FnImpl (FuncImpl _ impIdent _ _)) -> impIdent == defIdent
                  _ -> False
                count = length matching
            in if count == 0 then do
               errSem (SEMissingFnImpls defLoc defIdent)
               go rest allStmts
            else
               go rest allStmts

         StmtSystem (SysImpl (SystemImpl impLoc impIdent _ _ _)) ->
            let matching = L.forMaybe allStmts $ \case
                  StmtSystem (SysDef (SystemDef defLoc _ defIdent _ _ _)) ->
                     if defIdent == impIdent then
                        Just defLoc
                     else
                        Nothing
                  _ -> Nothing
                defCount = length matching
            in if defCount == 0 then do
               errSem (SEMissingSystemDef impLoc impIdent)
               go rest allStmts
            else if defCount > 1 then do
               errSem (SEExtraSystemDef impLoc impIdent matching)
               go rest allStmts
            else
               go rest allStmts

         StmtSystem (SysDef (SystemDef defLoc _ defIdent _ _ _)) ->
            let matching = flip filter allStmts $ \case
                  StmtSystem (SysImpl (SystemImpl _ impIdent _ _ _)) -> impIdent == defIdent
                  _ -> False
                count = length matching
            in if count == 0 then do
               errSem (SEMissingSystemImpls defLoc defIdent)
               go rest allStmts
            else
               go rest allStmts

         _ -> go rest allStmts

      go [] _ = pure ()

patternVarLocs :: Pattern Parsed -> [(Ident, Location)]
patternVarLocs = \case
   PatVar (ParsedInfo loc) name -> [(name, loc)]
   PatWildcard _                -> []
   PatLit _ _                   -> []
   PatList _ patterns           -> concatMap patternVarLocs patterns
   PatTuple _ patterns          -> concatMap patternVarLocs patterns
   PatCon _ _ patterns          -> concatMap patternVarLocs patterns

freshCheckedScope
   :: M.Map Ident (LocalId, Location)
   -> [Located ImportDef]
   -> [(Ident, Location)]
   -> SemAnalyzer (M.Map Ident (LocalId, Location))
freshCheckedScope outerScope imports newVars = do
   checkNoDuplicates M.empty newVars
   forM_ newVars $ \(ident, loc) ->
      case M.lookup ident outerScope of
         Just (_, outerLoc) -> errSem (SEShadowedBinding ident outerLoc loc)
         Nothing -> do
            currentModuleSymbol <- lookupCurrentModule ident
            importedSymbol <- lookupUnqualifiedSymbol imports ident
            case currentModuleSymbol <|> importedSymbol of
               Just symbol -> errSem (SEShadowedBinding ident (symbolLoc symbol) loc)
               Nothing     -> pure ()
   freshLocalScope newVars
   where
      checkNoDuplicates _ [] = pure ()
      checkNoDuplicates seen ((ident, loc) : rest) =
         case M.lookup ident seen of
            Just firstLoc -> do
               errSem (SEDuplicateBinding ident firstLoc loc)
               checkNoDuplicates seen rest
            Nothing ->
               checkNoDuplicates (M.insert ident loc seen) rest

resolvePattern :: M.Map Ident (LocalId, Location) -> Pattern Parsed -> Pattern Resolved
resolvePattern scope = \case
   PatVar (ParsedInfo loc) name ->
      PatVar (ResolvedInfo loc (ResLocal . fst <$> M.lookup name scope)) name
   PatWildcard (ParsedInfo loc) ->
      PatWildcard (ResolvedInfo loc Nothing)
   PatLit (ParsedInfo loc) lit ->
      PatLit (ResolvedInfo loc Nothing) lit
   PatList (ParsedInfo loc) patterns ->
      PatList (ResolvedInfo loc Nothing) (map (resolvePattern scope) patterns)
   PatTuple (ParsedInfo loc) patterns ->
      PatTuple (ResolvedInfo loc Nothing) (map (resolvePattern scope) patterns)
   PatCon (ParsedInfo loc) ident patterns ->
      PatCon (ResolvedInfo loc Nothing) ident (map (resolvePattern scope) patterns)

componentAccess :: Type -> Type -> BindingAccess
componentAccess systemRetType componentType
   | typeContains componentType systemRetType = AccessWrite
   | otherwise                                = AccessRead

typeContains :: Type -> Type -> Bool
typeContains needle haystack
   | needle == haystack = True
   | otherwise = case haystack of
      TyApp func arg      -> typeContains needle func || typeContains needle arg
      TyTuple memberTypes -> any (typeContains needle) memberTypes
      TyFn args retType   -> any (typeContains needle) args || typeContains needle retType
      _                   -> False

resolveEntityPattern :: M.Map Ident (LocalId, Location) -> Type -> Maybe QueriedEntity -> EntityPattern Parsed -> EntityPattern Resolved
resolveEntityPattern scope systemRetType maybeEntity (EntityPattern bindings) =
   EntityPattern (zipWith bindOne componentTypes bindings)
   where
      componentTypes = case maybeEntity of
         Just (QueriedEntity entityTypes) -> map (Just . (.node)) entityTypes ++ repeat Nothing
         Nothing                          -> repeat Nothing

      bindOne maybeType (EntPatBinding pat _) =
         EntPatBinding (resolvePattern scope pat) (maybe AccessRead (componentAccess systemRetType) maybeType)

resolveExprAt
   :: M.Map Ident (LocalId, Location)
   -> [Located ImportDef]
   -> Expr Parsed
   -> SemAnalyzer (Expr Resolved)
resolveExprAt = resolveExpr

resolveExpr
   :: M.Map Ident (LocalId, Location)
   -> [Located ImportDef]
   -> Expr Parsed
   -> SemAnalyzer (Expr Resolved)
resolveExpr scope imports expr = case expr of
   ExpVar (ParsedInfo loc) Nothing name -> case M.lookup name scope of
      Just (localId, _) -> pure $ ExpVar (ResolvedInfo loc (Just (ResLocal localId))) Nothing name
      Nothing -> do
         currentModuleSymbol <- lookupCurrentModule name
         importedSymbol <- lookupUnqualifiedSymbol imports name
         case currentModuleSymbol <|> importedSymbol of
            Just (SymbolFn _ functionId _)     -> pure $ ExpVar (ResolvedInfo loc (Just (ResFunction functionId))) Nothing name
            Just (SymbolExternFn _ externId _) -> pure $ ExpVar (ResolvedInfo loc (Just (ResExternFunction externId))) Nothing name
            _                                  -> do
               errSem (SEUndefinedVar loc name)
               pure $ ExpVar (ResolvedInfo loc Nothing) Nothing name

   ExpVar (ParsedInfo loc) (Just alias) name ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ importAlias))) -> importAlias == alias
            _ -> False
      in if not $ any hasAlias imports then do
         errSem (SEUndefinedAlias (sourceName loc.pos) alias)
         pure $ ExpVar (ResolvedInfo loc Nothing) (Just alias) name
      else do
         qualifiedSymbol <- lookupQualifiedSymbol imports alias name
         case qualifiedSymbol of
            Just (SymbolFn _ functionId _)     -> pure $ ExpVar (ResolvedInfo loc (Just (ResFunction functionId))) (Just alias) name
            Just (SymbolExternFn _ externId _) -> pure $ ExpVar (ResolvedInfo loc (Just (ResExternFunction externId))) (Just alias) name
            _                                  -> do
               errSem (SEUndefinedVar loc name)
               pure $ ExpVar (ResolvedInfo loc Nothing) (Just alias) name

   ExpCon (ParsedInfo loc) Nothing name -> do
      currentModuleConstructor <- lookupCurrentConstructor name
      importedConstructor <- lookupUnqualifiedConstructor imports name
      case currentModuleConstructor <|> importedConstructor of
         Just (SymbolCtor _ constructorId _) -> pure $ ExpCon (ResolvedInfo loc (Just (ResConstructor constructorId))) Nothing name
         _ -> do
            errSem (SEUndefinedCon loc name)
            pure $ ExpCon (ResolvedInfo loc Nothing) Nothing name

   ExpCon (ParsedInfo loc) (Just alias) name ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ importAlias))) -> importAlias == alias
            _ -> False
      in if not $ any hasAlias imports then do
         errSem (SEUndefinedAlias (sourceName loc.pos) alias)
         pure $ ExpCon (ResolvedInfo loc Nothing) (Just alias) name
      else do
         qualifiedConstructor <- lookupQualifiedConstructor imports alias name
         case qualifiedConstructor of
            Just (SymbolCtor _ constructorId _) -> pure $ ExpCon (ResolvedInfo loc (Just (ResConstructor constructorId))) (Just alias) name
            _ -> do
               errSem (SEUndefinedCon loc name)
               pure $ ExpCon (ResolvedInfo loc Nothing) (Just alias) name

   ExpTuple (ParsedInfo loc) expressions -> ExpTuple (ResolvedInfo loc Nothing) <$> mapM (resolveExprAt scope imports) expressions

   ExpList (ParsedInfo loc) expressions -> ExpList (ResolvedInfo loc Nothing) <$> mapM (resolveExprAt scope imports) expressions

   ExpLit (ParsedInfo loc) lit -> pure $ ExpLit (ResolvedInfo loc Nothing) lit

   ExpLambda (ParsedInfo loc) (Lambda args body) -> do
      argScope <- freshCheckedScope scope imports (concatMap patternVarLocs args)
      body' <- resolveExprAt (M.union argScope scope) imports body
      let args' = map (resolvePattern argScope) args
      pure $ ExpLambda (ResolvedInfo loc Nothing) (Lambda args' body')

   ExpApp (ParsedInfo loc) lhs rhs -> do
      lhs' <- resolveExprAt scope imports lhs
      rhs' <- resolveExprAt scope imports rhs
      pure $ ExpApp (ResolvedInfo loc Nothing) lhs' rhs'

   ExpLetIn (ParsedInfo loc) (LetIn bindings body) -> do
      bindScope <- freshCheckedScope scope imports (concatMap (patternVarLocs . (.pat)) bindings)
      let scope' = M.union bindScope scope
      bindings' <- forM bindings $ \(Let pat value) -> do
         value' <- resolveExprAt scope' imports value
         pure $ Let (resolvePattern bindScope pat) value'
      body' <- resolveExprAt scope' imports body
      pure $ ExpLetIn (ResolvedInfo loc Nothing) (LetIn bindings' body')

   ExpIfThen (ParsedInfo loc) (IfThenElse condition thenBranch elseBranch) -> do
      condition' <- resolveExprAt scope imports condition
      thenBranch' <- resolveExprAt scope imports thenBranch
      elseBranch' <- resolveExprAt scope imports elseBranch
      pure $ ExpIfThen (ResolvedInfo loc Nothing) (IfThenElse condition' thenBranch' elseBranch')

   ExpMatch (ParsedInfo loc) (Match matchedExpr matchWings exhaustive) -> do
      matchedExpr' <- resolveExprAt scope imports matchedExpr
      matchWings' <- forM matchWings $ \(MatchWing pat branch) -> do
         patScope <- freshCheckedScope scope imports (patternVarLocs pat)
         branch' <- resolveExprAt (M.union patScope scope) imports branch
         pure $ MatchWing (resolvePattern patScope pat) branch'
      pure $ ExpMatch (ResolvedInfo loc Nothing) (Match matchedExpr' matchWings' exhaustive)

   ExpRecConstruct (ParsedInfo loc) (RecConstruct recordBinding recordCtor assigns) -> do
      assigns' <- forM assigns $ \(RecAssign field value) -> do
         value' <- resolveExprAt scope imports value
         pure $ RecAssign field value'
      pure $ ExpRecConstruct (ResolvedInfo loc Nothing) (RecConstruct recordBinding recordCtor assigns')

   ExpRecUpdate (ParsedInfo loc) (RecUpdate baseRecord assigns) -> do
      baseRecord' <- resolveExprAt scope imports baseRecord
      assigns' <- forM assigns $ \(RecAssign field value) -> do
         value' <- resolveExprAt scope imports value
         pure $ RecAssign field value'
      pure $ ExpRecUpdate (ResolvedInfo loc Nothing) (RecUpdate baseRecord' assigns')

   ExpVarGetter (ParsedInfo loc) baseExpr getter -> do
      baseExpr' <- resolveExprAt scope imports baseExpr
      pure $ ExpVarGetter (ResolvedInfo loc Nothing) baseExpr' getter
