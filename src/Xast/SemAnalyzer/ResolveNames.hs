{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.ResolveNames where

import Control.Monad.State
import Control.Monad (forM_, forM)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query
import Text.Megaparsec (SourcePos(sourceName))
import Control.Applicative ((<|>))
import Xast.SemAnalyzer.Unify (freshLocalScope)

-- #### RESOLVE NAMES ####
resolveNames :: Program Parsed -> SemAnalyzer (Program Resolved)
resolveNames (Program md@(ModuleDef _ m _) imps stmts src) = do
   resolveDefImplMatches stmts

   modify $ \st -> st { currentModule = m }
   stmts' <- forM stmts $ \case
      StmtFunc (FnImpl (FuncImpl implLoc fnIdent args body)) -> do
         scope <- freshCheckedScope M.empty imps (concatMap patternVarLocs args)
         body' <- resolveExprAt scope imps body
         let args' = map (resolvePattern scope) args
         pure $ StmtFunc (FnImpl (FuncImpl implLoc fnIdent args' body'))

      StmtSystem (SysImpl (SystemImpl implLoc sysIdent entPats mWith body)) -> do
         entScope  <- freshCheckedScope M.empty imps (concatMap (\(EntityPattern bs) -> concatMap (patternVarLocs . (.pat)) bs) entPats)
         withScope <- freshCheckedScope entScope imps (maybe [] (concatMap patternVarLocs) mWith)
         body' <- resolveExprAt (M.union entScope withScope) imps body

         sig <- lookupCurrentSystem sysIdent
         let sigEnts = maybe [] (.entities) sig
             sigRet  = maybe TyInvalid (.retType) sig
             sigEnts' = map Just sigEnts ++ repeat Nothing
         let entPats' = zipWith (resolveEntityPattern entScope sigRet) sigEnts' entPats
         let mWith' = fmap (map (resolvePattern withScope)) mWith
         pure $ StmtSystem (SysImpl (SystemImpl implLoc sysIdent entPats' mWith' body'))

      StmtSystem (SysDef def) -> pure $ StmtSystem (SysDef def)

      StmtFunc (FnDef def) -> pure $ StmtFunc (FnDef def)

      StmtTypeDef td -> pure $ StmtTypeDef td

      StmtExtern ext -> pure $ StmtExtern ext

   pure $ Program md imps stmts' src

resolveDefImplMatches :: [Stmt Parsed] -> SemAnalyzer ()
resolveDefImplMatches stmts = go stmts stmts
   where
      go (x:xs) allStmts = case x of
         StmtFunc (FnImpl (FuncImpl impLoc impIdent _ _)) ->
            let matching = flip mapMaybe allStmts $ \case
                  StmtFunc (FnDef (FuncDef defLoc _ defIdent _ _)) ->
                     if defIdent == impIdent then
                        Just defLoc
                     else
                        Nothing
                  _ -> Nothing
                defCount = length matching
            in if defCount == 0 then do
               errSem (SEMissingFnDef impLoc impIdent)
               go xs allStmts
            else if defCount > 1 then do
               errSem (SEExtraFnDef impLoc impIdent matching)
               go xs allStmts
            else
               go xs allStmts

         StmtFunc (FnDef (FuncDef defLoc _ defIdent _ _)) ->
            let matching = flip filter allStmts $ \case
                  StmtFunc (FnImpl (FuncImpl _ impIdent _ _)) -> impIdent == defIdent
                  _ -> False
                count = length matching
            in if count == 0 then do
               errSem (SEMissingFnImpls defLoc defIdent)
               go xs allStmts
            else
               go xs allStmts

         StmtSystem (SysImpl (SystemImpl impLoc impIdent _ _ _)) ->
            let matching = flip mapMaybe allStmts $ \case
                  StmtSystem (SysDef (SystemDef defLoc _ defIdent _ _ _)) ->
                     if defIdent == impIdent then
                        Just defLoc
                     else
                        Nothing
                  _ -> Nothing
                defCount = length matching
            in if defCount == 0 then do
               errSem (SEMissingSystemDef impLoc impIdent)
               go xs allStmts
            else if defCount > 1 then do
               errSem (SEExtraSystemDef impLoc impIdent matching)
               go xs allStmts
            else
               go xs allStmts

         StmtSystem (SysDef (SystemDef defLoc _ defIdent _ _ _)) ->
            let matching = flip filter allStmts $ \case
                  StmtSystem (SysImpl (SystemImpl _ impIdent _ _ _)) -> impIdent == defIdent
                  _ -> False
                count = length matching
            in if count == 0 then do
               errSem (SEMissingSystemImpls defLoc defIdent)
               go xs allStmts
            else
               go xs allStmts

         _ -> go xs allStmts

      go [] _ = pure ()

patternVarLocs :: Pattern Parsed -> [(Ident, Location)]
patternVarLocs = \case
   PatVar (ParsedInfo loc) x -> [(x, loc)]
   PatWildcard _ -> []
   PatLit _ _    -> []
   PatList _ ps  -> concatMap patternVarLocs ps
   PatTuple _ ps -> concatMap patternVarLocs ps
   PatCon _ _ ps -> concatMap patternVarLocs ps

freshCheckedScope
   :: M.Map Ident (LocalId, Location)
   -> [Located ImportDef]
   -> [(Ident, Location)]
   -> SemAnalyzer (M.Map Ident (LocalId, Location))
freshCheckedScope outerScope imps newVars = do
   checkNoDuplicates M.empty newVars
   forM_ newVars $ \(ident, loc) ->
      case M.lookup ident outerScope of
         Just (_, outerLoc) -> errSem (SEShadowedBinding ident outerLoc loc)
         Nothing -> do
            modSym <- lookupCurrentModule ident
            impSym <- lookupUnqualifiedSymbol imps ident
            case modSym <|> impSym of
               Just sym -> errSem (SEShadowedBinding ident (symbolLoc sym) loc)
               Nothing  -> pure ()
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
   PatVar (ParsedInfo loc) x ->
      PatVar (ResolvedInfo loc (ResLocal . fst <$> M.lookup x scope)) x
   PatWildcard (ParsedInfo loc) ->
      PatWildcard (ResolvedInfo loc Nothing)
   PatLit (ParsedInfo loc) lit ->
      PatLit (ResolvedInfo loc Nothing) lit
   PatList (ParsedInfo loc) ps ->
      PatList (ResolvedInfo loc Nothing) (map (resolvePattern scope) ps)
   PatTuple (ParsedInfo loc) ps ->
      PatTuple (ResolvedInfo loc Nothing) (map (resolvePattern scope) ps)
   PatCon (ParsedInfo loc) ident ps ->
      PatCon (ResolvedInfo loc Nothing) ident (map (resolvePattern scope) ps)

componentAccess :: Type -> Type -> BindingAccess
componentAccess sysRet ty
   | typeContains ty sysRet = AccessWrite
   | otherwise              = AccessRead

typeContains :: Type -> Type -> Bool
typeContains needle haystack
   | needle == haystack = True
   | otherwise = case haystack of
      TyApp a b   -> typeContains needle a || typeContains needle b
      TyTuple xs  -> any (typeContains needle) xs
      TyFn args r -> any (typeContains needle) args || typeContains needle r
      _           -> False

resolveEntityPattern :: M.Map Ident (LocalId, Location) -> Type -> Maybe QueriedEntity -> EntityPattern Parsed -> EntityPattern Resolved
resolveEntityPattern scope sysRet mEnt (EntityPattern bindings) =
   EntityPattern (zipWith bindOne tys bindings)
   where
      tys = case mEnt of
         Just (QueriedEntity ts) -> map (Just . (.node)) ts ++ repeat Nothing
         Nothing                 -> repeat Nothing

      bindOne mTy (EntPatBinding pat _) =
         EntPatBinding (resolvePattern scope pat) (maybe AccessRead (componentAccess sysRet) mTy)

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
resolveExpr scope imps expr = case expr of
   ExpVar (ParsedInfo loc) Nothing x -> case M.lookup x scope of
      Just (lid, _) -> pure $ ExpVar (ResolvedInfo loc (Just (ResLocal lid))) Nothing x
      Nothing -> do
         modSym <- lookupCurrentModule x
         impSym <- lookupUnqualifiedSymbol imps x
         case modSym <|> impSym of
            Just (SymbolFn _ fid _)       -> pure $ ExpVar (ResolvedInfo loc (Just (ResFunction fid))) Nothing x
            Just (SymbolExternFn _ eid _) -> pure $ ExpVar (ResolvedInfo loc (Just (ResExternFunction eid))) Nothing x
            _ -> do
               errSem (SEUndefinedVar loc x)
               pure $ ExpVar (ResolvedInfo loc Nothing) Nothing x

   ExpVar (ParsedInfo loc) (Just alias) x ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ a))) -> a == alias
            _ -> False
      in if not $ any hasAlias imps then do
         errSem (SEUndefinedAlias (sourceName loc.pos) alias)
         pure $ ExpVar (ResolvedInfo loc Nothing) (Just alias) x
      else do
         sym <- lookupQualifiedSymbol imps alias x
         case sym of
            Just (SymbolFn _ fid _)       -> pure $ ExpVar (ResolvedInfo loc (Just (ResFunction fid))) (Just alias) x
            Just (SymbolExternFn _ eid _) -> pure $ ExpVar (ResolvedInfo loc (Just (ResExternFunction eid))) (Just alias) x
            _ -> do
               errSem (SEUndefinedVar loc x)
               pure $ ExpVar (ResolvedInfo loc Nothing) (Just alias) x

   ExpCon (ParsedInfo loc) Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      case modCon <|> impCon of
         Just (SymbolCtor _ cid _) -> pure $ ExpCon (ResolvedInfo loc (Just (ResConstructor cid))) Nothing x
         _ -> do
            errSem (SEUndefinedCon loc x)
            pure $ ExpCon (ResolvedInfo loc Nothing) Nothing x

   ExpCon (ParsedInfo loc) (Just alias) x ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ a))) -> a == alias
            _ -> False
      in if not $ any hasAlias imps then do
         errSem (SEUndefinedAlias (sourceName loc.pos) alias)
         pure $ ExpCon (ResolvedInfo loc Nothing) (Just alias) x
      else do
         con <- lookupQualifiedConstructor imps alias x
         case con of
            Just (SymbolCtor _ cid _) -> pure $ ExpCon (ResolvedInfo loc (Just (ResConstructor cid))) (Just alias) x
            _ -> do
               errSem (SEUndefinedCon loc x)
               pure $ ExpCon (ResolvedInfo loc Nothing) (Just alias) x

   ExpTuple (ParsedInfo loc) xs -> ExpTuple (ResolvedInfo loc Nothing) <$> mapM (resolveExprAt scope imps) xs

   ExpList (ParsedInfo loc) xs -> ExpList (ResolvedInfo loc Nothing) <$> mapM (resolveExprAt scope imps) xs

   ExpLit (ParsedInfo loc) lit -> pure $ ExpLit (ResolvedInfo loc Nothing) lit

   ExpLambda (ParsedInfo loc) (Lambda args body) -> do
      argScope <- freshCheckedScope scope imps (concatMap patternVarLocs args)
      body' <- resolveExprAt (M.union argScope scope) imps body
      let args' = map (resolvePattern argScope) args
      pure $ ExpLambda (ResolvedInfo loc Nothing) (Lambda args' body')

   ExpApp (ParsedInfo loc) lhs rhs -> do
      lhs' <- resolveExprAt scope imps lhs
      rhs' <- resolveExprAt scope imps rhs
      pure $ ExpApp (ResolvedInfo loc Nothing) lhs' rhs'

   ExpLetIn (ParsedInfo loc) (LetIn binds body) -> do
      bindScope <- freshCheckedScope scope imps (concatMap (patternVarLocs . (.pat)) binds)
      let scope' = M.union bindScope scope
      binds' <- forM binds $ \(Let pat value) -> do
         value' <- resolveExprAt scope' imps value
         pure $ Let (resolvePattern bindScope pat) value'
      body' <- resolveExprAt scope' imps body
      pure $ ExpLetIn (ResolvedInfo loc Nothing) (LetIn binds' body')

   ExpIfThen (ParsedInfo loc) (IfThenElse cond tr fl) -> do
      cond' <- resolveExprAt scope imps cond
      tr' <- resolveExprAt scope imps tr
      fl' <- resolveExprAt scope imps fl
      pure $ ExpIfThen (ResolvedInfo loc Nothing) (IfThenElse cond' tr' fl')

   ExpMatch (ParsedInfo loc) (Match mtExp mtMatches exhaustive) -> do
      mtExp' <- resolveExprAt scope imps mtExp
      mtMatches' <- forM mtMatches $ \(MatchWing pat branch) -> do
         patScope <- freshCheckedScope scope imps (patternVarLocs pat)
         branch' <- resolveExprAt (M.union patScope scope) imps branch
         pure $ MatchWing (resolvePattern patScope pat) branch'
      pure $ ExpMatch (ResolvedInfo loc Nothing) (Match mtExp' mtMatches' exhaustive)

   ExpRecConstruct (ParsedInfo loc) (RecConstruct rcBind rcCon rcAssigns) -> do
      rcAssigns' <- forM rcAssigns $ \(RecAssign fld value) -> do
         value' <- resolveExprAt scope imps value
         pure $ RecAssign fld value'
      pure $ ExpRecConstruct (ResolvedInfo loc Nothing) (RecConstruct rcBind rcCon rcAssigns')

   ExpRecUpdate (ParsedInfo loc) (RecUpdate ruBase ruAssigns) -> do
      ruBase' <- resolveExprAt scope imps ruBase
      ruAssigns' <- forM ruAssigns $ \(RecAssign fld value) -> do
         value' <- resolveExprAt scope imps value
         pure $ RecAssign fld value'
      pure $ ExpRecUpdate (ResolvedInfo loc Nothing) (RecUpdate ruBase' ruAssigns')

   ExpVarGetter (ParsedInfo loc) baseExpr getter -> do
      baseExpr' <- resolveExprAt scope imps baseExpr
      pure $ ExpVarGetter (ResolvedInfo loc Nothing) baseExpr' getter
