{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Pass where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.State
import Control.Monad.Writer (listen, censor)
import Control.Monad (forM_, unless, when, foldM, zipWithM_, zipWithM, forM)
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.List (sortBy, sortOn, groupBy, transpose)
import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Data.Map as M

import Xast.AST
import Xast.Error.Types
import Xast.Utils.List (allEqual, pairs)
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Data.Function (on)
import Xast.SemAnalyzer.Query
import Text.Megaparsec (SourcePos(sourceName))
import Control.Applicative ((<|>))
import Xast.Utils.Generic (unreachableWith, (<--))
import qualified Data.Text as T
import Xast.Utils.Generic ((|>))
import Xast.Utils.Compiler (Target (Target32, Target64))
import Control.Monad.RWS (asks)

-- #### FULL ANALYSIS ####

fullAnalysis
   :: Monad m
   => ([SemWarning] -> m ())
   -> (FilePath -> String -> m ())
   -> [Program Parsed]
   -> Target
   -> ExceptT [SemError] m AnalysisResult
fullAnalysis reportWarnings saveFile progs target = do
   let env = emptyEnv target
       st0 = emptySymTable

   (_, st1, warns1) <- ExceptT $ pure $ runPhase env st0 (forM progs declareStmts)
   lift $ reportWarnings warns1

   (_, st2, warns2) <- ExceptT $ pure $ runPhase env st1 (importAnalysis progs)
   lift $ reportWarnings warns2

   (_, st3, warns3) <- ExceptT $ pure $ runPhase env st2 (forM_ progs resolveTypeUsages)
   lift $ reportWarnings warns3

   (progsResolved, st4, warns4) <- ExceptT $ pure $ runPhase env st3 (forM progs resolveNames)
   lift $ reportWarnings warns4

   (progsTyped, st5, warns5) <- ExceptT $ pure $ runPhase env st4 (forM progsResolved typeCheck)
   lift $ reportWarnings warns5

   -- Types stored on AST nodes may have been frozen before later unification
   -- resolved their type variables (e.g. a variable's own reference is typed
   -- before its use forces a substitution). Zonk every node against the final
   -- substitution map so the typed AST reflects fully-resolved types.
   let progsZonked = map (zonkProgram st5.tySubst) progsTyped

   (progsDesugared, st6, warns6) <- ExceptT $ pure $ runPhase env st5 (forM progsZonked desugarProgram)

   let warningsCount = sum $ map length [warns1, warns2, warns3, warns4, warns5, warns6]

   return $ AnalysisResult
      { warningsCount = warningsCount
      , progs = progsDesugared
      , ctorSpans = collectCtorSpans st6
      }

-- | Number of constructors of the owning type for every constructor id
-- (`maxBound` for non-exhaustive types), used by the decision tree compiler.
collectCtorSpans :: SymTable -> M.Map ConstructorId Int
collectCtorSpans st = M.fromList
   [ (cid, spanOf modInfo sig.owner)
   | modInfo <- M.elems st.modules
   , sym <- M.elems modInfo.symbols
   , (cid, sig) <- case sym of
         SymbolCtor _ c s       -> [(c, s)]
         SymbolTypeCtor _ _ c s -> [(c, s)]
         _                      -> []
   ]
   where
      spanOf modInfo owner = case M.lookup owner modInfo.symbols of
         Just (SymbolType _ ts)         -> tsSpan ts
         Just (SymbolTypeCtor _ ts _ _) -> tsSpan ts
         _                              -> maxBound
      tsSpan ts
         | ts.nonExhaustive = maxBound
         | otherwise        = S.size ts.ctors

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

-- #### RESOLVE IMPORTS ####

importAnalysis :: [Program Parsed] -> SemAnalyzer ()
importAnalysis progs = do
   -- Resolve A imports A
   forM_ progs resolveSelfImport

   -- Resolve A imports B -> B imports A
   resolveCyclicImports progs

   -- Resolve A imports from B multiple times
   forM_ progs resolveRedundantImports

   -- Initialize exports and resolve missing exported symbols
   forM_ progs resolveInvalidExports

   -- Resolve A imports private/missing symbols from B
   -- or the module B is missing itself
   resolveMissing progs

   -- 1) Resolve A and B imports the same symbol x
   -- 2) Resolve import both A and B as M
   forM_ progs resolveAmbiguity

   -- Resolve import x from A vs local declaration x
   forM_ progs resolveImportDeclConflicts

resolveAmbiguity :: Program Parsed -> SemAnalyzer ()
resolveAmbiguity (Program _ imps _ _) = do
   ms <- gets (.modules)
   let aliasPairs =
         [ (a, loc)
         | Located loc (ImportDef _ (ImpAlias (Located _ a))) <- imps
         ]
   let aliasMap = M.fromListWith (++) [ (a, [loc]) | (a, loc) <- aliasPairs ]

   forM_ (M.toList aliasMap) $ \(a, locs) ->
      let sorted = sortBy sortLocByPos locs
      in case sorted of
         _l1:_l2:_ ->
            forM_ (pairs sorted) $
               \(loc1, loc2) -> errSem (SEAmbiguousAlias a loc1 loc2)
         _ ->
            pure ()

   let addMany m ident loc = M.insertWith S.union ident (S.singleton loc) m
   imported <- foldM
      (\acc (Located loc (ImportDef m pl)) ->
         case pl of
            ImpAlias _ -> pure acc
            ImpSelect ids ->
               case M.lookup m ms of
                  Just _ -> do
                     exps <- getModuleExports m
                     let names = [ i.node | i <- ids, i.node `S.member` exps ]
                     pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
                  Nothing ->
                     let names = map (.node) ids
                     in pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
            ImpFull ->
               case M.lookup m ms of
                  Just _ -> do
                     exps <- getModuleExports m
                     let names = S.toList exps
                     pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
                  Nothing -> pure acc
      )
      M.empty
      imps
   forM_ (M.toList imported) $ \(ident, locs) ->
      let sorted = sortBy sortLocByPos (S.toList locs)
      in case sorted of
         _l1:_l2:_ ->
            forM_ (pairs sorted) $
               \(loc1, loc2) -> errSem (SEAmbiguousImport ident loc1 loc2)
         _ ->
            pure ()

resolveImportDeclConflicts :: Program Parsed -> SemAnalyzer ()
resolveImportDeclConflicts (Program (ModuleDef _ m _) imps _ _) = do
   ms <- gets (.modules)
   let addMany mp ident loc = M.insertWith S.union ident (S.singleton loc) mp
   imported <- foldM
      (\acc (Located loc (ImportDef module_ pl)) ->
         case pl of
            ImpAlias _ -> pure acc
            ImpSelect ids ->
               case M.lookup module_ ms of
                  Just _ -> do
                     exps <- getModuleExports module_
                     let names = [ i.node | i <- ids, i.node `S.member` exps ]
                     pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
                  Nothing ->
                     let names = map (.node) ids
                     in pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
            ImpFull ->
               case M.lookup module_ ms of
                  Just _ -> do
                     exps <- getModuleExports module_
                     let names = S.toList exps
                     pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
                  Nothing -> pure acc
      )
      M.empty
      imps

   moduleData <- getModuleSymbols m
   forM_ (M.toList imported) $ \(ident, importLocs) ->
      case M.lookup ident moduleData of
         Just sym ->
            let declLoc = symbolLoc sym
                sorted = sortBy sortLocByPos (S.toList importLocs)
            in case sorted of
               impLoc:_ -> errSem (SEImportDeclConflict ident impLoc declLoc)
               _ -> pure ()
         Nothing -> pure ()

resolveMissing :: [Program Parsed] -> SemAnalyzer ()
resolveMissing progs = do
   ms <- gets (.modules)
   forM_ progs $ \(Program _ imps _ _) ->
      forM_ imps $ \(Located loc (ImportDef m pl)) ->
         if M.member m ms then
            case pl of
               ImpSelect ids -> do
                  moduleData <- getModuleSymbols m
                  exports <- getModuleExports m

                  let nodes = map (.node) ids
                  let missing =
                        [ x
                        | x <- nodes
                        , x `M.notMember` moduleData
                        ]
                  let private =
                        [ y
                        | y <- nodes
                        , y `M.member` moduleData
                        , y `S.notMember` exports
                        ]

                  unless (null missing) $
                     errSem (SEMissingImports m loc missing)

                  unless (null private) $
                     errSem (SEPrivateImports m loc private)
               _ ->
                  pure ()
         else
            errSem (SEMissingModule m loc)


getModuleSymbols :: Module -> SemAnalyzer (M.Map Ident SymbolInfo)
getModuleSymbols m = gets $ \st ->
   st
      |> (.modules)
      |> M.lookup m
      |> fromJust
      |> (.symbols)

getModuleExports :: Module -> SemAnalyzer (S.Set Ident)
getModuleExports m = gets $ \st ->
   st
      |> (.modules)
      |> M.lookup m
      |> fromJust
      |> (.exports)

setModuleExports :: Module -> S.Set Ident -> SemAnalyzer ()
setModuleExports m exps =
   modify $ \st ->
      let mi = M.findWithDefault emptyModuleInfo m st.modules
      in st
         { modules = M.insert m (mi { exports = exps }) st.modules
         }

resolveInvalidExports :: Program Parsed -> SemAnalyzer ()
resolveInvalidExports (Program (ModuleDef _ m (Located loc exps)) _ _ _) =
   case exps of
      ExpSelect ids -> do
         moduleData <- getModuleSymbols m

         let invalid = filter (`M.notMember` moduleData) ids
         case invalid of
            [] -> setModuleExports m (S.fromList ids)
            err -> errSem (SEInvalidExport m loc err)

      ExpFull -> do
         symbols <- M.keys <$> getModuleSymbols m
         setModuleExports m (S.fromList symbols)

resolveRedundantImports :: Program Parsed -> SemAnalyzer ()
resolveRedundantImports (Program _ imports _ _) =
   when (length imports >= 2) $
      let intr = mapMaybe (uncurry intersectImport) (pairs imports)
      in forM_ intr $
         \i -> warnSem (SWRedundantImport i)

resolveCyclicImports :: [Program Parsed] -> SemAnalyzer ()
resolveCyclicImports progs = do
   let moduleMap = M.fromList [(fst (getModuleName p), getImports p) | p <- progs]
   let moduleLocations = M.fromList [getModuleName p | p <- progs]

   forM_ (M.keys moduleMap) $ \moduleName ->
      forM_ (M.lookup moduleName moduleLocations)
         ( detectCycle
            moduleMap
            moduleLocations
            S.empty
            [moduleName]
            moduleName
         )

getModuleName :: Program Parsed -> (Module, Location)
getModuleName (Program (ModuleDef loc name _) _ _ _) = (name, loc)

getImports :: Program Parsed -> [Module]
getImports (Program _ imports _ _) = [module_ | Located _ (ImportDef module_ _) <- imports]

detectCycle
   :: M.Map Module [Module]
   -> M.Map Module Location
   -> S.Set Module
   -> [Module]
   -> Module
   -> Location
   -> SemAnalyzer ()
detectCycle moduleMap moduleLocations visited path current loc
   | current `S.member` visited =
      case dropWhile (/= current) path of
         [] -> return ()
         cyc -> unless (allEqual cyc) $
            errSem (SECyclicImportError cyc loc)
   | otherwise =
      case M.lookup current moduleMap of
         Nothing -> return ()
         Just imports ->
            forM_ imports $ \imp ->
               forM_ (M.lookup imp moduleLocations)
                  ( detectCycle
                     moduleMap
                     moduleLocations
                     (S.insert current visited)
                     (path ++ [imp]) imp
                  )

resolveSelfImport :: Program Parsed -> SemAnalyzer ()
resolveSelfImport (Program (ModuleDef from this _) imports _ _) =
   case filter (\(Located _ (ImportDef imported _)) -> imported == this) imports of
      (Located to _):_ -> errSem (SESelfImportError this from to)
      [] -> return ()

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

-- #### Type checking ####

intBounds :: IntKind -> Target -> (Integer, Integer)
intBounds Byte _   = (-128, 127)
intBounds UByte _  = (0, 255)
intBounds Short _  = (-32768, 32767)
intBounds UShort _ = (0, 65535)
intBounds Int _    = (-2147483648, 2147483647)
intBounds UInt _   = (0, 4294967295)
intBounds Long _   = (-9223372036854775808, 9223372036854775807)
intBounds ULong _  = (0, 18446744073709551615)
intBounds Size  t@Target32 = intBounds Int t
intBounds USize t@Target32 = intBounds UInt t
intBounds Size  t@Target64 = intBounds Long t
intBounds USize t@Target64 = intBounds ULong t

checkIntegerBounds :: Expr Resolved -> SemAnalyzer ()
checkIntegerBounds = \case
   ExpLit res (LitInt lit) -> checkIntegerLiteralBounds lit res.location

   ExpApp res
      (ExpVar _ _ (Ident "opNeg"))
      (ExpLit _ (LitInt (IntLiteral kind value))) ->
         checkIntegerLiteralBounds
            (IntLiteral kind (negate value))
            res.location
   ExpApp _ op opnd ->
      checkIntegerBounds op >>
      checkIntegerBounds opnd

   ExpLit {} -> pure ()
   ExpVar {} -> pure ()
   ExpCon {} -> pure ()
   ExpTuple _ xs -> forM_ xs checkIntegerBounds
   ExpList _ xs -> forM_ xs checkIntegerBounds
   ExpLambda _ lambda -> checkIntegerBounds lambda.body
   ExpLetIn _ letIn ->
      checkIntegerBounds letIn.bindExpr >>
      forM_ letIn.bindings (\bind -> checkIntegerBounds bind.value)
   ExpMatch _ match ->
      checkIntegerBounds match.baseExpr >>
      forM_ match.matches (\(MatchWing _ expr) -> checkIntegerBounds expr)
   ExpIfThen _ ite ->
      checkIntegerBounds ite.ifExpr >>
      checkIntegerBounds ite.thenExpr >>
      checkIntegerBounds ite.elseExpr
   ExpRecConstruct _ recCon -> forM_ recCon.assigns $ \(RecAssign _ expr) -> checkIntegerBounds expr
   ExpRecUpdate _ recUpd ->
      checkIntegerBounds recUpd.base >>
      forM_ recUpd.assigns (\(RecAssign _ expr) -> checkIntegerBounds expr)
   ExpVarGetter _ expr _ -> checkIntegerBounds expr

checkIntegerLiteralBounds :: IntLiteral -> Location -> SemAnalyzer ()
checkIntegerLiteralBounds lit loc = do
   target <- asks (.currentTarget)
   let bounds@(low, high) = intBounds lit.kind target

   unless (lit.value >= low && lit.value <= high) $
      errSem (SEIntegerOutOfBounds loc lit.kind lit.value bounds)

typeCheck :: Program Resolved -> SemAnalyzer (Program Typed)
typeCheck (Program mdl@(ModuleDef _ m _) imps stmts src) = do
   modify $ \st -> st { currentModule = m }
   Program mdl imps <$> traverse (typeCheckStmt imps) stmts <-- src

typeCheckStmt :: [Located ImportDef] -> Stmt Resolved -> SemAnalyzer (Stmt Typed)
typeCheckStmt imps (StmtFunc (FnImpl (FuncImpl implLoc fnIdent pats expr))) = do
   (FuncSig argTypes retType) <- fromJust <$> lookupCurrentFunction fnIdent

   -- 0) Check expressions integer bounds
   checkIntegerBounds expr

   -- 1) match patterns and args count
   unless (length pats == length argTypes) $
      errSem (SEFnArityMismatch implLoc fnIdent (length argTypes) (length pats))

   -- 2) match patterns and args types
   inferred <- zipWithM (inferPattern imps) argTypes pats
   let (pats', varMaps) = unzip inferred
   let patVars = M.unions varMaps

   -- 3) withVars inferType of `expr`
   expr' <- withVars patVars (inferType imps expr)

   -- 4) compare types
   compareTypes implLoc retType (typeOf expr')

   pure $ StmtFunc (FnImpl (FuncImpl implLoc fnIdent pats' expr'))

typeCheckStmt imps (StmtSystem (SysImpl (SystemImpl implLoc sysIdent entPats mWith body))) = do
   (SystemSig _ sigEnts sigRet sigWith) <- fromJust <$> lookupCurrentSystem sysIdent
   -- 0) Check expressions integer bounds
   checkIntegerBounds body

   -- 1) match entity patterns and queried components count
   unless (length entPats == length sigEnts) $
      errSem (SESystemArityMismatch implLoc sysIdent (length sigEnts) (length entPats))

   entResults <- forM (zip entPats sigEnts) $ \(EntityPattern bindings, QueriedEntity tysLoc) -> do
      let tys = map (.node) tysLoc
      unless (length bindings == length tys) $
         errSem (SESystemArityMismatch implLoc sysIdent (length tys) (length bindings))
      inferred <- zipWithM
         (\ty (EntPatBinding pat access) -> do
            (pat', vars) <- inferPattern imps ty pat
            pure (EntPatBinding pat' access, vars))
         tys bindings
      let (bindings', varMaps) = unzip inferred
      pure (EntityPattern bindings', varMaps)
   let entPats' = map fst entResults
   let entVarMaps = concatMap snd entResults

   -- 2) match `with` patterns and `with` types count
   (mWith', withVarsList) <- case (mWith, sigWith) of
      (Nothing, _) -> pure (Nothing, [])
      (Just ps, Just wts) -> do
         unless (length ps == length wts) $
            errSem (SESystemArityMismatch implLoc sysIdent (length wts) (length ps))
         inferred <- zipWithM (inferPattern imps) (map withType wts) ps
         let (ps', varMaps) = unzip inferred
         pure (Just ps', varMaps)
      (Just ps, Nothing) -> do
         errSem (SESystemArityMismatch implLoc sysIdent 0 (length ps))
         inferred <- mapM (inferPattern imps TyInvalid) ps
         let (ps', varMaps) = unzip inferred
         pure (Just ps', varMaps)

   -- 3) withVars inferType of `body`
   body' <- withVars (M.unions (entVarMaps ++ withVarsList)) (inferType imps body)

   -- 4) compare types
   compareTypes implLoc sigRet (typeOf body')

   pure $ StmtSystem (SysImpl (SystemImpl implLoc sysIdent entPats' mWith' body'))

typeCheckStmt _ (StmtFunc (FnDef def)) = pure $ StmtFunc (FnDef def)

typeCheckStmt _ (StmtSystem (SysDef def)) = pure $ StmtSystem (SysDef def)

typeCheckStmt _ (StmtTypeDef td) = pure $ StmtTypeDef td

typeCheckStmt _ (StmtExtern ext) = pure $ StmtExtern ext

withType :: WithType -> Type
withType (WithEvent lt) = lt.node
withType (WithRes lt)   = lt.node

freshTyVar :: SemAnalyzer Type
freshTyVar = do
   st <- get
   let n = st.tyVarSupply
   put st { tyVarSupply = n + 1 }
   return $ TyVar n

freeTyVars :: Type -> S.Set Int
freeTyVars ty = go [ty] S.empty
   where
      go :: [Type] -> S.Set Int -> S.Set Int
      go [] acc = acc
      go (t:rest) acc = case t of
         TyVar n        -> go rest (S.insert n acc)
         TyApp a b      -> go (a : b : rest) acc
         TyTuple xs     -> go (xs ++ rest) acc
         TyFn args ret  -> go (ret : args ++ rest) acc
         _ -> go rest acc

generalize :: Type -> SemAnalyzer [Int]
generalize ty = do
   ty'  <- resolve ty
   vars <- asks (.vars)
   envFree <- fmap S.unions . forM (M.elems vars) $ \vi -> do
      t <- resolve vi.ty
      pure (freeTyVars t S.\\ S.fromList vi.quantified)

   return $ S.toList (freeTyVars ty' S.\\ envFree)

freshVarId :: SemAnalyzer VarId
freshVarId = do
   st <- get
   let n = st.varIdSupply
   put st { varIdSupply = n + 1 }
   return $ VarId n

freshLocalId :: SemAnalyzer LocalId
freshLocalId = do
   st <- get
   let n = st.localIdSupply
   put st { localIdSupply = n + 1 }
   return $ LocalId n

freshFunctionId :: SemAnalyzer FunctionId
freshFunctionId = do
   st <- get
   let n = st.fnIdSupply
   put st { fnIdSupply = n + 1 }
   return $ FunctionId n

freshConstructorId :: SemAnalyzer ConstructorId
freshConstructorId = do
   st <- get
   let n = st.ctorIdSupply
   put st { ctorIdSupply = n + 1 }
   return $ ConstructorId n

freshExternId :: SemAnalyzer ExternId
freshExternId = do
   st <- get
   let n = st.externIdSupply
   put st { externIdSupply = n + 1 }
   return $ ExternId n

-- | Assigns a fresh LocalId to every name, e.g. for a pattern's bound vars.
freshLocalScope :: [(Ident, Location)] -> SemAnalyzer (M.Map Ident (LocalId, Location))
freshLocalScope xs = M.fromList <$> mapM (\(x, loc) -> (\lid -> (x, (lid, loc))) <$> freshLocalId) xs

resolve :: Type -> SemAnalyzer Type
resolve t = do
   s <- gets (.tySubst)
   pure (go s t)
   where
      go s (TyVar n) = maybe (TyVar n) (go s) (M.lookup n s)
      go s (TyApp a b) = TyApp (go s a) (go s b)
      go s (TyTuple xs) = TyTuple (map (go s) xs)
      go s (TyFn args r) = TyFn (map (go s) args) (go s r)
      go _ t' = t'

bindVar :: Int -> Type -> SemAnalyzer ()
bindVar n t = modify $ \st -> st { tySubst = M.insert n t st.tySubst }

zonkType :: M.Map Int Type -> Type -> Type
zonkType s = go
   where
      go (TyVar n) = maybe (TyVar n) go (M.lookup n s)
      go (TyApp a b) = TyApp (go a) (go b)
      go (TyTuple xs) = TyTuple (map go xs)
      go (TyFn args r) = TyFn (map go args) (go r)
      go t = t

zonkTyped :: M.Map Int Type -> Typed -> Typed
zonkTyped s (TypedInfo loc ty res) = TypedInfo loc (zonkType s ty) res

zonkProgram :: M.Map Int Type -> Program Typed -> Program Typed
zonkProgram s = fmap (zonkTyped s)

unify :: Location -> Type -> Type -> SemAnalyzer ()
unify loc t1 t2 = do
   t1' <- resolve t1
   t2' <- resolve t2
   case (t1', t2') of
      (TyVar n, TyVar m) | n == m -> pure ()
      (TyVar n, _) -> bindOrFail loc n t2'
      (_, TyVar m) -> bindOrFail loc m t1'

      (TyCon a, TyCon b) | a == b -> pure ()
      (TyGnr a, TyGnr b) | a == b -> pure ()

      (TyApp a1 b1, TyApp a2 b2) -> unify loc a1 a2 >> unify loc b1 b2
      (TyTuple xs, TyTuple ys) | length xs == length ys ->
         zipWithM_ (unify loc) xs ys

      (TyFn args1 r1, TyFn args2 r2) | length args1 == length args2 -> do
         zipWithM_ (unify loc) args1 args2
         unify loc r1 r2

      (TyInvalid, _) -> pure ()
      (_, TyInvalid) -> pure ()

      _ -> errSem (SETypeError loc t1' t2')

bindOrFail :: Location -> Int -> Type -> SemAnalyzer ()
bindOrFail loc n t = do
   occ <- occursCheckType n t
   if occ then errSem (SEInfiniteType loc n t) else bindVar n t

occursCheckType :: Int -> Type -> SemAnalyzer Bool
occursCheckType n t = do
   t' <- resolve t
   case t' of
      TyVar m       -> pure (n == m)
      TyApp a b     -> (||) <$> occursCheckType n a <*> occursCheckType n b
      TyTuple xs    -> or <$> mapM (occursCheckType n) xs
      TyFn args ret -> ((||) . or <$> mapM (occursCheckType n) args) <*> occursCheckType n ret
      _             -> pure False

instantiate :: FuncSig -> SemAnalyzer Type
instantiate (FuncSig args ret) = do
   let gnrs = S.toList (foldMap collectGnrs args <> collectGnrs ret)
   fresh <- mapM (const freshTyVar) gnrs
   let subst = M.fromList (zip gnrs fresh)
   pure $ TyFn (map (substGnr subst) args) (substGnr subst ret)

collectGnrs :: Type -> S.Set Ident
collectGnrs = \case
   TyGnr i       -> S.singleton i
   TyApp a b     -> collectGnrs a <> collectGnrs b
   TyTuple xs    -> foldMap collectGnrs xs
   TyFn args r   -> foldMap collectGnrs args <> collectGnrs r
   _             -> S.empty

substGnr :: M.Map Ident Type -> Type -> Type
substGnr m = \case
   TyGnr i       -> M.findWithDefault (TyGnr i) i m
   TyApp a b     -> TyApp (substGnr m a) (substGnr m b)
   TyTuple xs    -> TyTuple (map (substGnr m) xs)
   TyFn args r   -> TyFn (map (substGnr m) args) (substGnr m r)
   t             -> t

substTyVars :: M.Map Int Type -> Type -> Type
substTyVars m = \case
   TyVar i       -> M.findWithDefault (TyVar i) i m
   TyApp a b     -> TyApp (substTyVars m a) (substTyVars m b)
   TyTuple xs    -> TyTuple (map (substTyVars m) xs)
   TyFn args r   -> TyFn (map (substTyVars m) args) (substTyVars m r)
   t             -> t

typeOf :: Expr Typed -> Type
typeOf = (.ty) . exprAnnotation

inferType
   :: [Located ImportDef]
   -> Expr Resolved
   -> SemAnalyzer (Expr Typed)
inferType imps expr = case expr of
   ExpLit (ResolvedInfo loc mRes) literal -> do
      ty <- literalType literal
      pure $ ExpLit (TypedInfo loc ty mRes) literal

   ExpVar (ResolvedInfo loc mRes) Nothing x -> do
      thisSym <- lookupLocal x
      modSym  <- lookupCurrentModule x
      impSym  <- lookupUnqualifiedSymbol imps x
      ty <- case thisSym of
         Just vi -> do
            t     <- resolve vi.ty
            fresh <- mapM (const freshTyVar) vi.quantified
            pure (substTyVars (M.fromList (zip vi.quantified fresh)) t)
         Nothing -> case modSym <|> impSym of
            Just (SymbolFn _ _ sig)       -> instantiate sig
            Just (SymbolExternFn _ _ sig) -> instantiate sig
            _ -> do
               errSem (SEUndefinedVar loc x)
               pure TyInvalid
      pure $ ExpVar (TypedInfo loc ty mRes) Nothing x

   ExpVar (ResolvedInfo loc mRes) (Just alias) x -> do
      sym <- lookupQualifiedSymbol imps alias x
      ty <- case sym of
         Just (SymbolFn _ _ sig)       -> instantiate sig
         Just (SymbolExternFn _ _ sig) -> instantiate sig
         _ -> do
            errSem (SEUndefinedVar loc x)
            pure TyInvalid
      pure $ ExpVar (TypedInfo loc ty mRes) (Just alias) x

   ExpCon (ResolvedInfo loc mRes) Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      ty <- case modCon <|> impCon of
         Just sym -> ctorType loc sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid
      pure $ ExpCon (TypedInfo loc ty mRes) Nothing x

   ExpCon (ResolvedInfo loc mRes) (Just alias) x -> do
      con <- lookupQualifiedConstructor imps alias x
      ty <- case con of
         Just sym -> ctorType loc sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid
      pure $ ExpCon (TypedInfo loc ty mRes) (Just alias) x

   ExpApp (ResolvedInfo loc mRes) applicant operand -> do
      applicant' <- inferType imps applicant
      operand'   <- inferType imps operand
      ty <- applyTypes loc (typeOf applicant') (typeOf operand')
      pure $ ExpApp (TypedInfo loc ty mRes) applicant' operand'

   ExpTuple (ResolvedInfo loc mRes) xs -> do
      xs' <- mapM (inferType imps) xs
      pure $ ExpTuple (TypedInfo loc (TyTuple (map typeOf xs')) mRes) xs'

   ExpList (ResolvedInfo loc mRes) [] -> do
      ty <- TyApp (TyCon (Ident "List")) <$> freshTyVar
      pure $ ExpList (TypedInfo loc ty mRes) []

   ExpList (ResolvedInfo loc mRes) (x:xs) -> do
      x' <- inferType imps x
      xs' <- mapM (inferType imps) xs
      -- Check inner list types
      forM_ xs' $ \other ->
         compareListElems 
            (exprAnnotation x').location (typeOf x')
            (exprAnnotation other).location (typeOf other)
      -- Type of list is defined as `List a`,
      -- where a is a type of the first element
      let ty = TyApp (TyCon (Ident "List")) (typeOf x')
      pure $ ExpList (TypedInfo loc ty mRes) (x':xs')

   ExpIfThen (ResolvedInfo loc mRes) (IfThenElse if' then' else') -> do
      -- Compare `if` type with Bool
      if'' <- inferType imps if'
      compareTypes (exprAnnotation if'').location (typeOf if'') boolType
      -- Compare `then` and `else` types
      then'' <- inferType imps then'
      else'' <- inferType imps else'
      compareThenElse (exprAnnotation then'').location (typeOf then'') (exprAnnotation else'').location (typeOf else'')
      -- Type of the whole expr is the type of the `then` block
      pure $ ExpIfThen (TypedInfo loc (typeOf then'') mRes) (IfThenElse if'' then'' else'')

   ExpLetIn (ResolvedInfo loc mRes) (LetIn binds body) -> do
      (binds', body') <- typeLetBinds imps binds body
      pure $ ExpLetIn (TypedInfo loc (typeOf body') mRes) (LetIn binds' body')

   ExpMatch (ResolvedInfo loc mRes) (Match mtExp mtMatches _) -> do
      mtExp' <- inferType imps mtExp
      wings <- forM mtMatches $ \(MatchWing pat branch) -> do
         (pat', patVars) <- inferPattern imps (typeOf mtExp') pat
         branch' <- withVars patVars (inferType imps branch)
         pure (MatchWing pat' branch', typeOf branch')
      ty <- case wings of
         [] -> pure TyInvalid
         ((_, t):rest) -> do
            forM_ rest (unify loc t . snd)
            resolve t
      scrutTy <- resolve (typeOf mtExp')
      isExhaustive <- checkMatchExhaustive loc scrutTy (map (patOf . fst) wings)
      pure $ ExpMatch (TypedInfo loc ty mRes) (Match mtExp' (map fst wings) isExhaustive)

   ExpLambda (ResolvedInfo loc mRes) (Lambda args body) -> do
      argTyVars <- forM args $ const freshTyVar
      inferred  <- zipWithM (inferPattern imps) argTyVars args
      let (args', argVarMaps) = unzip inferred
      let argVars = M.unions argVarMaps

      body' <- withVars argVars (inferType imps body)

      argTyVars' <- mapM resolve argTyVars
      let ty = TyFn argTyVars' (typeOf body')
      pure $ ExpLambda (TypedInfo loc ty mRes) (Lambda args' body')

   ExpRecConstruct (ResolvedInfo loc mRes) (RecConstruct rcBind rcCon rcAssigns) -> do
      sym <- case rcBind of
         Nothing -> do
            modCon <- lookupCurrentConstructor rcCon
            impCon <- lookupUnqualifiedConstructor imps rcCon
            pure (modCon <|> impCon)
         Just alias -> lookupQualifiedConstructor imps alias rcCon

      (ty, rcAssigns') <- case sym of
         Just s@(SymbolCtor _ _ (CtorSig _ _ mFieldNames _)) -> do
            resultTy <- ctorType loc s
            let (fieldTys, ctorResultTy) = case resultTy of
                  TyFn args r -> (args, r)
                  r           -> ([], r)
                fieldTypeOf fld = lookup fld (zip (fromMaybe [] mFieldNames) fieldTys)
            assigns' <- forM rcAssigns $ \(RecAssign fldName value) -> do
               value' <- inferType imps value
               case fieldTypeOf fldName.node of
                  Just expectedTy -> compareTypes loc expectedTy (typeOf value')
                  Nothing         -> errSem (SEUnknownField loc rcCon fldName.node)
               pure $ RecAssign fldName value'
            pure (ctorResultTy, assigns')

         _ -> do
            errSem (SEUndefinedCon loc rcCon)
            assigns' <- forM rcAssigns $ \(RecAssign fldName value) -> do
               value' <- inferType imps value
               pure $ RecAssign fldName value'
            pure (TyInvalid, assigns')

      pure $ ExpRecConstruct (TypedInfo loc ty mRes) (RecConstruct rcBind rcCon rcAssigns')

   ExpRecUpdate (ResolvedInfo loc mRes) (RecUpdate ruBase ruAssigns) -> do
      ruBase' <- inferType imps ruBase
      baseTy  <- resolve (typeOf ruBase')
      fields  <- recordFieldsOf imps loc baseTy

      ruAssigns' <- forM ruAssigns $ \(RecAssign fldName value) -> do
         value' <- inferType imps value
         case fields of
            Just (con, fieldNames, fieldTys) ->
               case lookup fldName.node (zip fieldNames fieldTys) of
                  Just expectedTy -> compareTypes loc expectedTy (typeOf value')
                  Nothing         -> errSem (SEUnknownField loc con fldName.node)
            Nothing -> pure ()
         pure $ RecAssign fldName value'

      pure $ ExpRecUpdate (TypedInfo loc baseTy mRes) (RecUpdate ruBase' ruAssigns')

   ExpVarGetter (ResolvedInfo loc mRes) baseExpr getter -> do
      baseExpr' <- inferType imps baseExpr
      baseTy    <- resolve (typeOf baseExpr')
      ty <- case getter of
         GetField fld -> do
            fields <- recordFieldsOf imps loc baseTy
            case fields of
               Just (con, fieldNames, fieldTys) ->
                  case lookup fld (zip fieldNames fieldTys) of
                     Just fldTy -> pure fldTy
                     Nothing    -> errSem (SEUnknownField loc con fld) >> pure TyInvalid
               Nothing -> pure TyInvalid  -- recordFieldsOf already reported why

         GetTupleField idx -> case baseTy of
            TyTuple tys | idx >= 0 && idx < length tys -> pure (tys !! idx)
            _ -> errSem (SEInvalidTupleIndex loc baseTy idx) >> pure TyInvalid

      pure $ ExpVarGetter (TypedInfo loc ty mRes) baseExpr' getter

recordFieldsOf :: [Located ImportDef] -> Location -> Type -> SemAnalyzer (Maybe (Ident, [Ident], [Type]))
recordFieldsOf imps loc ty = case typeHead ty of
   Nothing -> do
      errSem (SENotARecordType loc ty)
      pure Nothing
   Just conIdent -> do
      modCon <- lookupCurrentConstructor conIdent
      impCon <- lookupUnqualifiedConstructor imps conIdent
      case modCon <|> impCon of
         Just s@(SymbolCtor _ _ (CtorSig _ _ (Just fieldNames) _)) -> do
            modTy <- lookupCurrentConType conIdent
            impTy <- lookupUnqualifiedConType imps conIdent
            case modTy <|> impTy of
               Just (SymbolType _ (TypeSig ctors _ _)) | S.size ctors > 1 -> do
                  errSem (SEAmbiguousRecordAccess loc conIdent (S.toList (S.delete conIdent ctors)))
                  pure Nothing
               _ -> do
                  resultTy <- ctorType loc s
                  let (fieldTys, ctorResultTy) = case resultTy of
                        TyFn args r -> (args, r)
                        r           -> ([], r)
                  unify loc ctorResultTy ty
                  fieldTys' <- mapM resolve fieldTys
                  pure $ Just (conIdent, fieldNames, fieldTys')
         _ -> do
            errSem (SENotARecordType loc ty)
            pure Nothing

typeHead :: Type -> Maybe Ident
typeHead = \case
   TyCon n   -> Just n
   TyApp t _ -> typeHead t
   _         -> Nothing

patOf :: MatchWing a -> Pattern a
patOf (MatchWing pat _) = pat

checkMatchExhaustive :: Location -> Type -> [Pattern Typed] -> SemAnalyzer Bool
checkMatchExhaustive loc scrutTy pats = case typeHead scrutTy of
   Nothing -> pure False
   Just tyIdent -> do
      msym <- lookupCurrentModule tyIdent
      case msym of
         Just (SymbolType _ typeSig) | not typeSig.nonExhaustive ->
            if any isCatchAll pats
               then pure True
               else do
                  let covered = S.fromList [ctorIdent | PatCon _ ctorIdent _ <- pats]
                  let missing = S.toList (typeSig.ctors S.\\ covered)
                  if null missing
                     then pure True
                     else do
                        errSem (SENonExhaustiveMatch loc tyIdent missing)
                        pure False
         _ -> pure False
   where
      isCatchAll (PatVar _ _)    = True
      isCatchAll (PatWildcard _) = True
      isCatchAll _               = False

checkClausesExhaustive :: Location -> [Type] -> [[Pattern Typed]] -> SemAnalyzer Bool
checkClausesExhaustive loc paramTys clauseArgs = do
   results <- forM (zip paramTys (transpose clauseArgs)) $ uncurry (checkMatchExhaustive loc)
   pure (and results)

typeLetBinds
   :: [Located ImportDef]
   -> [Let Resolved]
   -> Expr Resolved
   -> SemAnalyzer ([Let Typed], Expr Typed)
typeLetBinds imps [] body = ([],) <$> inferType imps body
typeLetBinds imps (Let pat value : rest) body = do
   value'  <- inferType imps value
   (pat', patVars) <- inferPattern imps (typeOf value') pat
   patVars' <- case pat of
      PatVar {} -> forM patVars $ \vi -> do
         qs <- generalize vi.ty
         pure vi { quantified = qs }
      _ -> pure patVars

   (rest', body') <- withVars patVars' (typeLetBinds imps rest body)
   pure (Let pat' value' : rest', body')

inferPattern
   :: [Located ImportDef]
   -> Type
   -> Pattern Resolved
   -> SemAnalyzer (Pattern Typed, M.Map Ident VarInfo)
inferPattern imps ty p = case p of
   PatVar (ResolvedInfo loc mRes) x -> do
      vid <- freshVarId
      pure (PatVar (TypedInfo loc ty mRes) x, M.singleton x (VarInfo ty vid []))

   PatWildcard (ResolvedInfo loc mRes) ->
      pure (PatWildcard (TypedInfo loc ty mRes), M.empty)

   PatLit (ResolvedInfo loc mRes) lit -> do
      litTy <- literalType lit
      unify loc ty litTy
      pure (PatLit (TypedInfo loc ty mRes) lit, M.empty)

   PatList (ResolvedInfo loc mRes) ps -> do
      elemTv <- freshTyVar
      unify loc ty (TyApp (TyCon (Ident "List")) elemTv)
      inferred <- mapM (inferPattern imps elemTv) ps
      let (ps', varMaps) = unzip inferred
      pure (PatList (TypedInfo loc ty mRes) ps', M.unions varMaps)

   PatTuple (ResolvedInfo loc mRes) ps -> do
      elemTvs <- mapM (const freshTyVar) ps
      unify loc ty (TyTuple elemTvs)
      inferred <- zipWithM (inferPattern imps) elemTvs ps
      let (ps', varMaps) = unzip inferred
      pure (PatTuple (TypedInfo loc ty mRes) ps', M.unions varMaps)

   PatCon (ResolvedInfo loc mRes) ctorIdent ps -> do
      modCtor <- lookupCurrentConstructor ctorIdent
      impCtor <- lookupUnqualifiedConstructor imps ctorIdent
      case modCtor <|> impCtor of
         Nothing -> do
            inferred <- mapM (inferPattern imps TyInvalid) ps
            let ps' = map fst inferred
            pure (PatCon (TypedInfo loc TyInvalid mRes) ctorIdent ps', M.empty)

         Just sym@(SymbolCtor _ cid _) -> do
            let mRes' = Just (ResConstructor cid)
            resultTy <- ctorType loc sym
            let (expectedFieldTys, ctorResultTy) = case resultTy of
                  TyFn args r -> (args, r)
                  r           -> ([], r)

            unify loc ty ctorResultTy

            if length ps /= length expectedFieldTys
               then do
                  errSem (SECtorArityMismatch loc ctorIdent (length expectedFieldTys) (length ps))
                  inferred <- mapM (inferPattern imps TyInvalid) ps
                  let ps' = map fst inferred
                  pure (PatCon (TypedInfo loc ctorResultTy mRes') ctorIdent ps', M.empty)
               else do
                  inferred <- zipWithM (inferPattern imps) expectedFieldTys ps
                  let (ps', varMaps) = unzip inferred
                  pure (PatCon (TypedInfo loc ctorResultTy mRes') ctorIdent ps', M.unions varMaps)

         Just invalid ->
            unreachableWith $ "Invalid constructor symbol at " ++ show loc ++ ": " ++ show invalid

ctorType :: Location -> SymbolInfo -> SemAnalyzer Type
ctorType _ (SymbolCtor _ _ (CtorSig ownerIdent generics _ fieldTys)) = do
   fresh <- mapM (const freshTyVar) generics
   let subst    = M.fromList (zip generics fresh)
       fields'  = map (substGnr subst) fieldTys
       resultTy = foldl' TyApp (TyCon ownerIdent) fresh
   pure $ if null fields' then resultTy else TyFn fields' resultTy
ctorType loc invalid =
   unreachableWith $ "Invalid constructor symbol at " ++ show loc ++ ": " ++ show invalid

applyTypes :: Location -> Type -> Type -> SemAnalyzer Type
applyTypes loc applicantTy operandTy = do
   applicantTy' <- resolve applicantTy
   case applicantTy' of
      TyFn (argTy:restArgs) retTy -> do
         unify loc argTy operandTy
         if null restArgs
            then resolve retTy
            else resolve (TyFn restArgs retTy)

      TyFn [] _ -> do
         errSem (SETooManyArgs loc applicantTy')
         pure TyInvalid

      TyVar n -> do
         retTv <- freshTyVar
         occ <- occursCheckType n (TyFn [operandTy] retTv)
         if occ
            then errSem (SEInfiniteType loc n applicantTy') >> pure TyInvalid
            else bindVar n (TyFn [operandTy] retTv) >> pure retTv

      _ -> do
         errSem (SENotAFunction loc applicantTy')
         pure TyInvalid

-- | Unify `expected` with `current`, binding any unresolved type variables
-- on either side. `unify` reports its own (generic) `SETypeError` on
-- mismatch, so its output is suppressed here in favor of a plain type-error
-- message with a stable (expected, current) type ordering.
compareTypes :: Location -> Type -> Type -> SemAnalyzer ()
compareTypes loc expected current = do
   ((), reports) <- censor (const []) $ listen (unify loc expected current)
   when (any isSemError reports) $ do
      expected' <- resolve expected
      current' <- resolve current
      errSem (SETypeError loc expected' current')

-- | Like `compareTypes`, but unifies the two branch types and reports a
-- `then`/`else`-specific mismatch instead of a generic type error.
compareThenElse :: Location -> Type -> Location -> Type -> SemAnalyzer ()
compareThenElse thenLoc thenType elseLoc elseType = do
   ((), reports) <- censor (const []) $ listen (unify thenLoc thenType elseType)
   when (any isSemError reports) $ do
      thenType' <- resolve thenType
      elseType' <- resolve elseType
      errSem (SEThenElseTypeMismatch thenLoc thenType' elseLoc elseType')

-- | Unifies two list element types
compareListElems :: Location -> Type -> Location -> Type -> SemAnalyzer ()
compareListElems firstLoc firstTy otherLoc otherTy = do
   ((), reports) <- censor (const []) $ listen (unify firstLoc firstTy otherTy)
   when (any isSemError reports) $ do
      firstTy' <- resolve firstTy
      otherTy' <- resolve otherTy
      errSem (SEListElementTypeMismatch firstLoc firstTy' otherLoc otherTy')

isSemError :: SemReport -> Bool
isSemError (SemError _) = True
isSemError _            = False

literalType :: Literal -> SemAnalyzer Type
literalType (LitString _) = pure $ TyCon (Ident "String")
literalType (LitChar _) = pure $ TyCon (Ident "Char")
literalType (LitInt lit) = pure $ TyCon (Ident (T.pack (show lit.kind)))
literalType (LitFloat _) = pure $ TyCon (Ident "Float")
literalType (LitDouble _) = pure $ TyCon (Ident "Double")
literalType (LitTuple xs) = TyTuple <$> mapM (literalType . (.node)) xs
literalType (LitList []) = genericList
literalType (LitList (x:xs)) = do
   -- Check inner list types
   checkLitListType x xs
   -- Type of list is defined as `List a`, 
   -- where a is a type of the first element
   firstElemType <- literalType x.node
   return $ TyApp (TyCon (Ident "List")) firstElemType

checkLitListType :: Located Literal -> [Located Literal] -> SemAnalyzer ()
checkLitListType (Located firstLoc firstElem) others =
   forM_ others $ \(Located otherLoc otherElem) -> do
      firstType <- literalType firstElem
      otherType <- literalType otherElem
      unless (firstType == otherType) $
         errSem (SEListElementTypeMismatch firstLoc firstType otherLoc otherType)

genericList :: SemAnalyzer Type
genericList = TyApp (TyCon (Ident "List")) <$> freshTyVar

boolType :: Type
boolType = TyCon (Ident "Bool")

-- #### Desugaring ####

desugarProgram :: Program Typed -> SemAnalyzer (Program Desugared)
desugarProgram prog = do
   let notImpl = [desugaredAnn <$> x | x <- prog.stmts, isNotImpl x]

   -- Functions
   let fmImpls = [x | StmtFunc (FnImpl x) <- prog.stmts]
   let fnGroups =
         groupBy ((==) `on` (.name))
            $ sortOn (.name) fmImpls
   desugaredFns <- forM fnGroups desugarFn

   -- Systems
   let sysImpls = [x | StmtSystem (SysImpl x) <- prog.stmts]
   let sysGroups =
         groupBy ((==) `on` (.name))
            $ sortOn (.name) sysImpls
   desugaredSystems <- forM sysGroups desugarSys

   -- Combine stmts
   let combinedStmts = notImpl
         <> fmap (StmtFunc . FnImpl) desugaredFns
         <> fmap (StmtSystem . SysImpl) desugaredSystems

   return $ Program
      prog.moduleDef
      prog.imports
      combinedStmts
      prog.src

desugarFn :: [FuncImpl Typed] -> SemAnalyzer (FuncImpl Desugared)
desugarFn group = do
   let fstImpl = head group
   let argTypes = map ((.ty) . patAnnotation) fstImpl.args

   isExhaustive <- checkClausesExhaustive fstImpl.location argTypes (map (.args) group)

   -- Tuple patterns and bodies
   let tupleAnn = DesugaredInfo {res = Nothing, ty = TyTuple argTypes}
   let patBodies = flip map group $ \i ->
         ( PatTuple tupleAnn $ map (fmap desugaredAnn) i.args
         , fmap desugaredAnn i.body
         )

   -- Define args names
   let argsCnt = length fstImpl.args
   args <- forM [0..argsCnt] $ const freshParam
   let (ids, names) = unzip args
   let typedArgs = zip3 ids names argTypes

   -- Base expression
   let baseExpr = ExpTuple tupleAnn $ flip map typedArgs $ \(lid, name, ty) ->
         ExpVar DesugaredInfo {res = Just (ResLocal lid), ty = ty} Nothing name

   -- Generate match
   let matchExpr = Match baseExpr (map (uncurry MatchWing) patBodies) isExhaustive
   let retType = (exprAnnotation (snd (head patBodies))).ty

   -- Construct new func impl
   return $ FuncImpl
      fstImpl.location
      fstImpl.name
      (map fst patBodies)
      (ExpMatch DesugaredInfo {res = Nothing, ty = retType} matchExpr)

desugarSys :: [SystemImpl Typed] -> SemAnalyzer (SystemImpl Desugared)
desugarSys group = do
   let fstImpl = head group

   -- Flatten every entity's component patterns
   let flattenClause i =
         concatMap (\(EntityPattern bs) -> map (.pat) bs) i.entities
            ++ fromMaybe [] i.with

   let entityTypes = concatMap (\(EntityPattern bs) -> map ((.ty) . patAnnotation . (.pat)) bs) fstImpl.entities
   let withTypes = maybe [] (map ((.ty) . patAnnotation)) fstImpl.with
   let argTypes = entityTypes ++ withTypes

   isExhaustive <- checkClausesExhaustive fstImpl.location argTypes (map flattenClause group)

   -- Tuple patterns and bodies
   let tupleAnn = DesugaredInfo {res = Nothing, ty = TyTuple argTypes}
   let patBodies = flip map group $ \i ->
         ( PatTuple tupleAnn $ map (fmap desugaredAnn) (flattenClause i)
         , fmap desugaredAnn i.body
         )

   -- Define param names, one per flattened position
   args <- forM argTypes $ const freshParam
   let typedArgs = zipWith (\(lid, name) ty -> (lid, name, ty)) args argTypes

   -- Base expression
   let baseExpr = ExpTuple tupleAnn $ flip map typedArgs $ \(lid, name, ty) ->
         ExpVar DesugaredInfo {res = Just (ResLocal lid), ty = ty} Nothing name

   -- Generate match
   let matchExpr = Match baseExpr (map (uncurry MatchWing) patBodies) isExhaustive
   let retType = (exprAnnotation (snd (head patBodies))).ty

   -- Rebuild the entity/with shape from `fstImpl`, substituting each
   -- component's pattern with its freshly named param.
   let (entityArgs, withArgs) = splitAt (length entityTypes) typedArgs

   let mkVar (lid, name, ty) = PatVar (DesugaredInfo {res = Just (ResLocal lid), ty = ty}) name

   let rebuildEntities _ [] = []
       rebuildEntities avail (EntityPattern bindings : rest) =
         let (used, remaining) = splitAt (length bindings) avail
             newBindings = zipWith
                  (\arg binding -> EntPatBinding (mkVar arg) binding.access)
                  used bindings
         in EntityPattern newBindings : rebuildEntities remaining rest

   let newEntities = rebuildEntities entityArgs fstImpl.entities
   let newWith = fmap (const (map mkVar withArgs)) fstImpl.with

   -- Construct new system impl
   return $ SystemImpl
      fstImpl.location
      fstImpl.name
      newEntities
      newWith
      (ExpMatch DesugaredInfo {res = Nothing, ty = retType} matchExpr)

freshParam :: SemAnalyzer (LocalId, Ident)
freshParam = do
   lid@(LocalId n) <- freshLocalId
   pure (lid, Ident ("p_" <> T.pack (show n)))

isNotImpl :: Stmt a -> Bool
isNotImpl (StmtFunc (FnImpl _)) = False
isNotImpl (StmtSystem (SysImpl _)) = False
isNotImpl _ = True