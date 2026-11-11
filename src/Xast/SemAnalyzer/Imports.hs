{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Imports where

import Control.Monad.State
import Control.Monad (forM_, unless, when)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.Utils.List (allEqual, pairs)
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.Utils.Generic ((|>))
import qualified Xast.Utils.List as L
import Xast.SemAnalyzer.Query (exportOrigin, importBringsIn, isTypeSymbol)
import Data.Graph (stronglyConnComp, flattenSCCs)
import Data.Maybe (mapMaybe, listToMaybe)

-- #### RESOLVE IMPORTS ####

importAnalysis :: [Program Parsed] -> SemAnalyzer ()
importAnalysis progs = do
   -- Resolve A imports A
   forM_ progs resolveSelfImport

   -- Resolve A imports B -> B imports A
   resolveCyclicImports progs

   -- Resolve A imports from B multiple times
   forM_ progs resolveRedundantImports

   -- Initialize exports and resolve missing exported symbols.
   -- Dependencies go first, so re-exports can see their exports
   forM_ (inImportOrder progs) resolveInvalidExports

   -- Resolve A imports private/missing symbols from B
   -- or the module B is missing itself
   resolveMissing progs

   -- 1) Resolve A and B imports the same symbol x
   -- 2) Resolve import both A and B as M
   forM_ progs resolveAmbiguity

   -- Resolve import x from A vs local declaration x
   forM_ progs resolveImportDeclConflicts

resolveAmbiguity :: Program Parsed -> SemAnalyzer ()
resolveAmbiguity (Program _ imports _ _) = do
   modules <- gets (.modules)
   let aliasPairs =
         [ (alias, loc)
         | Located loc (ImportDef _ (ImpAlias (Located _ alias))) <- imports
         ]
   let aliasMap = M.fromListWith (++) [ (alias, [loc]) | (alias, loc) <- aliasPairs ]

   forM_ (M.toList aliasMap) $ \(alias, locs) ->
      let sorted = sortBy sortLocByPos locs
      in case sorted of
         _firstLoc:_secondLoc:_ ->
            forM_ (pairs sorted) $
               \(loc1, loc2) -> errSem (SEAmbiguousAlias alias loc1 loc2)
         _ ->
            pure ()

   -- Ambiguous only if the imports bring in different symbols
   let importedByName = imports
         |> concatMap (importedNames modules)
         |> map (\imported -> (imported.name, [imported]))
         |> M.fromListWith (++)
   forM_ (M.toList importedByName) $ \(ident, sources) ->
      forM_ (importClashes sources) $ \(loc1, loc2) ->
         errSem (SEAmbiguousImport ident loc1 loc2)

-- | Pairs of imports of one name that bring in different symbols, e.g. @map@
--   from both @use Std.Option *@ and @use Std.Result *@, in source order.
importClashes :: [ImportedName] -> [(Location, Location)]
importClashes sources =
   let sorted = sortBy (\first second -> sortLocByPos first.location second.location) sources
   in [ (first.location, second.location)
      | (first, second) <- pairs sorted
      , first.declaredIn /= second.declaredIn
      ]

-- | A name brought in by an unaliased import.
data ImportedName = ImportedName
   { name       :: Ident
   , declaredIn :: Module    -- ^ its original module, even if re-exported
   , location   :: Location  -- ^ the import
   }

-- | Names brought in by an unaliased import. For an unknown module (reported
--   elsewhere), each name counts as declared there.
importedNames :: M.Map Module ModuleInfo -> Located ImportDef -> [ImportedName]
importedNames modules (Located location (ImportDef moduleName payload)) =
   let imported name declaredIn = ImportedName
         { name       = name
         , declaredIn = declaredIn
         , location   = location
         }
       exported = maybe M.empty (.exports) (M.lookup moduleName modules)
   in case payload of
      ImpAlias _ -> []
      ImpSelect identifiers
         | M.member moduleName modules ->
            [ imported identifier.node declaredIn
            | identifier <- identifiers
            , Just declaredIn <- [M.lookup identifier.node exported]
            ]
         | otherwise ->
            [ imported identifier.node moduleName | identifier <- identifiers ]
      ImpFull ->
         [ imported name declaredIn | (name, declaredIn) <- M.toList exported ]

resolveImportDeclConflicts :: Program Parsed -> SemAnalyzer ()
resolveImportDeclConflicts (Program (ModuleDef _ currentModuleName _) imports _ _) = do
   modules <- gets (.modules)
   let imported = imports
         |> concatMap (importedNames modules)
         |> map (\importedName -> (importedName.name, S.singleton importedName.location))
         |> M.fromListWith S.union

   moduleData <- getModuleSymbols currentModuleName
   forM_ (M.toList imported) $ \(ident, importLocs) ->
      case M.lookup ident moduleData of
         Just symbol ->
            let declLoc = symbolLoc symbol
                sorted = sortBy sortLocByPos (S.toList importLocs)
            in case sorted of
               impLoc:_ -> errSem (SEImportDeclConflict ident impLoc declLoc)
               _ -> pure ()
         Nothing -> pure ()

resolveMissing :: [Program Parsed] -> SemAnalyzer ()
resolveMissing progs = do
   modules <- gets (.modules)
   forM_ progs $ \(Program _ imports _ _) ->
      forM_ imports $ \(Located loc (ImportDef moduleName payload)) ->
         if M.member moduleName modules then
            case payload of
               ImpSelect identifiers -> do
                  moduleData <- getModuleSymbols moduleName
                  exports <- getModuleExports moduleName

                  let nodes = map (.node) identifiers
                  let missing =
                        [ node
                        | node <- nodes
                        , node `M.notMember` moduleData
                        , node `M.notMember` exports
                        ]
                  let private =
                        [ node
                        | node <- nodes
                        , node `M.member` moduleData
                        , node `M.notMember` exports
                        ]

                  unless (null missing) $
                     errSem (SEMissingImports moduleName loc missing)

                  unless (null private) $
                     errSem (SEPrivateImports moduleName loc private)
               _ ->
                  pure ()
         else
            errSem (SEMissingModule moduleName loc)


getModuleSymbols :: Module -> SemAnalyzer (M.Map Ident SymbolInfo)
getModuleSymbols moduleName = gets $ \symTable ->
   symTable
      |> (.modules)
      |> M.lookup moduleName
      |> fromJust
      |> (.symbols)

getModuleExports :: Module -> SemAnalyzer (M.Map Ident Module)
getModuleExports moduleName = gets $ \symTable ->
   symTable
      |> (.modules)
      |> M.lookup moduleName
      |> fromJust
      |> (.exports)

setModuleExports :: Module -> ResolvedExport -> SemAnalyzer ()
setModuleExports moduleName resolved =
   modify $ \symTable ->
      let moduleInfo = M.findWithDefault emptyModuleInfo moduleName symTable.modules
          withExports = moduleInfo
            { exports        = resolved.names
            , abstractTypes  = resolved.abstract
            , exportedFields = resolved.fields
            }
      in symTable { modules = M.insert moduleName withExports symTable.modules }

-- | What one export list, or one entry of it, makes visible.
data ResolvedExport = ResolvedExport
   { names    :: M.Map Ident Module                -- ^ exported name → declaring module
   , abstract :: S.Set TypeName                    -- ^ types exported without their same-named constructor
   , fields   :: M.Map TypeName (S.Set FieldName)  -- ^ fields of own types visible to other modules
   }

instance Semigroup ResolvedExport where
   (<>) :: ResolvedExport -> ResolvedExport -> ResolvedExport
   first <> second = ResolvedExport
      { names    = M.union first.names second.names
      , abstract = S.union first.abstract second.abstract
      , fields   = M.unionWith S.union first.fields second.fields
      }

instance Monoid ResolvedExport where
   mempty :: ResolvedExport
   mempty = ResolvedExport { names = M.empty, abstract = S.empty, fields = M.empty }

-- | What an exporting module needs to resolve its export list.
data ExportContext = ExportContext
   { knownModules :: M.Map Module ModuleInfo
   , moduleName :: Module                  -- ^ the exporting module
   , ownSymbols :: M.Map Ident SymbolInfo
   , imports    :: [Located ImportDef]
   , location   :: Location                -- ^ of the export list
   }

-- | Sets a module's exports. @exports { .. }@ may list own or imported names;
--   @exports *@ exports only own names, with every constructor and field.
resolveInvalidExports :: Program Parsed -> SemAnalyzer ()
resolveInvalidExports (Program (ModuleDef _ moduleName (Located loc exportList)) imports _ _) = do
   ownSymbols <- getModuleSymbols moduleName
   modules <- gets (.modules)
   case exportList of
      ExpSelect items -> do
         let context = ExportContext
               { knownModules = modules
               , moduleName = moduleName
               , ownSymbols = ownSymbols
               , imports    = imports
               , location   = loc
               }
         resolved <- mapM (resolveExportItem context) items
         let invalid = [ name | Left name <- resolved ]
         setModuleExports moduleName (mconcat [ export | Right export <- resolved ])
         unless (null invalid) $
            errSem (SEInvalidExport moduleName loc invalid)

      ExpFull -> setModuleExports moduleName ResolvedExport
         { names    = M.map (const moduleName) ownSymbols
         , abstract = S.empty
         , fields   = M.map S.fromList (typeFields ownSymbols)
         }

-- | One export entry, or its name if the module has nothing by that name.
resolveExportItem :: ExportContext -> ExportItem -> SemAnalyzer (Either Ident ResolvedExport)
resolveExportItem context item =
   case exportedFrom context.knownModules context.moduleName context.ownSymbols context.imports item.name of
      Nothing -> pure (Left item.name)
      Just origin -> do
         let originSymbols = maybe M.empty (.symbols) (M.lookup origin context.knownModules)
         Right <$> case (M.lookup item.name originSymbols, item.members) of
            (Just (SymbolCtor _ _ ctorSignature), _) -> do
               errSem (SEExportedCtorAlone context.location item.name ctorSignature.owner.name)
               pure mempty
            (Just symbol, NoMembers) ->
               pure (exportTypeWith context origin symbol item.name [] [])
            (Just symbol, AllMembers) -> do
               let (ctors, fieldNames) = typeMembers originSymbols symbol
               pure (exportTypeWith context origin symbol item.name ctors fieldNames)
            (Just symbol, SomeMembers listed) -> do
               let (ctors, fieldNames) = typeMembers originSymbols symbol
               let unknown = filter (\member -> member `notElem` ctors && member `notElem` fieldNames) listed
               unless (null unknown) $
                  errSem (SEInvalidExportMembers context.location item.name unknown)
               let listedCtors = filter (`elem` ctors) listed
               let listedFields = filter (`elem` fieldNames) listed
               pure (exportTypeWith context origin symbol item.name listedCtors listedFields)
            (Nothing, _) -> pure mempty

-- | A type (or a function) with the constructors and fields chosen for export.
--   Constructors of an imported type are exported only if the imports bring them in.
exportTypeWith
   :: ExportContext
   -> Module       -- ^ where the name is declared
   -> SymbolInfo
   -> Ident        -- ^ the exported name
   -> [CtorName]   -- ^ chosen constructors
   -> [FieldName]  -- ^ chosen fields
   -> ResolvedExport
exportTypeWith context origin symbol name ctors fieldNames =
   let visibleCtors = ctors
         |> filter (ctorVisibleHere context origin name)
       ctorEntries = M.fromList [ (ctor, origin) | ctor <- visibleCtors ]
       sharesNameWithCtor = case symbol of
         SymbolTypeCtor {} -> True
         _                 -> False
       isAbstract = sharesNameWithCtor && name `notElem` visibleCtors
       ownFields
         | origin == context.moduleName && not (null fieldNames) = M.singleton name (S.fromList fieldNames)
         | otherwise = M.empty
   in ResolvedExport
      { names    = M.insert name origin ctorEntries
      , abstract = if isAbstract then S.singleton name else S.empty
      , fields   = ownFields
      }

-- | Whether this module can see a constructor, so that it may export it.
ctorVisibleHere :: ExportContext -> Module -> TypeName -> CtorName -> Bool
ctorVisibleHere context origin typeName ctor
   | origin == context.moduleName = True
   | ctor == typeName =
      -- the constructor that shares its type's name comes with the type, unless abstract
      context.imports
         |> map (.node)
         |> filter (\importDef -> importBringsIn context.knownModules importDef typeName)
         |> any (\importDef -> not (abstractIn context.knownModules importDef.importModule typeName))
   | otherwise =
      exportedFrom context.knownModules context.moduleName context.ownSymbols context.imports ctor == Just origin

abstractIn :: M.Map Module ModuleInfo -> Module -> TypeName -> Bool
abstractIn modules module' typeName = case M.lookup module' modules of
   Just moduleInfo -> typeName `S.member` moduleInfo.abstractTypes
   Nothing         -> False

-- | The constructors and record fields of a type; nothing for other symbols.
typeMembers :: M.Map Ident SymbolInfo -> SymbolInfo -> ([CtorName], [FieldName])
typeMembers symbols = \case
   SymbolType _ typeSignature -> membersOf symbols (S.toList typeSignature.ctors)
   SymbolTypeCtor _ typeSignature _ _ -> membersOf symbols (S.toList typeSignature.ctors)
   _ -> ([], [])

membersOf :: M.Map Ident SymbolInfo -> [CtorName] -> ([CtorName], [FieldName])
membersOf symbols ctors =
   let fieldNames = L.forFlat ctors $ \ctor -> case M.lookup ctor symbols of
         Just (SymbolCtor _ _ ctorSignature)       -> fromMaybe [] ctorSignature.fieldNames
         Just (SymbolTypeCtor _ _ _ ctorSignature) -> fromMaybe [] ctorSignature.fieldNames
         _                                         -> []
   in (ctors, fieldNames)

-- | The record fields of every type a module declares.
typeFields :: M.Map Ident SymbolInfo -> M.Map TypeName [FieldName]
typeFields symbols = M.fromList
   [ (name, snd (typeMembers symbols symbol))
   | (name, symbol) <- M.toList symbols
   , isTypeSymbol symbol
   ]

-- | Where an exported name is declared: here, or where it is re-exported from.
exportedFrom
   :: M.Map Module ModuleInfo
   -> Module                    -- ^ the exporting module
   -> M.Map Ident SymbolInfo    -- ^ its own symbols
   -> [Located ImportDef]       -- ^ its imports
   -> Ident
   -> Maybe Module
exportedFrom modules moduleName moduleData imports identifier
   | M.member identifier moduleData = Just moduleName
   | otherwise = imports
      |> map (.node)
      |> mapMaybe (reexportedFrom modules identifier)
      |> listToMaybe

-- | Where a name brought in by this import is declared.
reexportedFrom :: M.Map Module ModuleInfo -> Ident -> ImportDef -> Maybe Module
reexportedFrom modules identifier importDef
   | importBringsIn modules importDef identifier = exportOrigin modules importDef.importModule identifier
   | otherwise                                   = Nothing

-- | Programs sorted so that dependencies come first, e.g. for
--
--   > module Main  -- use Std.Prelude *
--   > module Std.Prelude  -- use Std.Option *
--
--   the order is @Std.Option@, @Std.Prelude@, @Main@.
--   Modules in an import cycle (reported separately) keep any order.
inImportOrder :: [Program Parsed] -> [Program Parsed]
inImportOrder programs =
   let node program = (program, fst (getModuleName program), getImports program)
   in programs
      |> map node
      |> stronglyConnComp
      |> flattenSCCs

resolveRedundantImports :: Program Parsed -> SemAnalyzer ()
resolveRedundantImports (Program _ imports _ _) =
   when (length imports >= 2) $ do
      forM_ (pairs imports) $ \importPair -> case uncurry intersectImport importPair of
         Just redundant -> warnSem (SWRedundantImport redundant)
         Nothing        -> return ()

resolveCyclicImports :: [Program Parsed] -> SemAnalyzer ()
resolveCyclicImports progs = do
   let moduleMap = M.fromList [(fst (getModuleName program), getImports program) | program <- progs]
   let moduleLocations = M.fromList [getModuleName program | program <- progs]

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
         cycle' -> unless (allEqual cycle') $
            errSem (SECyclicImportError cycle' loc)
   | otherwise =
      case M.lookup current moduleMap of
         Nothing -> return ()
         Just imports ->
            forM_ imports $ \importedModule ->
               forM_ (M.lookup importedModule moduleLocations)
                  ( detectCycle
                     moduleMap
                     moduleLocations
                     (S.insert current visited)
                     (path ++ [importedModule]) importedModule
                  )

resolveSelfImport :: Program Parsed -> SemAnalyzer ()
resolveSelfImport (Program (ModuleDef from this _) imports _ _) =
   case filter (\(Located _ (ImportDef imported _)) -> imported == this) imports of
      (Located to _):_ -> errSem (SESelfImportError this from to)
      [] -> return ()
