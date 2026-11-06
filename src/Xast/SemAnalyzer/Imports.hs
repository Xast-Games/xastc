{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Imports where

import Control.Monad.State
import Control.Monad (forM_, unless, when)
import Data.Maybe (fromJust)
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
import Xast.SemAnalyzer.Query (exportOrigin, importBringsIn)
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
      let sorted = sortBy (\first second -> sortLocByPos first.location second.location) sources
          clashes =
            [ (first.location, second.location)
            | (first, second) <- pairs sorted
            , first.declaredIn /= second.declaredIn
            ]
      in forM_ clashes $ \(loc1, loc2) -> errSem (SEAmbiguousImport ident loc1 loc2)

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
   let imported name declaredIn = ImportedName { name = name, declaredIn = declaredIn, location = location }
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

setModuleExports :: Module -> M.Map Ident Module -> SemAnalyzer ()
setModuleExports moduleName exports =
   modify $ \symTable ->
      let moduleInfo = M.findWithDefault emptyModuleInfo moduleName symTable.modules
      in symTable
         { modules = M.insert moduleName (moduleInfo { exports = exports }) symTable.modules
         }

-- | Sets a module's exports. @exports { .. }@ may list own or imported names;
--   @exports *@ exports only own names.
resolveInvalidExports :: Program Parsed -> SemAnalyzer ()
resolveInvalidExports (Program (ModuleDef _ moduleName (Located loc exportList)) imports _ _) =
   case exportList of
      ExpSelect identifiers -> do
         moduleData <- getModuleSymbols moduleName
         modules <- gets (.modules)

         let origins = L.forEach identifiers $ \identifier ->
               (identifier, exportedFrom modules moduleName moduleData imports identifier)
         let invalid = [ identifier | (identifier, Nothing) <- origins ]
         let valid = M.fromList [ (identifier, origin) | (identifier, Just origin) <- origins ]

         setModuleExports moduleName valid
         unless (null invalid) $
            errSem (SEInvalidExport moduleName loc invalid)

      ExpFull -> do
         symbols <- M.keys <$> getModuleSymbols moduleName
         let ownExports = symbols |> map (\symbol -> (symbol, moduleName))
         setModuleExports moduleName (M.fromList ownExports)

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

-- | Programs sorted so that dependencies come first.
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
