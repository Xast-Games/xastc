{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Imports where

import Control.Monad.State
import Control.Monad (forM_, unless, when, foldM)
import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.Utils.List (allEqual, pairs)
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.Utils.Generic ((|>))

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

   let addMany importMap ident loc = M.insertWith S.union ident (S.singleton loc) importMap
   imported <- foldM
      (\accumulator (Located loc (ImportDef moduleName payload)) ->
         case payload of
            ImpAlias _ -> pure accumulator
            ImpSelect identifiers ->
               case M.lookup moduleName modules of
                  Just _ -> do
                     exports <- getModuleExports moduleName
                     let names = [ identifier.node | identifier <- identifiers, identifier.node `S.member` exports ]
                     pure (foldl' (\importMap name -> addMany importMap name loc) accumulator names)
                  Nothing ->
                     let names = map (.node) identifiers
                     in pure (foldl' (\importMap name -> addMany importMap name loc) accumulator names)
            ImpFull ->
               case M.lookup moduleName modules of
                  Just _ -> do
                     exports <- getModuleExports moduleName
                     let names = S.toList exports
                     pure (foldl' (\importMap name -> addMany importMap name loc) accumulator names)
                  Nothing -> pure accumulator
      )
      M.empty
      imports
   forM_ (M.toList imported) $ \(ident, locs) ->
      let sorted = sortBy sortLocByPos (S.toList locs)
      in case sorted of
         _firstLoc:_secondLoc:_ ->
            forM_ (pairs sorted) $
               \(loc1, loc2) -> errSem (SEAmbiguousImport ident loc1 loc2)
         _ ->
            pure ()

resolveImportDeclConflicts :: Program Parsed -> SemAnalyzer ()
resolveImportDeclConflicts (Program (ModuleDef _ currentModuleName _) imports _ _) = do
   modules <- gets (.modules)
   let addMany importMap ident loc = M.insertWith S.union ident (S.singleton loc) importMap
   imported <- foldM
      (\accumulator (Located loc (ImportDef importedModule payload)) ->
         case payload of
            ImpAlias _ -> pure accumulator
            ImpSelect identifiers ->
               case M.lookup importedModule modules of
                  Just _ -> do
                     exports <- getModuleExports importedModule
                     let names = [ identifier.node | identifier <- identifiers, identifier.node `S.member` exports ]
                     pure (foldl' (\importMap name -> addMany importMap name loc) accumulator names)
                  Nothing ->
                     let names = map (.node) identifiers
                     in pure (foldl' (\importMap name -> addMany importMap name loc) accumulator names)
            ImpFull ->
               case M.lookup importedModule modules of
                  Just _ -> do
                     exports <- getModuleExports importedModule
                     let names = S.toList exports
                     pure (foldl' (\importMap name -> addMany importMap name loc) accumulator names)
                  Nothing -> pure accumulator
      )
      M.empty
      imports

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
                        ]
                  let private =
                        [ node
                        | node <- nodes
                        , node `M.member` moduleData
                        , node `S.notMember` exports
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

getModuleExports :: Module -> SemAnalyzer (S.Set Ident)
getModuleExports moduleName = gets $ \symTable ->
   symTable
      |> (.modules)
      |> M.lookup moduleName
      |> fromJust
      |> (.exports)

setModuleExports :: Module -> S.Set Ident -> SemAnalyzer ()
setModuleExports moduleName exports =
   modify $ \symTable ->
      let moduleInfo = M.findWithDefault emptyModuleInfo moduleName symTable.modules
      in symTable
         { modules = M.insert moduleName (moduleInfo { exports = exports }) symTable.modules
         }

resolveInvalidExports :: Program Parsed -> SemAnalyzer ()
resolveInvalidExports (Program (ModuleDef _ moduleName (Located loc exportList)) _ _ _) =
   case exportList of
      ExpSelect identifiers -> do
         moduleData <- getModuleSymbols moduleName

         let invalid = filter (`M.notMember` moduleData) identifiers
         case invalid of
            [] -> setModuleExports moduleName (S.fromList identifiers)
            invalidExports -> errSem (SEInvalidExport moduleName loc invalidExports)

      ExpFull -> do
         symbols <- M.keys <$> getModuleSymbols moduleName
         setModuleExports moduleName (S.fromList symbols)

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
