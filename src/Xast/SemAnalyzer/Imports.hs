{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Imports where

import Control.Monad.State
import Control.Monad (forM_, unless, when, foldM)
import Data.Maybe (mapMaybe, fromJust)
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
