{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Pass where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.State
import Control.Monad (forM_, forM)
import qualified Data.Set as S
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.Utils.Compiler (Target)
import Xast.SemAnalyzer.ResolveNames (resolveNames)
import Xast.SemAnalyzer.TypeUsages (resolveTypeUsages)
import Xast.SemAnalyzer.Declare (declareStmts)
import Xast.SemAnalyzer.Imports (importAnalysis)
import Xast.SemAnalyzer.TypeCheck (typeCheck)
import Xast.SemAnalyzer.Desugar (desugarProgram)
import Xast.SemAnalyzer.Unify (zonkProgram)

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
