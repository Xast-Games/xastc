{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.Exhaustive where

import Control.Monad (forM)
import Data.List (transpose)
import qualified Data.Set as S
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query

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
