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
   TyCon name   -> Just name
   TyApp func _ -> typeHead func
   _            -> Nothing

patOf :: MatchWing a -> Pattern a
patOf (MatchWing pat _) = pat

checkMatchExhaustive :: Location -> Type -> [Pattern Typed] -> SemAnalyzer Bool
checkMatchExhaustive location scrutineeType patterns = case typeHead scrutineeType of
   Nothing -> pure False
   Just typeName -> do
      maybeSymbol <- lookupCurrentModule typeName
      case maybeSymbol of
         Just (SymbolType _ typeSig) | not typeSig.nonExhaustive ->
            if any isCatchAll patterns
               then pure True
               else do
                  let covered = S.fromList [ctorIdent | PatCon _ ctorIdent _ <- patterns]
                  let missing = S.toList (typeSig.ctors S.\\ covered)
                  if null missing
                     then pure True
                     else do
                        errSem (SENonExhaustiveMatch location typeName missing)
                        pure False
         _ -> pure False
   where
      isCatchAll (PatVar _ _)    = True
      isCatchAll (PatWildcard _) = True
      isCatchAll _               = False

checkClausesExhaustive :: Location -> [Type] -> [[Pattern Typed]] -> SemAnalyzer Bool
checkClausesExhaustive location parameterTypes clauseArgs = do
   results <- forM (zip parameterTypes (transpose clauseArgs)) $ uncurry (checkMatchExhaustive location)
   pure (and results)
