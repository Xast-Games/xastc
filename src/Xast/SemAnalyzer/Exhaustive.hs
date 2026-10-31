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

typeHead :: Type -> Maybe QualifiedIdent
typeHead = \case
   TyCon qualified -> Just qualified
   TyApp func _ -> typeHead func
   _            -> Nothing

patOf :: MatchWing a -> Pattern a
patOf (MatchWing pat _) = pat

checkMatchExhaustive :: Location -> Type -> [Pattern Typed] -> SemAnalyzer Bool
checkMatchExhaustive location scrutineeType patterns = case typeHead scrutineeType of
   Nothing -> pure False
   Just qualified -> do
      let typeName = qualified.name
      maybeSymbol <- lookupDeclaringModule qualified
      case maybeSymbol of
         Just (SymbolType _ typeSig) | not typeSig.nonExhaustive ->
            if any isCatchAll patterns then 
               return True
            else do
               let covered = S.fromList [ctorIdent | PatCon _ ctorIdent _ <- patterns]
               let missing = S.toList (typeSig.ctors S.\\ covered)
               if null missing then 
                  return True
               else do
                  errSem (SENonExhaustiveMatch location typeName missing)
                  return False
         _ -> return False
   where
      isCatchAll (PatVar _ _)    = True
      isCatchAll (PatWildcard _) = True
      isCatchAll _               = False

-- | Looks a name up in the module that declares it, or in the current module
--   when that is not known.
lookupDeclaringModule :: QualifiedIdent -> SemAnalyzer (Maybe SymbolInfo)
lookupDeclaringModule qualified = case qualified.moduleName of
   Just module' -> lookupInModule module' qualified.name
   Nothing      -> lookupCurrentModule qualified.name

checkClausesExhaustive :: Location -> [Type] -> [[Pattern Typed]] -> SemAnalyzer Bool
checkClausesExhaustive location parameterTypes clauseArgs = do
   results <- forM (zip parameterTypes (transpose clauseArgs)) $ uncurry (checkMatchExhaustive location)
   pure (and results)
