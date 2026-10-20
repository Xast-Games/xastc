{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.InferPattern where

import Control.Monad (zipWithM)
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query
import Control.Applicative ((<|>))
import Xast.Utils.Generic (unreachableWith)
import Xast.SemAnalyzer.TypeUtils (ctorType, literalType)
import Xast.SemAnalyzer.Unify (freshTyVar, freshVarId, unify)

inferPattern
   :: [Located ImportDef]
   -> Type
   -> Pattern Resolved
   -> SemAnalyzer (Pattern Typed, M.Map Ident VarInfo)
inferPattern imports ty pattern = case pattern of
   PatVar (ResolvedInfo location maybeResolution) name -> do
      varId <- freshVarId
      pure (PatVar (TypedInfo location ty maybeResolution) name, M.singleton name (VarInfo ty varId []))

   PatWildcard (ResolvedInfo location maybeResolution) ->
      pure (PatWildcard (TypedInfo location ty maybeResolution), M.empty)

   PatLit (ResolvedInfo location maybeResolution) literal -> do
      literalTy <- literalType literal
      unify location ty literalTy
      pure (PatLit (TypedInfo location ty maybeResolution) literal, M.empty)

   PatList (ResolvedInfo location maybeResolution) patterns -> do
      elementTypeVar <- freshTyVar
      unify location ty (TyApp (TyCon (Ident "List")) elementTypeVar)
      inferred <- mapM (inferPattern imports elementTypeVar) patterns
      let (typedPatterns, varMaps) = unzip inferred
      pure (PatList (TypedInfo location ty maybeResolution) typedPatterns, M.unions varMaps)

   PatTuple (ResolvedInfo location maybeResolution) patterns -> do
      elementTypeVars <- mapM (const freshTyVar) patterns
      unify location ty (TyTuple elementTypeVars)
      inferred <- zipWithM (inferPattern imports) elementTypeVars patterns
      let (typedPatterns, varMaps) = unzip inferred
      pure (PatTuple (TypedInfo location ty maybeResolution) typedPatterns, M.unions varMaps)

   PatCon (ResolvedInfo location maybeResolution) ctorIdent patterns -> do
      moduleCtor <- lookupCurrentConstructor ctorIdent
      importedCtor <- lookupUnqualifiedConstructor imports ctorIdent
      case moduleCtor <|> importedCtor of
         Nothing -> do
            inferred <- mapM (inferPattern imports TyInvalid) patterns
            let typedPatterns = map fst inferred
            pure (PatCon (TypedInfo location TyInvalid maybeResolution) ctorIdent typedPatterns, M.empty)

         Just symbol@(SymbolCtor _ ctorId _) -> do
            let maybeResolution' = Just (ResConstructor ctorId)
            resultTy <- ctorType location symbol
            let (expectedFieldTys, ctorResultTy) = case resultTy of
                  TyFn args result -> (args, result)
                  result           -> ([], result)

            unify location ty ctorResultTy

            if length patterns /= length expectedFieldTys
               then do
                  errSem (SECtorArityMismatch location ctorIdent (length expectedFieldTys) (length patterns))
                  inferred <- mapM (inferPattern imports TyInvalid) patterns
                  let typedPatterns = map fst inferred
                  pure (PatCon (TypedInfo location ctorResultTy maybeResolution') ctorIdent typedPatterns, M.empty)
               else do
                  inferred <- zipWithM (inferPattern imports) expectedFieldTys patterns
                  let (typedPatterns, varMaps) = unzip inferred
                  pure (PatCon (TypedInfo location ctorResultTy maybeResolution') ctorIdent typedPatterns, M.unions varMaps)

         Just invalid ->
            unreachableWith $ "Invalid constructor symbol at " ++ show location ++ ": " ++ show invalid
