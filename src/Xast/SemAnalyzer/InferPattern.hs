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
