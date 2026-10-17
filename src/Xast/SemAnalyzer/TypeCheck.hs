{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.TypeCheck where

import Control.Monad.State
import Control.Monad (unless, zipWithM, forM)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query
import Xast.Utils.Generic ((<--))
import Xast.SemAnalyzer.IntBounds (checkIntegerBounds)
import Xast.SemAnalyzer.TypeUtils (compareTypes)
import Xast.SemAnalyzer.InferExpr (inferType)
import Xast.SemAnalyzer.InferPattern (inferPattern)
import Xast.SemAnalyzer.Unify (typeOf, withType)

typeCheck :: Program Resolved -> SemAnalyzer (Program Typed)
typeCheck (Program mdl@(ModuleDef _ m _) imps stmts src) = do
   modify $ \st -> st { currentModule = m }
   Program mdl imps <$> traverse (typeCheckStmt imps) stmts <-- src

typeCheckStmt :: [Located ImportDef] -> Stmt Resolved -> SemAnalyzer (Stmt Typed)
typeCheckStmt imps (StmtFunc (FnImpl (FuncImpl implLoc fnIdent pats expr))) = do
   (FuncSig argTypes retType) <- fromJust <$> lookupCurrentFunction fnIdent

   -- 0) Check expressions integer bounds
   checkIntegerBounds expr

   -- 1) match patterns and args count
   unless (length pats == length argTypes) $
      errSem (SEFnArityMismatch implLoc fnIdent (length argTypes) (length pats))

   -- 2) match patterns and args types
   inferred <- zipWithM (inferPattern imps) argTypes pats
   let (pats', varMaps) = unzip inferred
   let patVars = M.unions varMaps

   -- 3) withVars inferType of `expr`
   expr' <- withVars patVars (inferType imps expr)

   -- 4) compare types
   compareTypes implLoc retType (typeOf expr')

   pure $ StmtFunc (FnImpl (FuncImpl implLoc fnIdent pats' expr'))

typeCheckStmt imps (StmtSystem (SysImpl (SystemImpl implLoc sysIdent entPats mWith body))) = do
   (SystemSig _ sigEnts sigRet sigWith) <- fromJust <$> lookupCurrentSystem sysIdent
   -- 0) Check expressions integer bounds
   checkIntegerBounds body

   -- 1) match entity patterns and queried components count
   unless (length entPats == length sigEnts) $
      errSem (SESystemArityMismatch implLoc sysIdent (length sigEnts) (length entPats))

   entResults <- forM (zip entPats sigEnts) $ \(EntityPattern bindings, QueriedEntity tysLoc) -> do
      let tys = map (.node) tysLoc
      unless (length bindings == length tys) $
         errSem (SESystemArityMismatch implLoc sysIdent (length tys) (length bindings))
      inferred <- zipWithM
         (\ty (EntPatBinding pat access) -> do
            (pat', vars) <- inferPattern imps ty pat
            pure (EntPatBinding pat' access, vars))
         tys bindings
      let (bindings', varMaps) = unzip inferred
      pure (EntityPattern bindings', varMaps)
   let entPats' = map fst entResults
   let entVarMaps = concatMap snd entResults

   -- 2) match `with` patterns and `with` types count
   (mWith', withVarsList) <- case (mWith, sigWith) of
      (Nothing, _) -> pure (Nothing, [])
      (Just ps, Just wts) -> do
         unless (length ps == length wts) $
            errSem (SESystemArityMismatch implLoc sysIdent (length wts) (length ps))
         inferred <- zipWithM (inferPattern imps) (map withType wts) ps
         let (ps', varMaps) = unzip inferred
         pure (Just ps', varMaps)
      (Just ps, Nothing) -> do
         errSem (SESystemArityMismatch implLoc sysIdent 0 (length ps))
         inferred <- mapM (inferPattern imps TyInvalid) ps
         let (ps', varMaps) = unzip inferred
         pure (Just ps', varMaps)

   -- 3) withVars inferType of `body`
   body' <- withVars (M.unions (entVarMaps ++ withVarsList)) (inferType imps body)

   -- 4) compare types
   compareTypes implLoc sigRet (typeOf body')

   pure $ StmtSystem (SysImpl (SystemImpl implLoc sysIdent entPats' mWith' body'))

typeCheckStmt _ (StmtFunc (FnDef def)) = pure $ StmtFunc (FnDef def)

typeCheckStmt _ (StmtSystem (SysDef def)) = pure $ StmtSystem (SysDef def)

typeCheckStmt _ (StmtTypeDef td) = pure $ StmtTypeDef td

typeCheckStmt _ (StmtExtern ext) = pure $ StmtExtern ext
