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
typeCheck (Program moduleDef@(ModuleDef _ moduleName _) imports statements source) = do
   modify $ \currentState -> currentState { currentModule = moduleName }
   Program moduleDef imports <$> traverse (typeCheckStmt imports) statements <-- source

typeCheckStmt :: [Located ImportDef] -> Stmt Resolved -> SemAnalyzer (Stmt Typed)
typeCheckStmt imports (StmtFunc (FnImpl (FuncImpl implLocation functionName patterns expression))) = do
   (FuncSig argTypes retType) <- fromJust <$> lookupCurrentFunction functionName

   -- 0) Check expressions integer bounds
   checkIntegerBounds expression

   -- 1) match patterns and args count
   unless (length patterns == length argTypes) $
      errSem (SEFnArityMismatch implLocation functionName (length argTypes) (length patterns))

   -- 2) match patterns and args types
   inferred <- zipWithM (inferPattern imports) argTypes patterns
   let (typedPatterns, varMaps) = unzip inferred
   let patternVars = M.unions varMaps

   -- 3) withVars inferType of `expression`
   typedExpression <- withVars patternVars (inferType imports expression)

   -- 4) compare types
   compareTypes implLocation retType (typeOf typedExpression)

   pure $ StmtFunc (FnImpl (FuncImpl implLocation functionName typedPatterns typedExpression))

typeCheckStmt imports (StmtSystem (SysImpl (SystemImpl implLocation systemName entityPatterns maybeWith body))) = do
   (SystemSig _ signatureEntities signatureResult signatureWith) <- fromJust <$> lookupCurrentSystem systemName
   -- 0) Check expressions integer bounds
   checkIntegerBounds body

   -- 1) match entity patterns and queried components count
   unless (length entityPatterns == length signatureEntities) $
      errSem (SESystemArityMismatch implLocation systemName (length signatureEntities) (length entityPatterns))

   entityResults <- forM (zip entityPatterns signatureEntities) $ \(EntityPattern bindings, QueriedEntity locatedTypes) -> do
      let componentTypes = map (.node) locatedTypes
      unless (length bindings == length componentTypes) $
         errSem (SESystemArityMismatch implLocation systemName (length componentTypes) (length bindings))
      inferred <- zipWithM
         (\componentType (EntPatBinding pattern access) -> do
            (typedPattern, vars) <- inferPattern imports componentType pattern
            pure (EntPatBinding typedPattern access, vars))
         componentTypes bindings
      let (typedBindings, varMaps) = unzip inferred
      pure (EntityPattern typedBindings, varMaps)
   let typedEntityPatterns = map fst entityResults
   let entityVarMaps = concatMap snd entityResults

   -- 2) match `with` patterns and `with` types count
   (typedWith, withVarsList) <- case (maybeWith, signatureWith) of
      (Nothing, _) -> pure (Nothing, [])
      (Just withPatterns, Just withTypes) -> do
         unless (length withPatterns == length withTypes) $
            errSem (SESystemArityMismatch implLocation systemName (length withTypes) (length withPatterns))
         inferred <- zipWithM (inferPattern imports) (map withType withTypes) withPatterns
         let (typedWithPatterns, varMaps) = unzip inferred
         pure (Just typedWithPatterns, varMaps)
      (Just withPatterns, Nothing) -> do
         errSem (SESystemArityMismatch implLocation systemName 0 (length withPatterns))
         inferred <- mapM (inferPattern imports TyInvalid) withPatterns
         let (typedWithPatterns, varMaps) = unzip inferred
         pure (Just typedWithPatterns, varMaps)

   -- 3) withVars inferType of `body`
   typedBody <- withVars (M.unions (entityVarMaps ++ withVarsList)) (inferType imports body)

   -- 4) compare types
   compareTypes implLocation signatureResult (typeOf typedBody)

   pure $ StmtSystem (SysImpl (SystemImpl implLocation systemName typedEntityPatterns typedWith typedBody))

typeCheckStmt _ (StmtFunc (FnDef definition)) = pure $ StmtFunc (FnDef definition)

typeCheckStmt _ (StmtSystem (SysDef definition)) = pure $ StmtSystem (SysDef definition)

typeCheckStmt _ (StmtTypeDef typeDef) = pure $ StmtTypeDef typeDef

typeCheckStmt _ (StmtExtern extern) = pure $ StmtExtern extern
