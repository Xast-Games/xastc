{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.InferExpr where

import Control.Monad (forM_, zipWithM, forM)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query
import Control.Applicative ((<|>))
import Xast.SemAnalyzer.TypeUtils (applyTypes, boolType, compareListElems, compareThenElse, compareTypes, ctorType, literalType)
import Xast.SemAnalyzer.Exhaustive (checkMatchExhaustive, patOf, typeHead)
import Xast.SemAnalyzer.InferPattern (inferPattern)
import Xast.SemAnalyzer.Unify (freshTyVar, generalize, instantiate, resolve, substTyVars, typeOf, unify)

inferType
   :: [Located ImportDef]
   -> Expr Resolved
   -> SemAnalyzer (Expr Typed)
inferType imports expression = case expression of
   ExpLit (ResolvedInfo location maybeResolution) literal -> do
      exprType <- literalType literal
      pure $ ExpLit (TypedInfo location exprType maybeResolution) literal

   ExpVar (ResolvedInfo location maybeResolution) Nothing name -> do
      localSymbol <- lookupLocal name
      moduleSymbol  <- lookupCurrentModule name
      importedSymbol  <- lookupUnqualifiedSymbol imports name
      exprType <- case localSymbol of
         Just varInfo -> do
            varType     <- resolve varInfo.ty
            fresh <- mapM (const freshTyVar) varInfo.quantified
            pure (substTyVars (M.fromList (zip varInfo.quantified fresh)) varType)
         Nothing -> case moduleSymbol <|> importedSymbol of
            Just (SymbolFn _ _ signature)       -> instantiate signature
            Just (SymbolExternFn _ _ signature) -> instantiate signature
            _                                   -> do
               errSem (SEUndefinedVar location name)
               pure TyInvalid
      pure $ ExpVar (TypedInfo location exprType maybeResolution) Nothing name

   ExpVar (ResolvedInfo location maybeResolution) (Just alias) name -> do
      symbol <- lookupQualifiedSymbol imports alias name
      exprType <- case symbol of
         Just (SymbolFn _ _ signature)       -> instantiate signature
         Just (SymbolExternFn _ _ signature) -> instantiate signature
         _                                   -> do
            errSem (SEUndefinedVar location name)
            pure TyInvalid
      pure $ ExpVar (TypedInfo location exprType maybeResolution) (Just alias) name

   ExpCon (ResolvedInfo location maybeResolution) Nothing name -> do
      moduleCtor <- lookupCurrentConstructor name
      importedCtor <- lookupUnqualifiedConstructor imports name
      exprType <- case moduleCtor <|> importedCtor of
         Just symbol -> ctorType location symbol
         Nothing     -> do
            errSem (SEUndefinedCon location name)
            pure TyInvalid
      pure $ ExpCon (TypedInfo location exprType maybeResolution) Nothing name

   ExpCon (ResolvedInfo location maybeResolution) (Just alias) name -> do
      ctor <- lookupQualifiedConstructor imports alias name
      exprType <- case ctor of
         Just symbol -> ctorType location symbol
         Nothing     -> do
            errSem (SEUndefinedCon location name)
            pure TyInvalid
      pure $ ExpCon (TypedInfo location exprType maybeResolution) (Just alias) name

   ExpApp (ResolvedInfo location maybeResolution) applicant operand -> do
      applicant' <- inferType imports applicant
      operand'   <- inferType imports operand
      exprType <- applyTypes location (typeOf applicant') (typeOf operand')
      pure $ ExpApp (TypedInfo location exprType maybeResolution) applicant' operand'

   ExpTuple (ResolvedInfo location maybeResolution) elements -> do
      typedElements <- mapM (inferType imports) elements
      pure $ ExpTuple (TypedInfo location (TyTuple (map typeOf typedElements)) maybeResolution) typedElements

   ExpList (ResolvedInfo location maybeResolution) [] -> do
      exprType <- TyApp (TyCon (Ident "List")) <$> freshTyVar
      pure $ ExpList (TypedInfo location exprType maybeResolution) []

   ExpList (ResolvedInfo location maybeResolution) (first:rest) -> do
      typedFirst <- inferType imports first
      typedRest <- mapM (inferType imports) rest
      -- Check inner list types
      forM_ typedRest $ \other ->
         compareListElems 
            (exprAnnotation typedFirst).location (typeOf typedFirst)
            (exprAnnotation other).location (typeOf other)
      -- Type of list is defined as `List a`,
      -- where a is a type of the first element
      let exprType = TyApp (TyCon (Ident "List")) (typeOf typedFirst)
      pure $ ExpList (TypedInfo location exprType maybeResolution) (typedFirst:typedRest)

   ExpIfThen (ResolvedInfo location maybeResolution) (IfThenElse condition thenBranch elseBranch) -> do
      -- Compare `if` type with Bool
      typedCondition <- inferType imports condition
      compareTypes (exprAnnotation typedCondition).location (typeOf typedCondition) boolType
      -- Compare `then` and `else` types
      typedThen <- inferType imports thenBranch
      typedElse <- inferType imports elseBranch
      compareThenElse (exprAnnotation typedThen).location (typeOf typedThen) (exprAnnotation typedElse).location (typeOf typedElse)
      -- Type of the whole expression is the type of the `then` block
      pure $ ExpIfThen (TypedInfo location (typeOf typedThen) maybeResolution) (IfThenElse typedCondition typedThen typedElse)

   ExpLetIn (ResolvedInfo location maybeResolution) (LetIn bindings body) -> do
      (typedBinds, typedBody) <- typeLetBinds imports bindings body
      pure $ ExpLetIn (TypedInfo location (typeOf typedBody) maybeResolution) (LetIn typedBinds typedBody)

   ExpMatch (ResolvedInfo location maybeResolution) (Match scrutinee matches _) -> do
      typedScrutinee <- inferType imports scrutinee
      typedWings <- forM matches $ \(MatchWing pattern branch) -> do
         (typedPattern, patternVars) <- inferPattern imports (typeOf typedScrutinee) pattern
         typedBranch <- withVars patternVars (inferType imports branch)
         pure (MatchWing typedPattern typedBranch, typeOf typedBranch)
      exprType <- case typedWings of
         [] -> pure TyInvalid
         ((_, firstType):rest) -> do
            forM_ rest (unify location firstType . snd)
            resolve firstType
      scrutineeType <- resolve (typeOf typedScrutinee)
      isExhaustive <- checkMatchExhaustive location scrutineeType (map (patOf . fst) typedWings)
      pure $ ExpMatch (TypedInfo location exprType maybeResolution) (Match typedScrutinee (map fst typedWings) isExhaustive)

   ExpLambda (ResolvedInfo location maybeResolution) (Lambda args body) -> do
      argTypeVars <- forM args $ const freshTyVar
      inferred  <- zipWithM (inferPattern imports) argTypeVars args
      let (typedArgs, argVarMaps) = unzip inferred
      let argVars = M.unions argVarMaps

      typedBody <- withVars argVars (inferType imports body)

      resolvedArgTypes <- mapM resolve argTypeVars
      let exprType = TyFn resolvedArgTypes (typeOf typedBody)
      pure $ ExpLambda (TypedInfo location exprType maybeResolution) (Lambda typedArgs typedBody)

   ExpRecConstruct (ResolvedInfo location maybeResolution) (RecConstruct bindAlias ctorName assigns) -> do
      maybeSymbol <- case bindAlias of
         Nothing -> do
            moduleCtor <- lookupCurrentConstructor ctorName
            importedCtor <- lookupUnqualifiedConstructor imports ctorName
            pure (moduleCtor <|> importedCtor)
         Just alias -> lookupQualifiedConstructor imports alias ctorName

      (exprType, typedAssigns) <- case maybeSymbol of
         Just symbol@(SymbolCtor _ _ (CtorSig _ _ maybeFieldNames _)) -> do
            resultType <- ctorType location symbol
            let (fieldTypes, ctorResultTy) = case resultType of
                  TyFn args result -> (args, result)
                  result           -> ([], result)
                fieldTypeOf field = lookup field (zip (fromMaybe [] maybeFieldNames) fieldTypes)
            typedFieldAssigns <- forM assigns $ \(RecAssign fieldName value) -> do
               typedValue <- inferType imports value
               case fieldTypeOf fieldName.node of
                  Just expectedType -> compareTypes location expectedType (typeOf typedValue)
                  Nothing           -> errSem (SEUnknownField location ctorName fieldName.node)
               pure $ RecAssign fieldName typedValue
            pure (ctorResultTy, typedFieldAssigns)

         _ -> do
            errSem (SEUndefinedCon location ctorName)
            typedFieldAssigns <- forM assigns $ \(RecAssign fieldName value) -> do
               typedValue <- inferType imports value
               pure $ RecAssign fieldName typedValue
            pure (TyInvalid, typedFieldAssigns)

      pure $ ExpRecConstruct (TypedInfo location exprType maybeResolution) (RecConstruct bindAlias ctorName typedAssigns)

   ExpRecUpdate (ResolvedInfo location maybeResolution) (RecUpdate base assigns) -> do
      typedBase <- inferType imports base
      baseType  <- resolve (typeOf typedBase)
      fields  <- recordFieldsOf imports location baseType

      typedAssigns <- forM assigns $ \(RecAssign fieldName value) -> do
         typedValue <- inferType imports value
         case fields of
            Just (ctor, fieldNames, fieldTypes) ->
               case lookup fieldName.node (zip fieldNames fieldTypes) of
                  Just expectedType -> compareTypes location expectedType (typeOf typedValue)
                  Nothing           -> errSem (SEUnknownField location ctor fieldName.node)
            Nothing -> pure ()
         pure $ RecAssign fieldName typedValue

      pure $ ExpRecUpdate (TypedInfo location baseType maybeResolution) (RecUpdate typedBase typedAssigns)

   ExpVarGetter (ResolvedInfo location maybeResolution) baseExpression getter -> do
      typedBaseExpression <- inferType imports baseExpression
      baseType    <- resolve (typeOf typedBaseExpression)
      exprType <- case getter of
         GetField field -> do
            fields <- recordFieldsOf imports location baseType
            case fields of
               Just (ctor, fieldNames, fieldTypes) ->
                  case lookup field (zip fieldNames fieldTypes) of
                     Just fieldType -> pure fieldType
                     Nothing        -> errSem (SEUnknownField location ctor field) >> pure TyInvalid
               Nothing -> pure TyInvalid  -- recordFieldsOf already reported why

         GetTupleField index -> case baseType of
            TyTuple elementTypes | index >= 0 && index < length elementTypes -> pure (elementTypes !! index)
            _ -> errSem (SEInvalidTupleIndex location baseType index) >> pure TyInvalid

      pure $ ExpVarGetter (TypedInfo location exprType maybeResolution) typedBaseExpression getter

recordFieldsOf :: [Located ImportDef] -> Location -> Type -> SemAnalyzer (Maybe (Ident, [Ident], [Type]))
recordFieldsOf imports location recordType = case typeHead recordType of
   Nothing -> do
      errSem (SENotARecordType location recordType)
      pure Nothing
   Just ctorName -> do
      moduleCtor <- lookupCurrentConstructor ctorName
      importedCtor <- lookupUnqualifiedConstructor imports ctorName
      case moduleCtor <|> importedCtor of
         Just symbol@(SymbolCtor _ _ (CtorSig _ _ (Just fieldNames) _)) -> do
            moduleType <- lookupCurrentConType ctorName
            importedType <- lookupUnqualifiedConType imports ctorName
            case moduleType <|> importedType of
               Just (SymbolType _ (TypeSig ctors _ _)) | S.size ctors > 1 -> do
                  errSem (SEAmbiguousRecordAccess location ctorName (S.toList (S.delete ctorName ctors)))
                  pure Nothing
               _ -> do
                  resultType <- ctorType location symbol
                  let (fieldTypes, ctorResultTy) = case resultType of
                        TyFn args result -> (args, result)
                        result           -> ([], result)
                  unify location ctorResultTy recordType
                  resolvedFieldTypes <- mapM resolve fieldTypes
                  pure $ Just (ctorName, fieldNames, resolvedFieldTypes)
         _ -> do
            errSem (SENotARecordType location recordType)
            pure Nothing

typeLetBinds
   :: [Located ImportDef]
   -> [Let Resolved]
   -> Expr Resolved
   -> SemAnalyzer ([Let Typed], Expr Typed)
typeLetBinds imports [] body = ([],) <$> inferType imports body
typeLetBinds imports (Let pattern value : rest) body = do
   typedValue  <- inferType imports value
   (typedPattern, patternVars) <- inferPattern imports (typeOf typedValue) pattern
   generalizedVars <- case pattern of
      PatVar {} -> forM patternVars $ \varInfo -> do
         quantifiedVars <- generalize varInfo.ty
         pure varInfo { quantified = quantifiedVars }
      _ -> pure patternVars

   (typedRest, typedBody) <- withVars generalizedVars (typeLetBinds imports rest body)
   pure (Let typedPattern typedValue : typedRest, typedBody)
