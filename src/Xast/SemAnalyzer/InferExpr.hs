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
inferType imps expr = case expr of
   ExpLit (ResolvedInfo loc mRes) literal -> do
      ty <- literalType literal
      pure $ ExpLit (TypedInfo loc ty mRes) literal

   ExpVar (ResolvedInfo loc mRes) Nothing x -> do
      thisSym <- lookupLocal x
      modSym  <- lookupCurrentModule x
      impSym  <- lookupUnqualifiedSymbol imps x
      ty <- case thisSym of
         Just vi -> do
            t     <- resolve vi.ty
            fresh <- mapM (const freshTyVar) vi.quantified
            pure (substTyVars (M.fromList (zip vi.quantified fresh)) t)
         Nothing -> case modSym <|> impSym of
            Just (SymbolFn _ _ sig)       -> instantiate sig
            Just (SymbolExternFn _ _ sig) -> instantiate sig
            _ -> do
               errSem (SEUndefinedVar loc x)
               pure TyInvalid
      pure $ ExpVar (TypedInfo loc ty mRes) Nothing x

   ExpVar (ResolvedInfo loc mRes) (Just alias) x -> do
      sym <- lookupQualifiedSymbol imps alias x
      ty <- case sym of
         Just (SymbolFn _ _ sig)       -> instantiate sig
         Just (SymbolExternFn _ _ sig) -> instantiate sig
         _ -> do
            errSem (SEUndefinedVar loc x)
            pure TyInvalid
      pure $ ExpVar (TypedInfo loc ty mRes) (Just alias) x

   ExpCon (ResolvedInfo loc mRes) Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      ty <- case modCon <|> impCon of
         Just sym -> ctorType loc sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid
      pure $ ExpCon (TypedInfo loc ty mRes) Nothing x

   ExpCon (ResolvedInfo loc mRes) (Just alias) x -> do
      con <- lookupQualifiedConstructor imps alias x
      ty <- case con of
         Just sym -> ctorType loc sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid
      pure $ ExpCon (TypedInfo loc ty mRes) (Just alias) x

   ExpApp (ResolvedInfo loc mRes) applicant operand -> do
      applicant' <- inferType imps applicant
      operand'   <- inferType imps operand
      ty <- applyTypes loc (typeOf applicant') (typeOf operand')
      pure $ ExpApp (TypedInfo loc ty mRes) applicant' operand'

   ExpTuple (ResolvedInfo loc mRes) xs -> do
      xs' <- mapM (inferType imps) xs
      pure $ ExpTuple (TypedInfo loc (TyTuple (map typeOf xs')) mRes) xs'

   ExpList (ResolvedInfo loc mRes) [] -> do
      ty <- TyApp (TyCon (Ident "List")) <$> freshTyVar
      pure $ ExpList (TypedInfo loc ty mRes) []

   ExpList (ResolvedInfo loc mRes) (x:xs) -> do
      x' <- inferType imps x
      xs' <- mapM (inferType imps) xs
      -- Check inner list types
      forM_ xs' $ \other ->
         compareListElems 
            (exprAnnotation x').location (typeOf x')
            (exprAnnotation other).location (typeOf other)
      -- Type of list is defined as `List a`,
      -- where a is a type of the first element
      let ty = TyApp (TyCon (Ident "List")) (typeOf x')
      pure $ ExpList (TypedInfo loc ty mRes) (x':xs')

   ExpIfThen (ResolvedInfo loc mRes) (IfThenElse if' then' else') -> do
      -- Compare `if` type with Bool
      if'' <- inferType imps if'
      compareTypes (exprAnnotation if'').location (typeOf if'') boolType
      -- Compare `then` and `else` types
      then'' <- inferType imps then'
      else'' <- inferType imps else'
      compareThenElse (exprAnnotation then'').location (typeOf then'') (exprAnnotation else'').location (typeOf else'')
      -- Type of the whole expr is the type of the `then` block
      pure $ ExpIfThen (TypedInfo loc (typeOf then'') mRes) (IfThenElse if'' then'' else'')

   ExpLetIn (ResolvedInfo loc mRes) (LetIn binds body) -> do
      (binds', body') <- typeLetBinds imps binds body
      pure $ ExpLetIn (TypedInfo loc (typeOf body') mRes) (LetIn binds' body')

   ExpMatch (ResolvedInfo loc mRes) (Match mtExp mtMatches _) -> do
      mtExp' <- inferType imps mtExp
      wings <- forM mtMatches $ \(MatchWing pat branch) -> do
         (pat', patVars) <- inferPattern imps (typeOf mtExp') pat
         branch' <- withVars patVars (inferType imps branch)
         pure (MatchWing pat' branch', typeOf branch')
      ty <- case wings of
         [] -> pure TyInvalid
         ((_, t):rest) -> do
            forM_ rest (unify loc t . snd)
            resolve t
      scrutTy <- resolve (typeOf mtExp')
      isExhaustive <- checkMatchExhaustive loc scrutTy (map (patOf . fst) wings)
      pure $ ExpMatch (TypedInfo loc ty mRes) (Match mtExp' (map fst wings) isExhaustive)

   ExpLambda (ResolvedInfo loc mRes) (Lambda args body) -> do
      argTyVars <- forM args $ const freshTyVar
      inferred  <- zipWithM (inferPattern imps) argTyVars args
      let (args', argVarMaps) = unzip inferred
      let argVars = M.unions argVarMaps

      body' <- withVars argVars (inferType imps body)

      argTyVars' <- mapM resolve argTyVars
      let ty = TyFn argTyVars' (typeOf body')
      pure $ ExpLambda (TypedInfo loc ty mRes) (Lambda args' body')

   ExpRecConstruct (ResolvedInfo loc mRes) (RecConstruct rcBind rcCon rcAssigns) -> do
      sym <- case rcBind of
         Nothing -> do
            modCon <- lookupCurrentConstructor rcCon
            impCon <- lookupUnqualifiedConstructor imps rcCon
            pure (modCon <|> impCon)
         Just alias -> lookupQualifiedConstructor imps alias rcCon

      (ty, rcAssigns') <- case sym of
         Just s@(SymbolCtor _ _ (CtorSig _ _ mFieldNames _)) -> do
            resultTy <- ctorType loc s
            let (fieldTys, ctorResultTy) = case resultTy of
                  TyFn args r -> (args, r)
                  r           -> ([], r)
                fieldTypeOf fld = lookup fld (zip (fromMaybe [] mFieldNames) fieldTys)
            assigns' <- forM rcAssigns $ \(RecAssign fldName value) -> do
               value' <- inferType imps value
               case fieldTypeOf fldName.node of
                  Just expectedTy -> compareTypes loc expectedTy (typeOf value')
                  Nothing         -> errSem (SEUnknownField loc rcCon fldName.node)
               pure $ RecAssign fldName value'
            pure (ctorResultTy, assigns')

         _ -> do
            errSem (SEUndefinedCon loc rcCon)
            assigns' <- forM rcAssigns $ \(RecAssign fldName value) -> do
               value' <- inferType imps value
               pure $ RecAssign fldName value'
            pure (TyInvalid, assigns')

      pure $ ExpRecConstruct (TypedInfo loc ty mRes) (RecConstruct rcBind rcCon rcAssigns')

   ExpRecUpdate (ResolvedInfo loc mRes) (RecUpdate ruBase ruAssigns) -> do
      ruBase' <- inferType imps ruBase
      baseTy  <- resolve (typeOf ruBase')
      fields  <- recordFieldsOf imps loc baseTy

      ruAssigns' <- forM ruAssigns $ \(RecAssign fldName value) -> do
         value' <- inferType imps value
         case fields of
            Just (con, fieldNames, fieldTys) ->
               case lookup fldName.node (zip fieldNames fieldTys) of
                  Just expectedTy -> compareTypes loc expectedTy (typeOf value')
                  Nothing         -> errSem (SEUnknownField loc con fldName.node)
            Nothing -> pure ()
         pure $ RecAssign fldName value'

      pure $ ExpRecUpdate (TypedInfo loc baseTy mRes) (RecUpdate ruBase' ruAssigns')

   ExpVarGetter (ResolvedInfo loc mRes) baseExpr getter -> do
      baseExpr' <- inferType imps baseExpr
      baseTy    <- resolve (typeOf baseExpr')
      ty <- case getter of
         GetField fld -> do
            fields <- recordFieldsOf imps loc baseTy
            case fields of
               Just (con, fieldNames, fieldTys) ->
                  case lookup fld (zip fieldNames fieldTys) of
                     Just fldTy -> pure fldTy
                     Nothing    -> errSem (SEUnknownField loc con fld) >> pure TyInvalid
               Nothing -> pure TyInvalid  -- recordFieldsOf already reported why

         GetTupleField idx -> case baseTy of
            TyTuple tys | idx >= 0 && idx < length tys -> pure (tys !! idx)
            _ -> errSem (SEInvalidTupleIndex loc baseTy idx) >> pure TyInvalid

      pure $ ExpVarGetter (TypedInfo loc ty mRes) baseExpr' getter

recordFieldsOf :: [Located ImportDef] -> Location -> Type -> SemAnalyzer (Maybe (Ident, [Ident], [Type]))
recordFieldsOf imps loc ty = case typeHead ty of
   Nothing -> do
      errSem (SENotARecordType loc ty)
      pure Nothing
   Just conIdent -> do
      modCon <- lookupCurrentConstructor conIdent
      impCon <- lookupUnqualifiedConstructor imps conIdent
      case modCon <|> impCon of
         Just s@(SymbolCtor _ _ (CtorSig _ _ (Just fieldNames) _)) -> do
            modTy <- lookupCurrentConType conIdent
            impTy <- lookupUnqualifiedConType imps conIdent
            case modTy <|> impTy of
               Just (SymbolType _ (TypeSig ctors _ _)) | S.size ctors > 1 -> do
                  errSem (SEAmbiguousRecordAccess loc conIdent (S.toList (S.delete conIdent ctors)))
                  pure Nothing
               _ -> do
                  resultTy <- ctorType loc s
                  let (fieldTys, ctorResultTy) = case resultTy of
                        TyFn args r -> (args, r)
                        r           -> ([], r)
                  unify loc ctorResultTy ty
                  fieldTys' <- mapM resolve fieldTys
                  pure $ Just (conIdent, fieldNames, fieldTys')
         _ -> do
            errSem (SENotARecordType loc ty)
            pure Nothing

typeLetBinds
   :: [Located ImportDef]
   -> [Let Resolved]
   -> Expr Resolved
   -> SemAnalyzer ([Let Typed], Expr Typed)
typeLetBinds imps [] body = ([],) <$> inferType imps body
typeLetBinds imps (Let pat value : rest) body = do
   value'  <- inferType imps value
   (pat', patVars) <- inferPattern imps (typeOf value') pat
   patVars' <- case pat of
      PatVar {} -> forM patVars $ \vi -> do
         qs <- generalize vi.ty
         pure vi { quantified = qs }
      _ -> pure patVars

   (rest', body') <- withVars patVars' (typeLetBinds imps rest body)
   pure (Let pat' value' : rest', body')
