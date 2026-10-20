{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.TypeUtils where

import Control.Monad.Writer (listen, censor)
import Control.Monad (forM_, unless, when)
import Data.Foldable (foldl')
import qualified Data.Map as M
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.Utils.Generic (unreachableWith)
import qualified Data.Text as T
import Xast.SemAnalyzer.Unify (bindVar, freshTyVar, occursCheckType, resolve, substGnr, unify)

ctorType :: Location -> SymbolInfo -> SemAnalyzer Type
ctorType _ (SymbolCtor _ _ (CtorSig ownerIdent generics _ fieldTys)) = do
   fresh <- mapM (const freshTyVar) generics
   let subst    = M.fromList (zip generics fresh)
       fieldTypes = map (substGnr subst) fieldTys
       resultType = foldl' TyApp (TyCon ownerIdent) fresh
   pure $ if null fieldTypes then resultType else TyFn fieldTypes resultType
ctorType location invalid =
   unreachableWith $ "Invalid constructor symbol at " ++ show location ++ ": " ++ show invalid

applyTypes :: Location -> Type -> Type -> SemAnalyzer Type
applyTypes location applicantTy operandTy = do
   applicantTy' <- resolve applicantTy
   case applicantTy' of
      TyFn (argTy:restArgs) retTy -> do
         unify location argTy operandTy
         if null restArgs
            then resolve retTy
            else resolve (TyFn restArgs retTy)

      TyFn [] _ -> do
         errSem (SETooManyArgs location applicantTy')
         pure TyInvalid

      TyVar varId -> do
         returnTypeVar <- freshTyVar
         occurs <- occursCheckType varId (TyFn [operandTy] returnTypeVar)
         if occurs
            then errSem (SEInfiniteType location varId applicantTy') >> pure TyInvalid
            else bindVar varId (TyFn [operandTy] returnTypeVar) >> pure returnTypeVar

      _ -> do
         errSem (SENotAFunction location applicantTy')
         pure TyInvalid

-- | Unify `expected` with `current`, binding any unresolved type variables
-- on either side. `unify` reports its own (generic) `SETypeError` on
-- mismatch, so its output is suppressed here in favor of a plain type-error
-- message with a stable (expected, current) type ordering.
compareTypes :: Location -> Type -> Type -> SemAnalyzer ()
compareTypes location expected current = do
   ((), reports) <- censor (const []) $ listen (unify location expected current)
   when (any isSemError reports) $ do
      expected' <- resolve expected
      current' <- resolve current
      errSem (SETypeError location expected' current')

-- | Like `compareTypes`, but unifies the two branch types and reports a
-- `then`/`else`-specific mismatch instead of a generic type error.
compareThenElse :: Location -> Type -> Location -> Type -> SemAnalyzer ()
compareThenElse thenLoc thenType elseLoc elseType = do
   ((), reports) <- censor (const []) $ listen (unify thenLoc thenType elseType)
   when (any isSemError reports) $ do
      thenType' <- resolve thenType
      elseType' <- resolve elseType
      errSem (SEThenElseTypeMismatch thenLoc thenType' elseLoc elseType')

-- | Unifies two list element types
compareListElems :: Location -> Type -> Location -> Type -> SemAnalyzer ()
compareListElems firstLoc firstTy otherLoc otherTy = do
   ((), reports) <- censor (const []) $ listen (unify firstLoc firstTy otherTy)
   when (any isSemError reports) $ do
      firstTy' <- resolve firstTy
      otherTy' <- resolve otherTy
      errSem (SEListElementTypeMismatch firstLoc firstTy' otherLoc otherTy')

isSemError :: SemReport -> Bool
isSemError (SemError _) = True
isSemError _            = False

literalType :: Literal -> SemAnalyzer Type
literalType (LitString _) = pure $ TyCon (Ident "String")
literalType (LitChar _) = pure $ TyCon (Ident "Char")
literalType (LitInt lit) = pure $ TyCon (Ident (T.pack (show lit.kind)))
literalType (LitFloat _) = pure $ TyCon (Ident "Float")
literalType (LitDouble _) = pure $ TyCon (Ident "Double")
literalType (LitTuple xs) = TyTuple <$> mapM (literalType . (.node)) xs
literalType (LitList []) = genericList
literalType (LitList (x:xs)) = do
   -- Check inner list types
   checkLitListType x xs
   -- Type of list is defined as `List a`, 
   -- where a is a type of the first element
   firstElemType <- literalType x.node
   return $ TyApp (TyCon (Ident "List")) firstElemType

checkLitListType :: Located Literal -> [Located Literal] -> SemAnalyzer ()
checkLitListType (Located firstLoc firstElem) others =
   forM_ others $ \(Located otherLoc otherElem) -> do
      firstType <- literalType firstElem
      otherType <- literalType otherElem
      unless (firstType == otherType) $
         errSem (SEListElementTypeMismatch firstLoc firstType otherLoc otherType)

genericList :: SemAnalyzer Type
genericList = TyApp (TyCon (Ident "List")) <$> freshTyVar

boolType :: Type
boolType = TyCon (Ident "Bool")
