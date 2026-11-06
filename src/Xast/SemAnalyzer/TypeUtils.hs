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
import Xast.SemAnalyzer.Query (findAnyTypeModule, findTypeModule)
import Control.Applicative ((<|>))
import Xast.Utils.Generic (unreachableWith, (|>))
import qualified Data.Text as T
import Xast.SemAnalyzer.Unify (bindOrFail, freshTyVar, resolve, substGnr, unify)

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
         bindOrFail location varId (TyFn [operandTy] returnTypeVar)
         pure returnTypeVar

      _ -> do
         errSem (SENotAFunction location applicantTy')
         pure TyInvalid

-- | Unify `expected` with `current`, binding any unresolved type variables
-- on either side. `unify` reports its own (generic) `SETypeError` on
-- mismatch, so its output is suppressed here in favor of a plain type-error
-- message with a stable (expected, current) type ordering.
compareTypes :: Location -> Type -> Type -> SemAnalyzer ()
compareTypes location expected current = do
   mismatched <- unifyMismatched (unify location expected current)
   when mismatched $ do
      expected' <- resolve expected
      current' <- resolve current
      errSem (SETypeError location expected' current')

-- | Like `compareTypes`, but unifies the two branch types and reports a
-- `then`/`else`-specific mismatch instead of a generic type error.
compareThenElse :: Location -> Type -> Location -> Type -> SemAnalyzer ()
compareThenElse thenLoc thenType elseLoc elseType = do
   mismatched <- unifyMismatched (unify thenLoc thenType elseType)
   when mismatched $ do
      thenType' <- resolve thenType
      elseType' <- resolve elseType
      errSem (SEThenElseTypeMismatch thenLoc thenType' elseLoc elseType')

-- | Unifies two list element types
compareListElems :: Location -> Type -> Location -> Type -> SemAnalyzer ()
compareListElems firstLoc firstTy otherLoc otherTy = do
   mismatched <- unifyMismatched (unify firstLoc firstTy otherTy)
   when mismatched $ do
      firstTy' <- resolve firstTy
      otherTy' <- resolve otherTy
      errSem (SEListElementTypeMismatch firstLoc firstTy' otherLoc otherTy')

-- | Runs a unification whose mismatch the caller reports in its own words, and
--   says whether there was one. An unsatisfied constraint is not a mismatch of
--   the two types, so it is still reported as it is.
unifyMismatched :: SemAnalyzer () -> SemAnalyzer Bool
unifyMismatched unification = do
   ((), reports) <- censor (filter isConstraintError) $ listen unification
   let mismatches = reports
         |> filter isSemError
         |> filter (not . isConstraintError)
   pure (not (null mismatches))

isSemError :: SemReport -> Bool
isSemError (SemError _) = True
isSemError _            = False

isConstraintError :: SemReport -> Bool
isConstraintError (SemError (SETypeCheckError (SEUnsatisfiedConstraint {}))) = True
isConstraintError _                                                         = False

literalType :: [Located ImportDef] -> Literal -> SemAnalyzer Type
literalType imports = \case
   LitString _   -> builtinType imports (Ident "String")
   LitChar _     -> builtinType imports (Ident "Char")
   LitInt lit    -> builtinType imports (Ident (T.pack (show lit.kind)))
   LitFloat _    -> builtinType imports (Ident "Float")
   LitDouble _   -> builtinType imports (Ident "Double")
   LitTuple elements -> TyTuple <$> mapM (literalType imports . (.node)) elements
   LitList [] -> genericList imports
   LitList (first : rest) -> do
      -- Check inner list types
      checkLitListType imports first rest
      -- Type of list is defined as `List a`,
      -- where a is a type of the first element
      firstElemType <- literalType imports first.node
      listType imports firstElemType

checkLitListType :: [Located ImportDef] -> Located Literal -> [Located Literal] -> SemAnalyzer ()
checkLitListType imports (Located firstLoc firstElem) others =
   forM_ others $ \(Located otherLoc otherElem) -> do
      firstType <- literalType imports firstElem
      otherType <- literalType imports otherElem
      unless (firstType == otherType) $
         errSem (SEListElementTypeMismatch firstLoc firstType otherLoc otherType)

-- | @List a@ with a fresh element type.
genericList :: [Located ImportDef] -> SemAnalyzer Type
genericList imports = freshTyVar >>= listType imports

-- | @List@ of the given element type.
listType :: [Located ImportDef] -> Type -> SemAnalyzer Type
listType imports elementType = do
   listConstructor <- builtinType imports (Ident "List")
   pure (TyApp listConstructor elementType)

boolType :: [Located ImportDef] -> SemAnalyzer Type
boolType imports = builtinType imports (Ident "Bool")

-- | A type the language itself refers to, such as the type of a literal. It comes
--   from the module visible here that declares it, otherwise from any module of
--   the program, since such types may be used without being imported.
builtinType :: [Located ImportDef] -> TypeName -> SemAnalyzer Type
builtinType imports name = do
   visibleModule <- findTypeModule imports name
   anyModule <- findAnyTypeModule name
   pure $ case visibleModule <|> anyModule of
      Just module' -> typeIn module' name
      Nothing      -> unresolvedType name
