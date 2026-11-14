{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.SemAnalyzer.Exhaustive where

import Control.Monad (forM)
import Data.Foldable (toList)
import Data.List (nubBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Text as T
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query
import Xast.SemAnalyzer.Unify (resolve)
import qualified Xast.Utils.List as L
import Xast.Utils.Generic ((|>))

-- #### Exhaustiveness ####
-- Usefulness of a wildcard row (Maranget, "Warnings for pattern matching"):
-- a match is exhaustive when no value escapes all its rows.

typeHead :: Type -> Maybe QualifiedIdent
typeHead = \case
   TyCon qualified -> Just qualified
   TyApp func _ -> typeHead func
   _            -> Nothing

patOf :: MatchWing a -> Pattern a
patOf (MatchWing pat _) = pat

-- | A pattern reduced to what coverage depends on: variables are wildcards.
data Space
   = Anything
   | Built
      { shape      :: Shape
      , fieldTypes :: [Type]   -- ^ e.g. @[Shape]@ for @Some Triangle@
      , fields     :: [Space]
      }

-- | What a pattern tests at the top.
data Shape
   = ShapeCtor CtorName
   | ShapeLit Literal
   | ShapeTuple Int
   | ShapeList Int  -- ^ a list of exactly this length
   deriving Eq

-- | The values a column can hold.
data Signature
   = Closed [(Shape, Int)]  -- ^ every constructor with its arity, e.g. @True@, @False@
   | Open [(Shape, Int)]    -- ^ a @NonExhaustive type: unnamed constructors may be left out
   | Unbounded              -- ^ literals, lists, generics: only a wildcard covers every value

-- | A value that no row matches, e.g. @Some Square@.
data Witness
   = AnyValue
   | BuiltValue
      { shape  :: Shape
      , fields :: [Witness]
      }

data Coverage = Coverage
   { missing :: [[Witness]]  -- ^ one vector per uncovered value
   , partial :: Bool         -- ^ values of a @NonExhaustive type were left out
   }

instance Semigroup Coverage where
   (<>) :: Coverage -> Coverage -> Coverage
   first <> second = Coverage
      { missing = first.missing ++ second.missing
      , partial = first.partial || second.partial
      }

instance Monoid Coverage where
   mempty :: Coverage
   mempty = Coverage { missing = [], partial = False }

-- | Checks one @match@: reports uncovered values, and tells whether every value is covered.
checkMatchExhaustive :: Location -> Type -> [Pattern Typed] -> SemAnalyzer Bool
checkMatchExhaustive location scrutineeType patterns =
   checkRows location [scrutineeType] (map (: []) patterns)

-- | Checks the clauses of a function or system, one column per parameter.
checkClausesExhaustive :: Location -> [Type] -> [[Pattern Typed]] -> SemAnalyzer Bool
checkClausesExhaustive = checkRows

checkRows
   :: Location
   -> [Type]             -- ^ the type of each column
   -> [[Pattern Typed]]  -- ^ one row per arm or clause
   -> SemAnalyzer Bool
checkRows location columnTypes rows = do
   invalid <- anyInvalid columnTypes rows
   if invalid then
      -- an earlier error is already reported
      pure False
   else do
      coverage <- uncovered columnTypes (map (map toSpace) rows)
      case coverage.missing of
         [] -> pure (not coverage.partial)
         witnesses -> do
            errSem (SENonExhaustiveMatch location (map renderVector witnesses))
            pure False

anyInvalid :: [Type] -> [[Pattern Typed]] -> SemAnalyzer Bool
anyInvalid columnTypes rows = do
   let patternTypes = L.forFlat (concat rows) $ \pattern' ->
         pattern' |> toList |> map (.ty)
   resolvedTypes <- mapM resolve (columnTypes ++ patternTypes)
   pure (any containsInvalid resolvedTypes)

containsInvalid :: Type -> Bool
containsInvalid = \case
   TyInvalid        -> True
   TyApp func arg   -> containsInvalid func || containsInvalid arg
   TyTuple elements -> any containsInvalid elements
   TyFn params results -> any containsInvalid (functionTypes params results)
   _                -> False

toSpace :: Pattern Typed -> Space
toSpace = \case
   PatVar _ _            -> Anything
   PatWildcard _         -> Anything
   PatLit _ literal      -> Built { shape = ShapeLit literal, fieldTypes = [], fields = [] }
   PatTuple _ [single]   -> toSpace single
   PatTuple _ elements   -> builtFrom (ShapeTuple (length elements)) elements
   PatList _ elements    -> builtFrom (ShapeList (length elements)) elements
   PatCon _ ctorName args -> builtFrom (ShapeCtor ctorName) args

builtFrom :: Shape -> [Pattern Typed] -> Space
builtFrom shape subPatterns = Built
   { shape      = shape
   , fieldTypes = map patType subPatterns
   , fields     = map toSpace subPatterns
   }

-- | The values that no row matches.
uncovered
   :: [Type]     -- ^ the type of each column
   -> [[Space]]  -- ^ rows, in order
   -> SemAnalyzer Coverage
uncovered [] rows
   | null rows = pure Coverage { missing = [[]], partial = False }
   | otherwise = pure mempty
uncovered (columnType : restTypes) rows = do
   signature <- signatureOf columnType
   let heads = headShapes rows
   let named = map fst heads
   case signature of
      -- every constructor is named: each one is checked on its own
      Closed ctors | allNamed named ctors -> splitByHeads restTypes rows heads
      Open ctors   | allNamed named ctors -> splitByHeads restTypes rows heads

      Closed ctors | not (null heads) -> do
         byHeads <- splitByHeads restTypes rows heads
         others <- uncovered restTypes (defaultRows rows)
         let missingCtors = L.forEach (unnamedCtors named ctors) $ \(shape, arity) ->
               BuiltValue { shape = shape, fields = replicate arity AnyValue }
         pure (byHeads <> prependEach missingCtors others)

      Open _ | not (null heads) -> do
         byHeads <- splitByHeads restTypes rows heads
         others <- uncovered restTypes (defaultRows rows)
         let leftOut = not (null others.missing) || others.partial
         pure byHeads { partial = byHeads.partial || leftOut }

      -- no constructor is named, or there are too many values to name
      _ -> do
         others <- uncovered restTypes (defaultRows rows)
         pure (prependEach [AnyValue] others)

-- | Every constructor of a signature is at the head of some row.
allNamed :: [Shape] -> [(Shape, Int)] -> Bool
allNamed named ctors = ctors
   |> all (\(shape, _) -> shape `elem` named)

unnamedCtors :: [Shape] -> [(Shape, Int)] -> [(Shape, Int)]
unnamedCtors named ctors = ctors
   |> filter (\(shape, _) -> shape `notElem` named)

-- | The distinct shapes at the head of the rows, with the types of their fields.
headShapes :: [[Space]] -> [(Shape, [Type])]
headShapes rows =
   let heads = L.forMaybe rows $ \case
         Built shape fieldTypes _ : _ -> Just (shape, fieldTypes)
         _                            -> Nothing
   in nubBy ((==) `on` fst) heads

-- | Checks the values built by each head shape separately.
splitByHeads :: [Type] -> [[Space]] -> [(Shape, [Type])] -> SemAnalyzer Coverage
splitByHeads restTypes rows heads = do
   coverages <- forM heads $ \(shape, fieldTypes) -> do
      let arity = length fieldTypes
      coverage <- uncovered (fieldTypes ++ restTypes) (specialize shape arity rows)
      pure coverage { missing = map (rebuild shape arity) coverage.missing }
   pure (mconcat coverages)

-- | The rows that match a value built by @shape@, with its fields as new columns.
specialize :: Shape -> Int -> [[Space]] -> [[Space]]
specialize shape arity = mapMaybe $ \case
   Built rowShape _ rowFields : rest | rowShape == shape ->
      Just (take arity (rowFields ++ repeat Anything) ++ rest)
   Anything : rest -> Just (replicate arity Anything ++ rest)
   _ -> Nothing

-- | The rows that match a value no other row names, without their first column.
defaultRows :: [[Space]] -> [[Space]]
defaultRows = mapMaybe $ \case
   Anything : rest -> Just rest
   _               -> Nothing

-- | Folds the first @arity@ witnesses back into the value built by @shape@.
rebuild :: Shape -> Int -> [Witness] -> [Witness]
rebuild shape arity witnesses =
   let (fieldWitnesses, rest) = splitAt arity witnesses
   in BuiltValue { shape = shape, fields = fieldWitnesses } : rest

-- | Puts each of @firsts@ in front of every uncovered vector.
prependEach :: [Witness] -> Coverage -> Coverage
prependEach firsts coverage = coverage
   { missing = [first : vector | first <- firsts, vector <- coverage.missing] }

signatureOf :: Type -> SemAnalyzer Signature
signatureOf columnType = resolve columnType >>= \case
   TyTuple [single] -> signatureOf single
   TyTuple elements -> pure (Closed [(ShapeTuple (length elements), length elements)])
   resolved -> case typeHead resolved of
      Nothing        -> pure Unbounded
      Just qualified -> userTypeSignature qualified

-- | The constructors of a declared type; extern types are unbounded.
userTypeSignature :: QualifiedIdent -> SemAnalyzer Signature
userTypeSignature qualified = lookupDeclaringModule qualified >>= \case
   Just symbol | Just typeSignature <- typeSignatureOf symbol -> do
      ctors <- forM (S.toList typeSignature.ctors) $ \ctorName -> do
         arity <- ctorArity qualified ctorName
         pure (ShapeCtor ctorName, arity)
      pure $ if typeSignature.nonExhaustive then Open ctors else Closed ctors
   _ -> pure Unbounded

typeSignatureOf :: SymbolInfo -> Maybe TypeSig
typeSignatureOf = \case
   SymbolType _ typeSignature         -> Just typeSignature
   SymbolTypeCtor _ typeSignature _ _ -> Just typeSignature
   _                                  -> Nothing

-- | The number of fields of a constructor, declared in the module of its type.
ctorArity :: QualifiedIdent -> CtorName -> SemAnalyzer Int
ctorArity typeName ctorName =
   let qualifiedCtor = QualifiedIdent { moduleName = typeName.moduleName, name = ctorName }
   in lookupDeclaringModule qualifiedCtor >>= \case
      Just (SymbolCtor _ _ ctorSignature)       -> pure (length ctorSignature.fields)
      Just (SymbolTypeCtor _ _ _ ctorSignature) -> pure (length ctorSignature.fields)
      _                                         -> pure 0

-- | Looks a name up in the module that declares it, or in the current module
--   when that is not known.
lookupDeclaringModule :: QualifiedIdent -> SemAnalyzer (Maybe SymbolInfo)
lookupDeclaringModule qualified = case qualified.moduleName of
   Just module' -> lookupInModule module' qualified.name
   Nothing      -> lookupCurrentModule qualified.name

-- | An uncovered value as source, one column as is, several as a tuple,
--   e.g. @(True, True)@.
renderVector :: [Witness] -> Text
renderVector = \case
   [single] -> renderWitness single
   columns  -> "(" <> T.intercalate ", " (map renderWitness columns) <> ")"

renderWitness :: Witness -> Text
renderWitness = \case
   AnyValue -> "_"
   BuiltValue (ShapeCtor ctorName) [] -> ctorName.inner
   BuiltValue (ShapeCtor ctorName) fields ->
      T.unwords (ctorName.inner : map renderField fields)
   BuiltValue (ShapeTuple _) fields ->
      "(" <> T.intercalate ", " (map renderWitness fields) <> ")"
   BuiltValue (ShapeList _) fields ->
      "[" <> T.intercalate ", " (map renderWitness fields) <> "]"
   BuiltValue (ShapeLit literal) _ -> T.pack (show literal)

-- | A constructor argument, in parentheses when it has fields, e.g. @(Some _)@.
renderField :: Witness -> Text
renderField witness = case witness of
   BuiltValue (ShapeCtor _) (_ : _) -> "(" <> renderWitness witness <> ")"
   _                                -> renderWitness witness
