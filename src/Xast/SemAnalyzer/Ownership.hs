{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
module Xast.SemAnalyzer.Ownership where

import Control.Monad (forM_, unless, when)
import Control.Monad.State (modify)
import qualified Data.Set as S
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Query (lookupCurrentFunction)
import Xast.SemAnalyzer.Exhaustive (lookupDeclaringModule, typeSignatureOf)
import Xast.SemAnalyzer.TypeUsages (payloadTypes)

-- #### @Copyable, @Pod and @Drop ####

-- | Checks the ownership modifiers of every type a program declares.
checkOwnership :: Program Parsed -> SemAnalyzer ()
checkOwnership program = do
   modify $ \symTable -> symTable { currentModule = program.moduleDef.name }
   forM_ program.stmts $ \case
      StmtTypeDef typeDef             -> checkTypeDef program.moduleDef.name typeDef
      StmtExtern (ExtType externType) -> checkExternType program.moduleDef.name externType
      StmtFunc (FnDef funcDef)        -> checkSignatureMarks funcDef.location (map (.node) funcDef.args) (map (.node) funcDef.results)
      StmtExtern (ExtFunc externFunc) -> checkSignatureMarks externFunc.location (map (.node) externFunc.args) (map (.node) externFunc.results)
      _                               -> pure ()

checkTypeDef :: Module -> TypeDef -> SemAnalyzer ()
checkTypeDef module' typeDef = do
   let ownership = ownershipOf typeDef.modifiers
   let self = applyGenerics module' typeDef.name typeDef.generics
   checkDropCombination typeDef.location typeDef.name ownership
   checkOwnershipFunctions typeDef.location typeDef.name self ownership
   case ownership.copyability of
      Pod      -> checkPod typeDef
      Copyable -> checkCopyableFields typeDef
      _        -> pure ()

checkExternType :: Module -> ExternType -> SemAnalyzer ()
checkExternType module' externType = do
   let ownership = ownershipOf externType.modifiers
   let self = applyGenerics module' externType.name externType.generics
   checkDropCombination externType.location externType.name ownership
   when (ownership.copyability == Copyable) $
      errSem (SECopyableExtern externType.location externType.name)
   checkOwnershipFunctions externType.location externType.name self ownership

-- #### `*` in signatures ####

-- | Every @*@ result hands back a @*@ parameter of its type, one each:
--   @fn(*Console) -> (Int, *Console)@ is valid, @fn(Console) -> *Console@ is not.
--   Function types inside the signature follow the same rule.
checkSignatureMarks :: Location -> [MarkedType] -> [MarkedType] -> SemAnalyzer ()
checkSignatureMarks location params results = do
   let takenTypes = [ param.ty | param <- params, param.mark == Take ]
   let handedTypes = [ result.ty | result <- results, result.mark == Take ]
   forM_ (unmatchedResults takenTypes handedTypes) $ \resType ->
      errSem (SEHandedBackWithoutTake location resType)
   forM_ (concatMap functionTypesIn (functionTypes params results)) 
      $ \(innerParams, innerResults) ->
         checkSignatureMarks location innerParams innerResults

-- | The handed-back results left without a taken parameter, each parameter used once.
unmatchedResults :: [Type] -> [Type] -> [Type]
unmatchedResults takenTypes = \case
   [] -> []
   handed : rest -> case break (== handed) takenTypes of
      (before, _ : after) -> unmatchedResults (before ++ after) rest
      (_, [])             -> handed : unmatchedResults takenTypes rest

-- | The function types written inside a type, e.g. the callback of @map@.
functionTypesIn :: Type -> [([MarkedType], [MarkedType])]
functionTypesIn = \case
   TyFn params results      -> [(params, results)]
   TyApp function arg       -> functionTypesIn function ++ functionTypesIn arg
   TyTuple elements         -> concatMap functionTypesIn elements
   _                        -> []

-- | @Pod excludes @Drop, and plain @Copyable on a type with @Drop would copy what it frees.
checkDropCombination :: Location -> TypeName -> Ownership -> SemAnalyzer ()
checkDropCombination location typeName ownership = case (ownership.copyability, ownership.dropFn) of
   (Pod, Just _)      -> errSem (SEPodWithDrop location typeName)
   (Copyable, Just _) -> errSem (SECopyableWithDrop location typeName)
   _                  -> pure ()

-- | The functions named by @Copyable(fn) and @Drop(fn) exist, a copy function
--   has the type @fn(T) -> T@, e.g. @fn(Vec a) -> Vec a@, and a drop function
--   @fn(*T) -> ()@.
checkOwnershipFunctions
   :: Location
   -> TypeName
   -> Type       -- ^ the type as written in its own definition, e.g. @Vec a@
   -> Ownership
   -> SemAnalyzer ()
checkOwnershipFunctions location typeName self ownership = do
   case ownership.copyability of
      CopyableWith copyFn -> lookupCurrentFunction copyFn >>= \case
         Nothing -> errSem (SEUnknownOwnershipFunction location typeName copyFn)
         Just signature ->
            unless (paramTypes signature == [self] && resultType signature == self) $
               errSem (SECopyFunctionSignature location copyFn self)
      _ -> pure ()
   forM_ ownership.dropFn $ \dropFn -> lookupCurrentFunction dropFn >>= \case
      Nothing -> errSem (SEUnknownOwnershipFunction location typeName dropFn)
      Just signature ->
         unless (isDropSignature self signature) $
            errSem (SEDropFunctionSignature location dropFn self)

isDropSignature :: Type -> FuncSig -> Bool
isDropSignature self signature =
   signature.params == [MarkedType Take self] && resultType signature == TyTuple []

-- | A type applied to its own parameters, e.g. @Vec a@.
applyGenerics :: Module -> TypeName -> [GenericName] -> Type
applyGenerics module' typeName generics =
   let typeCon = TyCon QualifiedIdent { moduleName = Just module', name = typeName }
   in foldl TyApp typeCon (map TyGnr generics)

-- | A @Pod type is an enum of constructors without fields, or one constructor
--   whose fields are all @Pod and have data.
checkPod :: TypeDef -> SemAnalyzer ()
checkPod typeDef
   | not (null typeDef.generics) = podError typeDef PodGeneric
   | all hasNoFields typeDef.ctors = pure ()
   | [single] <- typeDef.ctors = forM_ (payloadTypes single.payload) (checkPodField typeDef)
   | otherwise = podError typeDef PodSumWithFields

checkPodField :: TypeDef -> Located Type -> SemAnalyzer ()
checkPodField typeDef field = do
   zeroSized <- isZeroSizedType field.node
   pod <- isPodType field.node
   if zeroSized
      then podError typeDef (PodZeroSizedField field.node)
      else unless pod (podError typeDef (PodNotPodField field.node))

podError :: TypeDef -> PodProblem -> SemAnalyzer ()
podError typeDef problem = errSem (SEInvalidPod typeDef.location typeDef.name problem)

hasNoFields :: Ctor -> Bool
hasNoFields ctor = null (payloadTypes ctor.payload)

-- | Every field of a plain @Copyable type is copyable. A type parameter counts
--   as copyable: @Option a@ is copyable when @a@ is.
checkCopyableFields :: TypeDef -> SemAnalyzer ()
checkCopyableFields typeDef =
   forM_ typeDef.ctors $ \ctor ->
      forM_ (payloadTypes ctor.payload) $ \field -> do
         copyable <- isCopyableType field.node
         unless copyable $
            errSem (SENotCopyableField field.location typeDef.name field.node)

-- | Whether values of a type may be copied, given that its type parameters can be:
--   @Option Int@ needs both @Option@ and @Int@ to be copyable.
isCopyableType :: Type -> SemAnalyzer Bool
isCopyableType = \case
   TyTuple elements -> allM isCopyableType elements
   TyFn {}         -> pure True  -- a function that captures nothing
   type' -> case typeHeadAndArgs type' of
      Just (typeName, args) -> do
         copyable <- (/= NotCopyable) <$> copyabilityOf typeName
         argsCopyable <- allM isCopyableType args
         pure (copyable && argsCopyable)
      -- generics count as copyable here; unknown types are reported elsewhere
      Nothing -> pure True

-- | A @Pod type, without type arguments.
isPodType :: Type -> SemAnalyzer Bool
isPodType type' = case typeHeadAndArgs type' of
   Just (typeName, []) -> (== Pod) <$> copyabilityOf typeName
   _                   -> pure False

-- | A type with no data: unit, or one constructor without fields, e.g. @type Marker = Marker@.
isZeroSizedType :: Type -> SemAnalyzer Bool
isZeroSizedType = \case
   TyTuple [] -> pure True
   type' -> case typeHeadAndArgs type' of
      Just (typeName, _) -> lookupDeclaringModule typeName >>= \case
         Just (SymbolTypeCtor _ typeSignature _ ctorSignature) ->
            pure (S.size typeSignature.ctors == 1 && null ctorSignature.fields)
         Just (SymbolType _ typeSignature) | [ctorName] <- S.toList typeSignature.ctors ->
            lookupDeclaringModule QualifiedIdent { moduleName = typeName.moduleName, name = ctorName } >>= \case
               Just (SymbolCtor _ _ ctorSignature) -> pure (null ctorSignature.fields)
               _                                   -> pure False
         _ -> pure False
      Nothing -> pure False

copyabilityOf :: QualifiedIdent -> SemAnalyzer Copyability
copyabilityOf typeName = lookupDeclaringModule typeName >>= \case
   Just (SymbolExternType _ ownership) -> pure ownership.copyability
   Just symbol | Just typeSignature <- typeSignatureOf symbol -> pure typeSignature.ownership.copyability
   _ -> pure Copyable  -- unknown types are reported elsewhere

-- | @Option Int@ is @(Std.Option.Option, [Int])@.
typeHeadAndArgs :: Type -> Maybe (QualifiedIdent, [Type])
typeHeadAndArgs = \case
   TyCon typeName      -> Just (typeName, [])
   TyApp function arg  -> fmap (\(typeName, args) -> (typeName, args ++ [arg])) (typeHeadAndArgs function)
   _                   -> Nothing

allM :: (a -> SemAnalyzer Bool) -> [a] -> SemAnalyzer Bool
allM predicate = \case
   []            -> pure True
   first : rest -> predicate first >>= \case
      True  -> allM predicate rest
      False -> pure False
