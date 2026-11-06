module Xast.Error.Types where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Xast.AST
import Toml (TomlDecodeError)

data SemReport
   = SemWarning SemWarning
   | SemError SemError

class ToSemError a where
   toSemError :: a -> SemError

instance ToSemError SemError where
   toSemError :: SemError -> SemError
   toSemError = id

data SemError
   = SEImportError ImportError
   | SERedeclarationError RedeclarationError
   | SEModifierError ModifierError
   | SEShadowingError ShadowingError
   | SEUndefinedError UndefinedError
   | SEDefinitionError DefinitionError
   | SETypeCheckError TypeCheckError
   deriving Show

data ImportError
   = SESelfImportError
      { moduleName         :: Module
      , definitionLocation :: Location  -- ^ the module header
      , location           :: Location  -- ^ the import of the module itself
      }
   | SECyclicImportError
      { cycle    :: [Module]  -- ^ modules of the cycle, in import order
      , location :: Location
      }
   | SEMissingModule
      { moduleName :: Module
      , location   :: Location
      }
   | SEMissingImports
      { moduleName   :: Module      -- ^ the imported module
      , location     :: Location
      , missingNames :: [Ident]     -- ^ imported names the module does not declare
      }
   | SEPrivateImports
      { moduleName   :: Module      -- ^ the imported module
      , location     :: Location
      , privateNames :: [Ident]     -- ^ imported names the module does not export
      }
   | SEInvalidExport
      { moduleName   :: Module
      , location     :: Location
      , invalidNames :: [Ident]     -- ^ exported names the module does not declare
      }
   | SEAmbiguousAlias
      { alias          :: Ident
      , firstLocation  :: Location
      , secondLocation :: Location
      }
   | SEAmbiguousImport
      { name           :: Ident
      , firstLocation  :: Location
      , secondLocation :: Location
      }
   | SEImportDeclConflict
      { name                :: Ident
      , importLocation      :: Location
      , declarationLocation :: Location
      }
   deriving Show

instance ToSemError ImportError where
  toSemError :: ImportError -> SemError
  toSemError = SEImportError

data RedeclarationError
   = SETypeRedeclaration
      { name             :: Ident
      , previousLocation :: Location  -- ^ the first declaration
      , location         :: Location  -- ^ the redeclaration
      }
   | SEFnRedeclaration
      { name             :: Ident
      , previousLocation :: Location
      , location         :: Location
      }
   | SEExternFnRedeclaration
      { name             :: Ident
      , previousLocation :: Location
      , location         :: Location
      }
   | SEExternTypeRedeclaration
      { name             :: Ident
      , previousLocation :: Location
      , location         :: Location
      }
   | SESystemRedeclaration
      { name             :: Ident
      , previousLocation :: Location
      , location         :: Location
      }
   | SECtorRedeclaration
      { name             :: Ident
      , previousLocation :: Location
      , location         :: Location
      }
   deriving Show

instance ToSemError RedeclarationError where
  toSemError :: RedeclarationError -> SemError
  toSemError = SERedeclarationError

-- | A modifier used where it does not fit.
data ModifierError
   = SEInvalidTagType
      { location :: Location
      , typeName :: TypeName
      }
   | SEUnknownIntrinsic
      { location :: Location
      , name     :: FunctionName  -- ^ marked @Intrinsic, but the compiler does not define it
      }
   | SEIntrinsicArityMismatch
      { location      :: Location
      , name          :: FunctionName
      , expectedArity :: Int  -- ^ the parameters of the compiler's definition
      , actualArity   :: Int  -- ^ the parameters of the declaration
      }
   deriving Show

instance ToSemError ModifierError where
   toSemError :: ModifierError -> SemError
   toSemError = SEModifierError

-- | A local binding that reuses a name already bound.
data ShadowingError
   = SEDuplicateBinding
      { name             :: Ident
      , previousLocation :: Location  -- ^ the first binding
      , location         :: Location  -- ^ the duplicate binding
      }
   | SEShadowedBinding
      { name          :: Ident
      , outerLocation :: Location  -- ^ the binding being shadowed
      , location      :: Location  -- ^ the shadowing binding
      }
   deriving Show

instance ToSemError ShadowingError where
   toSemError :: ShadowingError -> SemError
   toSemError = SEShadowingError

-- | A use of a name that is not declared.
data UndefinedError
   = SEUndefinedVar
      { location :: Location
      , name     :: Ident
      }
   | SEUndefinedCon
      { location :: Location
      , name     :: Ident
      }
   | SEUndefinedType
      { location :: Location
      , name     :: Ident
      }
   | SEUndefinedAlias
      { file  :: FilePath
      , alias :: Ident
      }
   deriving Show

instance ToSemError UndefinedError where
   toSemError :: UndefinedError -> SemError
   toSemError = SEUndefinedError

-- | A declaration and its implementations that do not pair up.
data DefinitionError
   = SEMissingFnDef
      { location :: Location  -- ^ the implementation without a declaration
      , name     :: Ident
      }
   | SEExtraFnDef
      { location       :: Location    -- ^ the redundant implementation
      , name           :: Ident
      , otherLocations :: [Location]  -- ^ the other implementations
      }
   | SEMissingFnImpls
      { location :: Location  -- ^ the declaration without an implementation
      , name     :: Ident
      }
   | SEMissingSystemDef
      { location :: Location  -- ^ the implementation without a declaration
      , name     :: Ident
      }
   | SEExtraSystemDef
      { location       :: Location    -- ^ the redundant implementation
      , name           :: Ident
      , otherLocations :: [Location]  -- ^ the other implementations
      }
   | SEMissingSystemImpls
      { location :: Location  -- ^ the declaration without an implementation
      , name     :: Ident
      }
   deriving Show

instance ToSemError DefinitionError where
   toSemError :: DefinitionError -> SemError
   toSemError = SEDefinitionError

-- | An expression or pattern of the wrong type.
data TypeCheckError
   = SEIntegerOutOfBounds
      { location :: Location
      , intKind  :: IntKind
      , value    :: Integer
      , bounds   :: (Integer, Integer)  -- ^ lowest and highest value of the kind
      }
   | SETypeError
      { location :: Location
      , expected :: Type
      , actual   :: Type
      }
   | SEListElementTypeMismatch
      { firstLocation :: Location  -- ^ the first element, which sets the list type
      , firstType     :: Type
      , otherLocation :: Location  -- ^ the element of another type
      , otherType     :: Type
      }
   | SEThenElseTypeMismatch
      { thenLocation :: Location
      , thenType     :: Type
      , elseLocation :: Location
      , elseType     :: Type
      }
   | SEInfiniteType
      { location  :: Location
      , typeVar   :: Int   -- ^ id of the type variable
      , boundType :: Type  -- ^ the type it would be bound to, which contains it
      }
   | SEUnsatisfiedConstraint
      { location   :: Location
      , constraint :: TyIntrinsic  -- ^ what the type had to be, e.g. @number@
      , origin     :: Location     -- ^ the expression whose signature requires it, e.g. an operator
      , actual     :: Type
      }
   | SEAmbiguousConstraint
      { location   :: Location     -- ^ the expression whose signature introduced the type
      , constraint :: TyIntrinsic
      }
   | SETooManyArgs
      { location     :: Location
      , functionType :: Type
      }
   | SENotAFunction
      { location  :: Location
      , valueType :: Type
      }
   | SECtorArityMismatch
      { location      :: Location
      , name          :: Ident
      , expectedArity :: Int
      , actualArity   :: Int
      }
   | SEFnArityMismatch
      { location      :: Location
      , name          :: Ident
      , expectedArity :: Int
      , actualArity   :: Int
      }
   | SESystemArityMismatch
      { location      :: Location
      , name          :: Ident
      , expectedArity :: Int
      , actualArity   :: Int
      }
   | SEUnknownField
      { location  :: Location
      , ctorName  :: CtorName
      , fieldName :: FieldName
      }
   | SENotARecordType
      { location  :: Location
      , valueType :: Type
      }
   | SEAmbiguousRecordAccess
      { location   :: Location
      , ctorName   :: CtorName
      , otherCtors :: [CtorName]  -- ^ the other constructors of its type
      }
   | SEInvalidTupleIndex
      { location  :: Location
      , tupleType :: Type
      , index     :: Int
      }
   | SENonExhaustiveMatch
      { location     :: Location
      , typeName     :: TypeName
      , missingCtors :: [CtorName]
      }
   deriving Show

instance ToSemError TypeCheckError where
   toSemError :: TypeCheckError -> SemError
   toSemError = SETypeCheckError

data SemWarning
   = SWUnusedImport Module
   | SWDeadCode Ident
   | SWRedundantImport ImportIntersection
   | SWFunctionGayness
      { location  :: Location
      , name      :: Ident
      , argsCount :: Int
      }
   deriving Show

data XastError
   = XastParseError (ParseErrorBundle Text Void)
   | XastTomlDecodeError
      { file        :: FilePath
      , decodeError :: TomlDecodeError
      }
   | XastSemAnalyzeError SemError
   | XastFileNotFound
      { file      :: FilePath
      , directory :: FilePath
      }
   | XastModuleNotFound
      { moduleName :: Module
      , directory  :: FilePath
      }
   | XastModuleNameMismatch
      { location :: Location  -- ^ the module header
      , expected :: Module    -- ^ the module the file path stands for
      , declared :: Module    -- ^ the module the header names
      }
   deriving Show
