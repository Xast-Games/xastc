module Xast.Error.Types where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Xast.AST
import Toml (TomlDecodeError)

data SemReport
   = SemWarning SemWarning
   | SemError SemError

data SemError
   -- Import error
   = SESelfImportError Module Location Location
   | SECyclicImportError [Module] Location
   | SEMissingModule Module Location
   | SEMissingImports Module Location [Ident]
   | SEPrivateImports Module Location [Ident]
   | SEInvalidExport Module Location [Ident]
   | SEAmbiguousAlias Ident Location Location
   | SEAmbiguousImport Ident Location Location
   | SEImportDeclConflict Ident Location Location
   -- Redeclaration error
   | SETypeRedeclaration Ident Location Location
   | SEFnRedeclaration Ident Location Location
   | SEExternFnRedeclaration Ident Location Location
   | SEExternTypeRedeclaration Ident Location Location
   | SESystemRedeclaration Ident Location Location
   | SECtorRedeclaration Ident Location Location
   -- Modifier error
   | SEInvalidTagType Location TypeName
   -- Shadowing error
   | SEDuplicateBinding Ident Location Location
   | SEShadowedBinding Ident Location Location
   -- Undefined symbols
   | SEUndefinedVar Location Ident
   | SEUndefinedCon Location Ident
   | SEUndefinedType Location Ident
   | SEUndefinedAlias FilePath Ident
   | SEMissingFnDef Location Ident
   | SEExtraFnDef Location Ident [Location]
   | SEMissingFnImpls Location Ident
   | SEMissingSystemDef Location Ident
   | SEExtraSystemDef Location Ident [Location]
   | SEMissingSystemImpls Location Ident
   -- Type checking
   | SEIntegerOutOfBounds Location IntKind Integer (Integer, Integer)
   | SETypeError Location Type Type
   | SEListElementTypeMismatch Location Type Location Type
   | SEThenElseTypeMismatch Location Type Location Type
   | SEInfiniteType Location Int Type
   | SETooManyArgs Location Type
   | SENotAFunction Location Type
   | SECtorArityMismatch Location Ident Int Int
   | SEFnArityMismatch Location Ident Int Int
   | SESystemArityMismatch Location Ident Int Int
   | SEUnknownField Location Ident Ident
   | SENotARecordType Location Type
   | SEAmbiguousRecordAccess Location Ident [Ident]
   | SEInvalidTupleIndex Location Type Int
   | SENonExhaustiveMatch Location Ident [Ident]
   deriving Show

data SemWarning
   = SWUnusedImport Module
   | SWDeadCode Ident
   | SWRedundantImport ImportIntersection
   | SWFunctionGayness Location Ident Int
   deriving Show

data XastError
   = XastParseError (ParseErrorBundle Text Void)
   | XastTomlDecodeError FilePath TomlDecodeError
   | XastSemAnalyzeError SemError
   | XastFileNotFound FilePath FilePath
   | XastModuleNotFound Module FilePath
   deriving Show