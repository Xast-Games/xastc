{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Types where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe)

import Xast.AST
import Xast.Utils.Compiler (Target)

data AnalysisResult = AnalysisResult
   { warningsCount :: Int
   , progs         :: [Program Desugared]
   , ctorSpans     :: M.Map ConstructorId Int
   , intrinsics    :: M.Map ExternId IntrinsicDecl
   , functionModules :: M.Map FunctionId Module  -- ^ declaring module of every function
   }

data Env = Env
   { vars           :: M.Map VarName VarInfo
   , functions      :: M.Map FunctionName FuncSig
   , systems        :: M.Map SystemName SystemSig
   , modulePackages :: M.Map Module PackageName
   , currentTarget  :: Target
   }

emptyEnv :: M.Map Module PackageName -> Target -> Env
emptyEnv = Env M.empty M.empty M.empty

data SymTable = SymTable
   { modules         :: M.Map Module ModuleInfo
   , currentModule   :: Module
   , varIdSupply     :: Int
   , localIdSupply   :: Int
   , fnIdSupply      :: Int
   , ctorIdSupply    :: Int
   , externIdSupply  :: Int
   , tyVarSupply     :: Int
   , tySubst         :: M.Map Int Type
   , tyVarConstraints :: M.Map Int TyVarConstraint  -- ^ constrained type variables
   }
   deriving (Eq, Show)

-- | A type variable limited to @number@ or @concatenative@ types.
data TyVarConstraint = TyVarConstraint
   { kind   :: TyIntrinsic
   , origin :: Location  -- ^ the expression that introduced it
   }
   deriving (Eq, Show)

emptySymTable :: SymTable
emptySymTable = SymTable
   { modules = M.empty
   , currentModule = Module []
   , varIdSupply = 0
   , localIdSupply = 0
   , fnIdSupply = 0
   , ctorIdSupply = 0
   , externIdSupply = 0
   , tyVarSupply = 0
   , tySubst = M.empty
   , tyVarConstraints = M.empty
   }

data QualifiedName = QualifiedName
   { moduleName :: Module
   , name       :: Ident
   }
   deriving (Eq, Show, Ord)

data ModuleInfo = ModuleInfo
   { symbols        :: M.Map Ident SymbolInfo
   , exports        :: M.Map Ident Module  -- ^ exported name → declaring module
   , abstractTypes  :: S.Set TypeName      -- ^ exported without their same-named constructor, e.g. @Vec@
   , exportedFields :: M.Map TypeName (S.Set FieldName)  -- ^ fields of own types that other modules may use
   }
   deriving (Eq, Show)

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo
   { symbols        = M.empty
   , exports        = M.empty
   , abstractTypes  = S.empty
   , exportedFields = M.empty
   }

data SymbolInfo
   = SymbolType
      { location :: Location
      , typeSig  :: TypeSig
      }
   | SymbolTypeCtor
      { location      :: Location
      , typeSig       :: TypeSig
      , constructorId :: ConstructorId
      , ctorSig       :: CtorSig
      }
   | SymbolCtor
      { location      :: Location
      , constructorId :: ConstructorId
      , ctorSig       :: CtorSig
      }
   | SymbolFn
      { location   :: Location
      , functionId :: FunctionId
      , funcSig    :: FuncSig
      }
   | SymbolSystem
      { location :: Location
      , sysSig   :: SystemSig
      }
   | SymbolExternFn
      { location :: Location
      , externId :: ExternId
      , funcSig  :: FuncSig
      }
   | SymbolExternType
      { location  :: Location
      , ownership :: Ownership
      }
   deriving (Eq, Show)

-- | How values of a type are copied and dropped, from its modifiers.
data Ownership = Ownership
   { copyability :: Copyability
   , dropFn      :: Maybe FunctionName  -- ^ from @\@Drop(fn)@
   }
   deriving (Eq, Show)

data Copyability
   = NotCopyable
   | Copyable                  -- ^ @\@Copyable@: field by field, when its type parameters are copyable
   | CopyableWith FunctionName -- ^ @\@Copyable(fn)@: by calling @fn@
   | Pod                       -- ^ @\@Pod@: bit by bit
   deriving (Eq, Show)

-- | The ownership a type declares with @\@Copyable@, @\@Pod@ and @\@Drop@.
ownershipOf :: [Modifier] -> Ownership
ownershipOf modifiers =
   let copyability
         | ModPod `elem` modifiers = Pod
         | otherwise = case [ copyFn | ModCopyable copyFn <- modifiers ] of
            Just copyFn : _ -> CopyableWith copyFn
            Nothing : _     -> Copyable
            []              -> NotCopyable
   in Ownership
      { copyability = copyability
      , dropFn      = listToMaybe [ dropFn | ModDrop dropFn <- modifiers ]
      }

data CtorSig = CtorSig
   { owner        :: QualifiedIdent  -- ^ the type the constructor builds
   , generics     :: [GenericName]
   , fieldNames   :: Maybe [FieldName] -- `Just` field names when declared as a record, `Nothing` otherwise
   , fields       :: [Type]
   }
   deriving (Eq, Show)

data TypeSig = TypeSig
   { ctors         :: S.Set CtorName
   , generics      :: [GenericName]
   , nonExhaustive :: Bool
   , ownership     :: Ownership
   }
   deriving (Eq, Show)

symbolLoc :: SymbolInfo -> Location
symbolLoc = \case
   SymbolType loc _         -> loc
   SymbolTypeCtor loc _ _ _ -> loc
   SymbolCtor loc _ _       -> loc
   SymbolFn loc _ _         -> loc
   SymbolSystem loc _       -> loc
   SymbolExternFn loc _ _   -> loc
   SymbolExternType loc _   -> loc

data SystemSig = SystemSig
   { name      :: SystemName
   , entities  :: [QueriedEntity]
   , retType   :: Type
   , with      :: Maybe [WithType]
   }
   deriving (Eq, Show)

data VarInfo = VarInfo
   { ty         :: Type
   , id         :: VarId
   , quantified :: [Int]
   }
   deriving (Eq, Show)

newtype VarId = VarId { inner :: Int }
   deriving (Eq, Show)

data FuncSig = FuncSig
   { params  :: [MarkedType]
   , results :: [MarkedType]
   }
   deriving (Eq, Show)

funcSig :: FuncDef -> FuncSig
funcSig fn = FuncSig (map (.node) fn.args) (map (.node) fn.results)

externFuncSig :: ExternFunc -> FuncSig
externFuncSig fn = FuncSig (map (.node) fn.args) (map (.node) fn.results)

-- | The parameter types, without their marks.
paramTypes :: FuncSig -> [Type]
paramTypes signature = map (.ty) signature.params

-- | The type a call returns.
resultType :: FuncSig -> Type
resultType signature = joinResults signature.results

-- | The type of the function itself, e.g. @fn(Int, *Vec a) -> *Vec a@.
signatureType :: FuncSig -> Type
signatureType signature = functionType signature.params signature.results

systemSig :: SystemDef -> SystemSig
systemSig def =
   SystemSig def.name def.entities def.retType.node def.with

data SuggestedImports = SuggestedImports
   { symbol     :: Ident
   , candidates :: [Module]
   }
