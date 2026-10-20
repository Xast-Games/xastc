{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Types where

import qualified Data.Map as M
import qualified Data.Set as S

import Xast.AST
import Xast.Utils.Compiler (Target)

data AnalysisResult = AnalysisResult
   { warningsCount :: Int
   , progs         :: [Program Desugared]
   , ctorSpans     :: M.Map ConstructorId Int
   }

data Env = Env
   { vars      :: M.Map VarName VarInfo
   , functions :: M.Map FunctionName FuncSig
   , systems   :: M.Map SystemName SystemSig
   , allowedIntrinsics :: [FunctionName]
   , currentTarget :: Target
   }

emptyEnv :: Target -> Env
emptyEnv = Env M.empty M.empty M.empty allowedIntrinsics

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
   }

data QualifiedName = QualifiedName
   { moduleName :: Module
   , name       :: Ident
   }
   deriving (Eq, Show, Ord)

data ModuleInfo = ModuleInfo
   { symbols :: M.Map Ident SymbolInfo
   , exports :: S.Set Ident
   }
   deriving (Eq, Show)

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo M.empty S.empty

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
   | SymbolExternType Location
   deriving (Eq, Show)

data CtorSig = CtorSig
   { owner        :: TypeName
   , generics     :: [GenericName]
   , fieldNames   :: Maybe [FieldName] -- `Just` field names when declared as a record, `Nothing` otherwise
   , fields       :: [Type]
   }
   deriving (Eq, Show)

data TypeSig = TypeSig
   { ctors         :: S.Set CtorName
   , generics      :: [GenericName]
   , nonExhaustive :: Bool
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
   SymbolExternType loc     -> loc

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
   { args      :: [Type]
   , retType   :: Type
   }
   deriving (Eq, Show)

funcSig :: FuncDef -> FuncSig
funcSig fn = FuncSig (map (.node) fn.args) fn.retType.node

externFuncSig :: ExternFunc -> FuncSig
externFuncSig fn = FuncSig (map (.node) fn.args) fn.retType.node

systemSig :: SystemDef -> SystemSig
systemSig def =
   SystemSig def.name def.entities def.retType.node def.with

data SuggestedImports = SuggestedImports
   { symbol     :: Ident
   , candidates :: [Module]
   }
