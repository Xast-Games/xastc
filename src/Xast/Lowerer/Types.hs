{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Types where

import Xast.AST
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Xast.Utils.Generic ((|>))
import Xast.SemAnalyzer.Types (AnalysisResult (..))

data LowerState = LowerState
   { nameSupply :: Int
   , typeTmpls  :: M.Map QualifiedIdent TypeDef
   , typeInsts  :: M.Map TypeInst KirName
   , typeQueue  :: [TypeInst]
   , ctorSpans  :: M.Map ConstructorId Int
   , ctorFieldNames :: M.Map QualifiedIdent [FieldName]  -- ^ declared field names of each constructor
   , intrinsics :: M.Map ExternId IntrinsicDecl  -- ^ @Intrinsic declarations
   , functionModules :: M.Map FunctionId Module  -- ^ declaring module of every function
   , intrinsicInsts :: M.Map TypeInst KirIntrinsicDef  -- ^ requested intrinsic instances
   , funcTmpls :: M.Map QualifiedIdent FunctionTemplate  -- ^ every function, by name
   , funcInsts :: M.Map TypeInst KirName  -- ^ requested generic function instances
   , funcQueue :: [FunctionRequest]  -- ^ instances still to lower
   , loweredInsts :: [(Module, KirFunction)]  -- ^ lowered instances, with their module
   }

-- | A function as declared, to lower once per set of types.
data FunctionTemplate = FunctionTemplate
   { moduleName :: Module
   , signature  :: FuncDef
   , impl       :: FuncImpl Desugared
   }

-- | A generic function instance waiting to be lowered.
data FunctionRequest = FunctionRequest
   { instance'  :: TypeInst
   , typeParams :: M.Map Type Type  -- ^ each generic or @number@ of the signature → its type here
   }

data TypeInst = TypeInst
   { name :: QualifiedIdent
   , args :: [Type]
   }
   deriving (Eq, Ord, Show)

newLowerState :: AnalysisResult -> LowerState
newLowerState analysis = LowerState
   { nameSupply   = 0
   , intrinsics   = analysis.intrinsics
   , functionModules = analysis.functionModules
   , intrinsicInsts = M.empty
   , funcTmpls    = functionTemplates analysis.progs
   , funcInsts    = M.empty
   , funcQueue    = []
   , loweredInsts = []
   , ctorSpans    = analysis.ctorSpans
   , typeTmpls    = M.fromList
      [ (declaredIn p td.name, td) | p <- analysis.progs, StmtTypeDef td <- p.stmts ] 
   , typeInsts    = M.empty
   , typeQueue    = []
   , ctorFieldNames = M.fromList
      [ (declaredIn p ctor.name, map fst (payloadFields ctor.payload))
      | p <- analysis.progs, StmtTypeDef td <- p.stmts, ctor <- td.ctors ]
   }

functionTemplates :: [Program Desugared] -> M.Map QualifiedIdent FunctionTemplate
functionTemplates programs = M.fromList
   [ (declaredIn program funcDef.name, functionTemplate program funcDef funcImpl)
   | program <- programs
   , StmtFunc (FnDef funcDef) <- program.stmts
   , StmtFunc (FnImpl funcImpl) <- program.stmts
   , funcImpl.name == funcDef.name
   ]

functionTemplate :: Program Desugared -> FuncDef -> FuncImpl Desugared -> FunctionTemplate
functionTemplate program funcDef funcImpl = FunctionTemplate
   { moduleName = program.moduleDef.name
   , signature  = funcDef
   , impl       = funcImpl
   }

-- | A name declared at the top level of a program.
declaredIn :: Program Desugared -> Ident -> QualifiedIdent
declaredIn program name = QualifiedIdent { moduleName = Just program.moduleDef.name, name = name }

-- KIRA = Khast Intermediate RepresentAtion
data Kira = Kira
   { moduleName :: Module
   , systems    :: [KirSystem]
   , functions  :: [KirFunction]
   }
   deriving Show

-- | An intrinsic instance, e.g. @opAdd@ on @Int@.
--   Only a signature: codegen writes the body.
data KirIntrinsicDef = KirIntrinsicDef
   { name      :: KirName       -- ^ e.g. @Std_Ops_opAdd_Std_Num_Int@
   , intrinsic :: FunctionName  -- ^ e.g. @opAdd@
   , params    :: [Type]        -- ^ monomorphic
   , retTy     :: Type          -- ^ monomorphic
   }
   deriving Show

data KirTypeDef = KirTypeDef
   { name  :: KirName
   , ctors :: [KirCtor]
   , mods  :: [TypeModifier]
   }
   deriving Show

data KirCtor = KirCtor 
   { name   :: CtorName
   , fields :: [KirFieldDef]
   }
   deriving Show

data KirFieldDef = KirFieldDef
   { name :: FieldName   -- ^ source name for records, @_0@, @_1@... for tuple-style payloads
   , ty   :: Type
   }
   deriving Show

-- | Name given to the n-th field of a tuple-style payload.
tupleFieldName :: Int -> FieldName
tupleFieldName index = Ident (T.pack ('_' : show index))

-- | Every field of a payload with its name, in declaration order.
payloadFields :: Payload -> [(Ident, Type)]
payloadFields = \case
   PUnit          -> []
   PTuple types   -> zip (map tupleFieldName [0 ..]) (map (.node) types)
   PRecord fields -> [(field.name, field.ty.node) | field <- fields]

isTag :: KirTypeDef -> Bool
isTag typeDef = typeDef.mods
   |> any (\case 
      ModTag -> True
      _      -> False
   )

isUnitStruct :: KirTypeDef -> Bool
isUnitStruct td = case td.ctors of
   [single] -> null single.fields
   _        -> False

isEnum :: KirTypeDef -> Bool
isEnum td = td.ctors
   |> all (\ctor -> null ctor.fields)

-- | Several constructors, some with fields, e.g. @type Option a = Some a | None;@
isTaggedUnion :: KirTypeDef -> Bool
isTaggedUnion td = not (isEnum td) && not (isPlainStruct td)

isPlainStruct :: KirTypeDef -> Bool
isPlainStruct td = case td.ctors of
   [single] -> not $ null single.fields
   _        -> False

newtype KirName = KirName { text :: Text }
   deriving (Eq, Ord, Show)

data KirSystem = KirSystem
   { name :: KirName
   , bindings :: [KirBinding]
   , body :: KirBlock
   }
   deriving Show

data KirFunction = KirFunction
   { name :: KirName
   , params :: [KirParam]
   , retTy :: Type
   , body :: KirBlock
   }
   deriving Show

data KirParam = KirParam
   { ty :: Type
   , name :: KirName
   }
   deriving Show

data KirBlock = KirBlock
   { instructs :: [KirInstruct]
   , term :: KirTerm
   }
   deriving Show

data KirBranch = KirBranch
   { instructs :: [KirInstruct]
   , value     :: KirValue
   }
   deriving Show

data KirTag
   = TagLit Literal
   | TagCtor CtorName
   deriving (Eq, Show)

data KirMatchArm = KirMatchArm
   { tag    :: KirTag
   , branch :: KirBranch
   }
   deriving Show

data KirInstruct
   = KirCall
      { ty     :: Type
      , callee :: KirName
      , args   :: [KirValue]
      , out    :: Maybe KirName
      }
   -- | A value built by a constructor, e.g. @Some 1@ or @None@.
   | KirConstruct
      { ty   :: Type        -- ^ the monomorphic type built
      , ctor :: CtorName    -- ^ mangled, e.g. @Std_Option_Option_Std_Num_Int_Some@
      , args :: [KirValue]  -- ^ one per field
      , dst  :: KirName
      }
   | KirAssign
      { binding :: KirBindingId
      , value   :: KirValue
      }
   | KirStore
      { ty    :: Type
      , dest  :: KirDest
      , value :: KirValue
      }
   | KirField 
      { ty     :: Type 
      , src    :: KirValue
      , ctor   :: CtorName
      , fieldName :: FieldName
      , dst    :: KirName
      }
   | KirMatch
      { ty         :: Type
      , scrutinee  :: KirValue
      , arms       :: [KirMatchArm]
      , fallback   :: Maybe KirBranch
      , dest       :: KirDest
      , exhaustive :: Bool
      }
   deriving Show

newtype KirTerm
   = KirReturn { value :: Maybe KirValue }
   deriving Show

data KirValue
   = KirConst KirTag
   | KirVar KirName
   | KirBindingRef KirBindingId
   | KirLocalRef KirLocalId
   | KirUnit  -- ^ the value of something erased (zero-sized); never reaches codegen
   deriving Show

newtype KirLocalId = KirLocalId { inner :: Int }
   deriving (Eq, Ord, Show)

newtype KirBindingId = KirBindingId { inner :: Int }
   deriving (Eq, Ord, Show)

data KirDest
   = DestBinding KirBindingId -- *_bN
   | DestLocal KirLocalId     -- tM
   deriving Show

data KirBinding = KirBinding
   { bindType    :: Type
   , bindSrc     :: KirBindingSrc
   , bindAccess  :: BindingAccess
   }
   deriving Show

data KirBindingSrc
   = SrcEntity
   | SrcSingleton
   deriving Show