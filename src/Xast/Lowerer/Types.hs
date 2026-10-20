{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Types where

import Xast.AST
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Xast.Utils.Generic ((|>))

data LowerState = LowerState
   { nameSupply :: Int
   , typeTmpls  :: M.Map TypeName TypeDef
   , typeInsts  :: M.Map TypeInst KirName
   , typeQueue  :: [TypeInst]
   , ctorSpans  :: M.Map ConstructorId Int
   , ctorFieldNames :: M.Map CtorName [FieldName]  -- ^ declared field names of each constructor
   }

data TypeInst = TypeInst
   { name :: Ident
   , args :: [Type]
   }
   deriving (Eq, Ord, Show)

newLowerState 
   :: M.Map ConstructorId Int 
   -> [Program Desugared]
   -> LowerState
newLowerState spans progs = LowerState 
   { nameSupply   = 0
   , ctorSpans    = spans
   , typeTmpls    = M.fromList
      [ (td.name, td) | p <- progs, StmtTypeDef td <- p.stmts ] 
   , typeInsts    = M.empty
   , typeQueue    = []
   , ctorFieldNames = M.fromList
      [ (ctor.name, map fst (payloadFields ctor.payload))
      | p <- progs, StmtTypeDef td <- p.stmts, ctor <- td.ctors ]
   }

-- KIRA = Khast Intermediate RepresentAtion
data Kira = Kira
   { moduleName :: Module
   , systems    :: [KirSystem]
   , functions  :: [KirFunction]
   }
   deriving Show

data KirTypeDef = KirTypeDef
   { name  :: KirName
   , ctors :: [KirCtor]
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

isUnitStruct :: KirTypeDef -> Bool
isUnitStruct td = case td.ctors of
   [single] -> null single.fields
   _        -> False

isEnum :: KirTypeDef -> Bool
isEnum td = td.ctors
   |> filter (\ctor -> not $ null ctor.fields)
   |> null

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
      , out    :: KirName
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
      , out    :: KirName
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
   deriving Show

newtype KirLocalId = KirLocalId { inner :: Int }
   deriving Show

newtype KirBindingId = KirBindingId { inner :: Int }
   deriving Show

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