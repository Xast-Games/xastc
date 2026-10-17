module Xast.Lowerer.Types where

import Xast.AST
import Data.Text (Text)
import qualified Data.Map as M

data LowerState = LowerState
   { nameSupply :: Int
   , typeTmpls  :: M.Map Ident TypeDef
   , typeInsts  :: M.Map TypeInst KirName
   , typeQueue  :: [TypeInst]
   , ctorSpans  :: M.Map ConstructorId Int
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
   { name   :: Ident
   , fields :: [Type]
   }
   deriving Show

newtype KirName = KirName Text
   deriving Show

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
   | TagCtor Ident
   deriving (Eq, Show)

data KirMatchArm = KirMatchArm
   { tag    :: KirTag
   , branch :: KirBranch
   }
   deriving Show

data KirInstruct
   = KirCall Type KirName [KirValue] KirName
   | KirAssign KirBindingId KirValue
   | KirStore Type KirDest KirValue 
   | KirField 
      { ty     :: Type 
      , src    :: KirValue
      , ctor   :: Ident 
      , plIdx  :: Int 
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
   = KirReturn (Maybe KirValue)
   deriving Show

data KirValue
   = KirConst KirTag
   | KirVar KirName
   | KirBindingRef KirBindingId
   | KirLocalRef KirLocalId
   deriving Show

newtype KirLocalId = KirLocalId Int
   deriving Show

newtype KirBindingId = KirBindingId Int
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