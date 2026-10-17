module Xast.Codegen.C.Types where

import Data.Text (Text)

data CProgram = CProgram
   { path :: FilePath
   , declarations :: [CDecl]
   }
   deriving Show

data CDecl 
   = CUnitDef CUnitDefinition
   | CStructDef CStructDefinition
   | CTaggedUnionDef CTaggedUnionDefinition
   | CEnumDef CEnumDefinition
   | CFunc CFunction
   | CGlob CGlobal
   deriving Show

data CUnitDefinition
   deriving Show

data CTaggedUnionDefinition = CTaggedUnionDefinition
   { name         :: Text
   , tagType      :: CType             -- tag
   , unionTypes   :: [(Text, CType)]  -- data
   }
   deriving Show

data CStructDefinition = CStructDefinition
   { name   :: Text
   , fields :: [(Text, CType)]
   }
   deriving Show

data CEnumDefinition = CEnumDefinition
   { name      :: Text
   , variants  :: [Text]
   }
   deriving Show

data CFunction = CFunction
   { ty   :: CType
   , name :: Text
   , args :: [CFuncArg]
   , body :: [CStmt]
   }
   deriving Show

data CFuncArg = CFuncArg
   { ty   :: CType
   , name :: Text
   }
   deriving Show

data CGlobal = CGlobal
   { ty     :: CType
   , name   :: Text
   , expr   :: Maybe CExpr
   }
   deriving Show

data CType
   = CVoid     -- void
   | CSize     -- ptrdiff_t
   | CUSize    -- size_t
   | CLong     -- int64_t
   | CInt      -- int32_t
   | CShort    -- int16_t
   | CByte     -- int8_t
   | CULong    -- uint64_t
   | CUInt     -- uint32_t
   | CUShort   -- uint16_t
   | CUByte    -- uint8_t
   | CFloat    -- float
   | CDouble   -- double
   | CPointer CType
   | CStruct Text
   deriving Show

data CExpr
   = CVar Text
   | CIntLit Integer
   | CFloatLit Double
   | CInvoke CExpr [CArg]
   | CBinary CBinOp CExpr CExpr
   | CUnary CUnOp CExpr
   | CAssign CExpr CExpr
   deriving Show

data CArg
   = CExprArg CExpr
   | CTypeArg CType
   deriving Show

data CBinOp
   = Add | Sub | Mul | Div
   | Eq | Ne | Lt | Le | Gt | Ge
   | And | Or
   deriving Show

data CUnOp
   = Neg
   | Not
   | AddrOf
   | Deref
   deriving Show

data CStmt
   = CBlock [CStmt]
   | CReturn (Maybe CExpr)
   | CIf CExpr [CStmt] (Maybe [CStmt])
   | CWhile CExpr CStmt
   | CExprStmt CExpr
   | CDeclStmt CType Text (Maybe CExpr)
   | CSwitch CExpr [(CExpr, [CStmt])] CDefaultBranch
   deriving Show

data CDefaultBranch
   = CDefaultUnreachable
   | CDefaultStmts [CStmt]
   deriving Show