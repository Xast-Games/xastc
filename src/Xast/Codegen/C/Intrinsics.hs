{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Codegen.C.Intrinsics where

import qualified Data.Map as M
import Data.Text (Text)

import Xast.AST
import Xast.Codegen.C.Types
import Xast.Utils.Generic (todo__, unreachableWith)

-- | What the compiler knows of one intrinsic.
data IntrinsicDefinition = IntrinsicDefinition
   { arity :: Int                        -- ^ the number of parameters its declaration must have
   , body  :: IntrinsicSite -> [CStmt]  -- ^ the body of its C function at one instance
   }

-- | One instance of an intrinsic, e.g. @opAdd@ on @Int@, as its body sees it.
data IntrinsicSite = IntrinsicSite
   { params      :: [CExpr]  -- ^ the parameters of the C function, in order
   , operandType :: Type     -- ^ the type of the first parameter, e.g. @Int@
   , resultType  :: Type     -- ^ e.g. @Bool@ for @opEq@
   }

-- | Every intrinsic the compiler defines. An extern function marked @Intrinsic
--   must be one of these, with as many parameters.
intrinsicDefinitions :: M.Map FunctionName IntrinsicDefinition
intrinsicDefinitions = M.fromList
   [ (Ident "opAdd",        binary (arithmetic Add))
   , (Ident "opSub",        binary (arithmetic Sub))
   , (Ident "opMul",        binary (arithmetic Mul))
   , (Ident "opDiv",        binary (arithmetic Div))
   , (Ident "opMod",        binary (integral Mod))
   , (Ident "opBitwiseAnd", binary (integral BitAnd))
   , (Ident "opBitwiseOr",  binary (integral BitOr))
   , (Ident "opBitwiseXor", binary (integral BitXor))
   , (Ident "opShiftLeft",  binary (integral Shl))
   , (Ident "opShiftRight", binary (integral Shr))
   , (Ident "opEq",         binary (comparison Eq))
   , (Ident "opNeq",        binary (comparison Ne))
   , (Ident "opLt",         binary (comparison Lt))
   , (Ident "opGt",         binary (comparison Gt))
   , (Ident "opLe",         binary (comparison Le))
   , (Ident "opGe",         binary (comparison Ge))
   , (Ident "opAnd",        binary (logical And))
   , (Ident "opOr",         binary (logical Or))
   , (Ident "opNeg",        unary negation)
   , (Ident "opNot",        unary logicalNot)
   ]

-- | An intrinsic of two parameters whose body returns one expression of them.
binary :: (IntrinsicSite -> CExpr -> CExpr -> CExpr) -> IntrinsicDefinition
binary operation = IntrinsicDefinition
   { arity = 2
   , body  = \site -> case site.params of
      [left, right] -> returning (operation site left right)
      _             -> unreachableWith "a binary intrinsic instance without two parameters"
   }

-- | An intrinsic of one parameter whose body returns one expression of it.
unary :: (IntrinsicSite -> CExpr -> CExpr) -> IntrinsicDefinition
unary operation = IntrinsicDefinition
   { arity = 1
   , body  = \site -> case site.params of
      [operand] -> returning (operation site operand)
      _         -> unreachableWith "a unary intrinsic instance without one parameter"
   }

returning :: CExpr -> [CStmt]
returning result = [CReturn (Just result)]

-- | A C operator that exists for every number type.
arithmetic :: CBinOp -> IntrinsicSite -> CExpr -> CExpr -> CExpr
arithmetic operator _ = CBinary operator

-- | A C operator that exists for integer types only, e.g. @%@.
integral :: CBinOp -> IntrinsicSite -> CExpr -> CExpr -> CExpr
integral operator site left right
   | isIntegralType site.operandType = CBinary operator left right
   | otherwise = todo__
      ("C operator " ++ show operator ++ " does not exist for `" ++ typename site.operandType ++ "`")

-- | A C comparison, whose int result becomes a Bool.
comparison :: CBinOp -> IntrinsicSite -> CExpr -> CExpr -> CExpr
comparison operator site left right =
   boolFromCondition site.resultType (CBinary operator left right)

-- | A C logical operator on two Bools.
logical :: CBinOp -> IntrinsicSite -> CExpr -> CExpr -> CExpr
logical operator site left right =
   let condition = CBinary operator (isTrue site.operandType left) (isTrue site.operandType right)
   in boolFromCondition site.resultType condition

negation :: IntrinsicSite -> CExpr -> CExpr
negation _ = CUnary Neg

logicalNot :: IntrinsicSite -> CExpr -> CExpr
logicalNot site operand =
   boolFromCondition site.resultType (CUnary Not (isTrue site.operandType operand))

-- | A number type that is not a floating-point one.
isIntegralType :: Type -> Bool
isIntegralType = \case
   TyCon qualified -> qualified.name `elem` numberTypeNames && qualified.name `notElem` floatingTypeNames
   _               -> False

floatingTypeNames :: [TypeName]
floatingTypeNames = [Ident "Float", Ident "Double"]

-- | The Bool a C condition stands for. Bool is an enum whose first constructor
--   is @True@, so the int a C condition gives is not a Bool itself.
boolFromCondition
   :: Type   -- ^ the monomorphic Bool type, e.g. @Std_Bool_Bool@
   -> CExpr  -- ^ the C condition
   -> CExpr
boolFromCondition boolType condition = CConditional
   { condition = condition
   , whenTrue  = boolCtor boolType "True"
   , whenFalse = boolCtor boolType "False"
   }

-- | The C condition that a Bool is @True@.
isTrue :: Type -> CExpr -> CExpr
isTrue boolType value = CBinary Eq value (boolCtor boolType "True")

-- | A constructor of the Bool type, named as 'mangleCtor' names it, e.g. @Std_Bool_Bool_True@.
boolCtor
   :: Type  -- ^ the monomorphic Bool type
   -> Text  -- ^ the constructor's own name
   -> CExpr
boolCtor boolType ctorName = case boolType of
   TyCon qualified -> CVar (qualified.name.inner <> "_" <> ctorName)
   other           -> unreachableWith ("a Bool of an intrinsic has type " ++ typename other)
