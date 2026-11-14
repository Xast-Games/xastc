{-# LANGUAGE OverloadedStrings #-}
module Xast.Intrinsics where

import qualified Data.Map as M

import Xast.AST

-- | Every intrinsic the compiler knows, with its parameter count.
--   Each @Intrinsic declaration must match one; each backend must implement all.
intrinsicArities :: M.Map FunctionName Int
intrinsicArities = M.fromList
   [ (Ident "opAdd",        2)
   , (Ident "opSub",        2)
   , (Ident "opMul",        2)
   , (Ident "opDiv",        2)
   , (Ident "opMod",        2)
   , (Ident "opBitwiseAnd", 2)
   , (Ident "opBitwiseOr",  2)
   , (Ident "opBitwiseXor", 2)
   , (Ident "opShiftLeft",  2)
   , (Ident "opShiftRight", 2)
   , (Ident "opEq",         2)
   , (Ident "opNeq",        2)
   , (Ident "opLt",         2)
   , (Ident "opGt",         2)
   , (Ident "opLe",         2)
   , (Ident "opGe",         2)
   , (Ident "opAnd",        2)
   , (Ident "opOr",         2)
   , (Ident "opNeg",        1)
   , (Ident "opNot",        1)
   ]
