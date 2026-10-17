{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Xast.SemAnalyzer.IntBounds where

import Control.Monad (forM_, unless)
import Xast.AST
import Xast.Error.Types
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.Utils.Compiler (Target (Target32, Target64))
import Control.Monad.RWS (asks)

-- #### Type checking ####

intBounds :: IntKind -> Target -> (Integer, Integer)
intBounds Byte _   = (-128, 127)
intBounds UByte _  = (0, 255)
intBounds Short _  = (-32768, 32767)
intBounds UShort _ = (0, 65535)
intBounds Int _    = (-2147483648, 2147483647)
intBounds UInt _   = (0, 4294967295)
intBounds Long _   = (-9223372036854775808, 9223372036854775807)
intBounds ULong _  = (0, 18446744073709551615)
intBounds Size  t@Target32 = intBounds Int t
intBounds USize t@Target32 = intBounds UInt t
intBounds Size  t@Target64 = intBounds Long t
intBounds USize t@Target64 = intBounds ULong t

checkIntegerBounds :: Expr Resolved -> SemAnalyzer ()
checkIntegerBounds = \case
   ExpLit res (LitInt lit) -> checkIntegerLiteralBounds lit res.location

   ExpApp res
      (ExpVar _ _ (Ident "opNeg"))
      (ExpLit _ (LitInt (IntLiteral kind value))) ->
         checkIntegerLiteralBounds
            (IntLiteral kind (negate value))
            res.location
   ExpApp _ op opnd ->
      checkIntegerBounds op >>
      checkIntegerBounds opnd

   ExpLit {} -> pure ()
   ExpVar {} -> pure ()
   ExpCon {} -> pure ()
   ExpTuple _ xs -> forM_ xs checkIntegerBounds
   ExpList _ xs -> forM_ xs checkIntegerBounds
   ExpLambda _ lambda -> checkIntegerBounds lambda.body
   ExpLetIn _ letIn ->
      checkIntegerBounds letIn.bindExpr >>
      forM_ letIn.bindings (\bind -> checkIntegerBounds bind.value)
   ExpMatch _ match ->
      checkIntegerBounds match.baseExpr >>
      forM_ match.matches (\(MatchWing _ expr) -> checkIntegerBounds expr)
   ExpIfThen _ ite ->
      checkIntegerBounds ite.ifExpr >>
      checkIntegerBounds ite.thenExpr >>
      checkIntegerBounds ite.elseExpr
   ExpRecConstruct _ recCon -> forM_ recCon.assigns $ \(RecAssign _ expr) -> checkIntegerBounds expr
   ExpRecUpdate _ recUpd ->
      checkIntegerBounds recUpd.base >>
      forM_ recUpd.assigns (\(RecAssign _ expr) -> checkIntegerBounds expr)
   ExpVarGetter _ expr _ -> checkIntegerBounds expr

checkIntegerLiteralBounds :: IntLiteral -> Location -> SemAnalyzer ()
checkIntegerLiteralBounds lit loc = do
   target <- asks (.currentTarget)
   let bounds@(low, high) = intBounds lit.kind target

   unless (lit.value >= low && lit.value <= high) $
      errSem (SEIntegerOutOfBounds loc lit.kind lit.value bounds)
