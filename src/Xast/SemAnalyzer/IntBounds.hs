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
intBounds Byte _                = (-128, 127)
intBounds UByte _               = (0, 255)
intBounds Short _               = (-32768, 32767)
intBounds UShort _              = (0, 65535)
intBounds Int _                 = (-2147483648, 2147483647)
intBounds UInt _                = (0, 4294967295)
intBounds Long _                = (-9223372036854775808, 9223372036854775807)
intBounds ULong _               = (0, 18446744073709551615)
intBounds Size  target@Target32 = intBounds Int target
intBounds USize target@Target32 = intBounds UInt target
intBounds Size  target@Target64 = intBounds Long target
intBounds USize target@Target64 = intBounds ULong target

checkIntegerBounds :: Expr Resolved -> SemAnalyzer ()
checkIntegerBounds = \case
   ExpLit info (LitInt literal) -> checkIntegerLiteralBounds literal info.location

   ExpApp info
      (ExpVar _ _ (Ident "opNeg"))
      (ExpLit _ (LitInt (IntLiteral kind value))) ->
         checkIntegerLiteralBounds
            (IntLiteral kind (negate value))
            info.location
   ExpApp _ operator operand ->
      checkIntegerBounds operator >>
      checkIntegerBounds operand

   ExpLit {} -> pure ()
   ExpVar {} -> pure ()
   ExpCon {} -> pure ()
   ExpTuple _ expressions -> forM_ expressions checkIntegerBounds
   ExpList _ expressions -> forM_ expressions checkIntegerBounds
   ExpLambda _ lambda -> checkIntegerBounds lambda.body
   ExpLetIn _ letIn ->
      checkIntegerBounds letIn.bindExpr >>
      forM_ letIn.bindings (\bind -> checkIntegerBounds bind.value)
   ExpMatch _ match ->
      checkIntegerBounds match.baseExpr >>
      forM_ match.matches (\(MatchWing _ expr) -> checkIntegerBounds expr)
   ExpIfThen _ ifThenElse ->
      checkIntegerBounds ifThenElse.ifExpr >>
      checkIntegerBounds ifThenElse.thenExpr >>
      checkIntegerBounds ifThenElse.elseExpr
   ExpRecConstruct _ recordConstruct -> forM_ recordConstruct.assigns $ \(RecAssign _ expr) -> checkIntegerBounds expr
   ExpRecUpdate _ recordUpdate ->
      checkIntegerBounds recordUpdate.base >>
      forM_ recordUpdate.assigns (\(RecAssign _ expr) -> checkIntegerBounds expr)
   ExpVarGetter _ expr _ -> checkIntegerBounds expr

checkIntegerLiteralBounds :: IntLiteral -> Location -> SemAnalyzer ()
checkIntegerLiteralBounds literal loc = do
   target <- asks (.currentTarget)
   let bounds@(low, high) = intBounds literal.kind target

   unless (literal.value >= low && literal.value <= high) $
      errSem (SEIntegerOutOfBounds loc literal.kind literal.value bounds)
