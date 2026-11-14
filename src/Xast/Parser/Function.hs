{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Function where

import Text.Megaparsec (many)

import Xast.Parser.Type (markedParams, markedResults)
import Xast.Parser.Ident (fnIdent)
import Xast.Parser.Expr (expr, atomPattern')
import Xast.Parser.Common
import Xast.AST
import Xast.Parser.Modifier (modifierOn, noRepeatedModifiers)

func :: Parser (Func Parsed)
func = do
   modifiers <- many (modifierOn OnFunction) >>= noRepeatedModifiers
   if null modifiers
      then (FnDef <$> funcDef modifiers) <-> (FnImpl <$> funcImpl)
      else FnDef <$> funcDef modifiers

funcDef :: [Modifier] -> Parser FuncDef
funcDef modifiers = withLoc $ do
   _           <- symbol "fn"
   name        <- fnIdent
   args     <- markedParams
   _        <- symbol "->"
   results  <- markedResults
   _        <- endOfStmt

   return $ \location -> FuncDef {..}

funcImpl :: Parser (FuncImpl Parsed)
funcImpl = withLoc $ do
   _        <- symbol "fn"
   name     <- fnIdent
   args     <- many atomPattern'
   _        <- symbol "="
   body     <- expr
   _        <- endOfStmt

   return $ \location -> FuncImpl {..}