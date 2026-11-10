{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Extern where

import Text.Megaparsec (sepBy, between, many)

import Xast.AST
import Xast.Parser.Type (type')
import Xast.Parser.Ident
import Xast.Parser.Common (endOfStmt, Parser, symbol, (<->), withLoc, located)
import Xast.Parser.Modifier (modifierOn, noRepeatedModifiers)

extern :: Parser Extern
extern = (ExtFunc <$> externFunc) <-> (ExtType <$> externType)

externFunc :: Parser ExternFunc
externFunc = do
   modifiers <- many (modifierOn OnExternFunction) >>= noRepeatedModifiers
   externFuncDef modifiers

externFuncDef :: [Modifier] -> Parser ExternFunc
externFuncDef modifiers = withLoc $ do
   _        <- symbol "extern"
   _        <- symbol "fn"
   name     <- fnIdent
   args     <- between (symbol "(") (symbol ")") (located type' `sepBy` symbol ",")
   _        <- symbol "->"
   retType  <- located type'
   _        <- endOfStmt

   return $ \location -> ExternFunc {..}

externType :: Parser ExternType
externType = withLoc $ do
   _           <- symbol "extern"
   _           <- symbol "type"
   name        <- typeIdent
   generics    <- many genericIdent
   _           <- endOfStmt

   return $ \location -> ExternType {..}