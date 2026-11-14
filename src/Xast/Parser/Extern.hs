{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Extern where

import Text.Megaparsec (many)

import Xast.AST
import Xast.Parser.Type (markedParams, markedResults)
import Xast.Parser.Ident
import Xast.Parser.Common (endOfStmt, Parser, symbol, (<->), withLoc)
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
   args     <- markedParams
   _        <- symbol "->"
   results  <- markedResults
   _        <- endOfStmt

   return $ \location -> ExternFunc {..}

externType :: Parser ExternType
externType = withLoc $ do
   modifiers   <- many (modifierOn OnExternType) >>= noRepeatedModifiers
   _           <- symbol "extern"
   _           <- symbol "type"
   name        <- typeIdent
   generics    <- many genericIdent
   _           <- endOfStmt

   return $ \location -> ExternType {..}