{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Type where

import Text.Megaparsec (choice, sepBy, between, some, MonadParsec (try), many, sepBy1, sepEndBy)

import Xast.Parser.Ident
import Xast.Parser.Common (Parser, symbol, lexeme, endOfStmt, withLoc, located)
import Xast.AST
import Data.List (foldl1')
import Xast.Parser.Modifier (typeModifier, noRepeatedModifiers)
import Xast.Utils.Generic ((|>))

typeDef :: Parser TypeDef
typeDef = withLoc $ do
   modifiers   <- many typeModifier >>= noRepeatedModifiers
   _           <- symbol "type"
   name        <- typeIdent
   generics    <- many genericIdent
   _           <- symbol "="
   ctors       <- ctor `sepBy1` symbol "|"
   _           <- endOfStmt

   return $ \location -> TypeDef {..}

ctor :: Parser Ctor
ctor = withLoc $ do
   name    <- typeIdent
   payload <- payload'
   return $ \location -> Ctor {..}

payload' :: Parser Payload
payload' = choice
   [ PRecord   <$> between (symbol "{") (symbol "}") (field `sepEndBy` symbol ",")
   , PTuple    <$> try (some (located (lexeme atomType)))
   , PUnit     |> pure
   ]

field :: Parser Field
field = do
   name  <- varIdent
   _     <- symbol ":"
   ty    <- located type'

   return Field {..}

type' :: Parser Type
type' = do
   atoms <- some atomType
   pure (foldl1' TyApp atoms)

atomType :: Parser Type
atomType = choice
   [ tupleOrParens
   , TyFn 
      <$ symbol "fn" 
      <*> between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
      <* symbol "->"
      <*> type'
   , unresolvedType <$> typeIdent
   , TyInt <$> try typeIntrinsic
   , TyGnr <$> genericIdent
   ]

typeIntrinsic :: Parser TyIntrinsic
typeIntrinsic = choice
   [ TyNumber <$ symbol "number"
   , TyConcat <$ symbol "concatenative"
   ]

tupleOrParens :: Parser Type
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- type' `sepBy` symbol ","
   case ts of
      [] -> pure (TyTuple [])
      [t] -> pure t
      manyT -> pure (TyTuple manyT)