{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Type where

import Data.List (foldl1')

import Text.Megaparsec (choice, sepBy, between, some, MonadParsec (try), many, sepBy1, sepEndBy)

import Xast.AST
import Xast.Parser.Common (Parser, symbol, lexeme, endOfStmt, withLoc, located)
import Xast.Parser.Ident
import Xast.Parser.Modifier (modifierOn, noRepeatedModifiers)
import Xast.Utils.Generic ((|>))

typeDef :: Parser TypeDef
typeDef = withLoc $ do
   modifiers   <- many (modifierOn OnType) >>= noRepeatedModifiers
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
   , fnType
   , unresolvedType <$> typeIdent
   , TyInt <$> try typeIntrinsic
   , TyGnr <$> genericIdent
   ]

-- | @fn(Int, *Vec a) -> *Vec a@.
fnType :: Parser Type
fnType = do
   _       <- symbol "fn"
   params  <- markedParams
   _       <- symbol "->"
   results <- markedResults
   pure (functionType (map (.node) params) (map (.node) results))

-- | Parameters in parentheses, each a type with an optional @*@: @(Int, *Vec a)@.
markedParams :: Parser [Located MarkedType]
markedParams = between (symbol "(") (symbol ")") (markedType `sepBy` symbol ",")

-- | One result, @*Vec a@, or a tuple of results with a @*@: @(Int, *Console)@.
markedResults :: Parser [Located MarkedType]
markedResults = choice
   [ try markedTuple
   , (: []) <$> markedType
   ]

markedTuple :: Parser [Located MarkedType]
markedTuple = do
   elements <- between (symbol "(") (symbol ")") (markedType `sepBy1` symbol ",")
   if length elements < 2 || all ((== Read) . (.mark) . (.node)) elements
      then fail "not a tuple of results with a `*`"
      else pure elements

mark' :: Parser Mark
mark' = choice
   [ Take <$ symbol "*"
   -- , OtherMarks <$ symbol "?"
   , Read |> pure
   ]

-- | A type with an optional @*@ in front.
markedType :: Parser (Located MarkedType)
markedType = located $ do
   mark <- mark'
   ty   <- type'
   pure MarkedType { .. }

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