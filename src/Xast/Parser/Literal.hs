{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Literal where

import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text, pack)

import Text.Megaparsec
import Text.Megaparsec.Char (char, char', string, string')

import Xast.AST
import Xast.Parser.Common (Parser, lexeme, symbol, located)

literal :: Parser Literal
literal = choice
   [ tupleOrParensLit
   , LitString <$> stringLiteral
   , LitChar   <$> charLiteral
   , LitFloat  <$> try floatLiteral
   , LitDouble <$> try doubleLiteral
   , LitInt    <$> intLiteral
   , LitList   <$> between (symbol "[") (symbol "]") (located literal `sepBy` symbol ",")
   ]

tupleOrParensLit :: Parser Literal
tupleOrParensLit = between (symbol "(") (symbol ")") $ do
   ts <- located literal `sepBy` symbol ","
   case ts of
      [] -> pure (LitTuple [])
      [t] -> pure t.node
      manyT -> pure (LitTuple manyT)

decimalLiteral :: Parser Int
decimalLiteral = lexeme L.decimal

intLiteral :: Parser IntLiteral
intLiteral = lexeme $ do
   (isHex, value :: Integer) <- choice
      [ (False,) <$> try (string "0b" *> L.binary)
      , (True,)  <$> (string "0x" *> L.hexadecimal)
      , (False,) <$> (string "0o" *> L.octal)
      , (False,) <$> L.decimal
      ]

   let sep = if isHex then "_" else mempty

   kind <- choice
      [ USize  <$ string' (sep <> "uz")
      , ULong  <$ string' (sep <> "ul")
      , UShort <$ string' (sep <> "us")
      , UByte  <$ string' (sep <> "ub")
      , UInt   <$ string' (sep <> "u")
      , Size   <$ string' (sep <> "z")
      , Long   <$ string' (sep <> "l")
      , Short  <$ string' (sep <> "s")
      , Byte   <$ string' (sep <> "b")
      , pure Int
      ]

   return IntLiteral {..}

doubleLiteral :: Parser Double
doubleLiteral = lexeme L.float

floatLiteral :: Parser Float
floatLiteral = lexeme (L.float <* char' 'f')

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = pack <$> lexeme lit
   where lit = char '\"' *> manyTill L.charLiteral (char '\"')
