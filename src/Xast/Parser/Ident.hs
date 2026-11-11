{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Ident where

import Data.Text (Text, pack, unpack)
import Text.Megaparsec.Char (upperChar, lowerChar, alphaNumChar, char)
import Text.Megaparsec

import Xast.Parser.Common
import Xast.AST (Ident(..))

reserved :: [Text]
reserved = 
   -- Keywords
   [ "type", "fn", "let", "in", "if", "then", "else"
   , "match", "system", "with", "extern"
   , "event", "res"
   ]

genericIdent :: Parser Ident
genericIdent = try $ lexeme $ do
   c <- lowerChar
   notFollowedBy alphaNumChar
   return $ Ident (pack [c])

typeIdent :: Parser Ident
typeIdent = try pascalCase

inferIdent :: Parser Ident
inferIdent = try $ Ident <$> symbol "_"

fnIdent :: Parser Ident
fnIdent = try $ do
   ident <- camelCase
   if ident.inner `elem` reserved
      then fail ("keyword `" ++ unpack ident.inner ++ "` is reserved")
      else return ident

varIdent :: Parser Ident
varIdent = try $ do
   ident <- camelCase
   if ident.inner `elem` reserved
      then fail ("keyword `" ++ unpack ident.inner ++ "` is reserved")
      else return ident

pascalCase :: Parser Ident
pascalCase = lexeme $ do
   first <- upperChar
   rest  <- many alphaNumChar
   return $ Ident (pack (first:rest))

-- | A lowercase name; it may contain @'@, as in @map'@, the unique variant of @map@.
camelCase :: Parser Ident
camelCase = lexeme $ do
   first <- lowerChar
   rest  <- many (alphaNumChar <|> char '\'')
   return $ Ident (pack (first:rest))