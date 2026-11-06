{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Program where

import Data.Bifunctor (Bifunctor(first))
import Data.Text (Text)
import Text.Megaparsec (MonadParsec (lookAhead, eof), many, (<|>), runParser)

import Xast.Parser.Type (typeDef)
import Xast.Parser.Function (func)
import Xast.Parser.Common (Parser, sc)
import Xast.Parser.Headers (moduleDef, importDef)
import Xast.Parser.System (system)
import Xast.Parser.Extern
import Xast.Parser.Modifier (modifier)
import Xast.Error.Types (XastError (XastParseError))
import Xast.AST

parseProgram :: String -> Text -> Either XastError (Program Parsed)
parseProgram filename code = 
   fmap (\p -> p code) $ 
   first XastParseError $ 
   runParser (sc *> program <* eof) filename code

program :: Parser (Text -> Program Parsed)
program = do
   progModuleDef  <- moduleDef
   progImports    <- many importDef
   -- may be empty, e.g. a prelude that only re-exports
   progStmts      <- many stmt

   return $ Program progModuleDef progImports progStmts

stmtKeyword :: Parser Text
stmtKeyword = "extern" <|> "fn" <|> "type" <|> "system"

stmt :: Parser (Stmt Parsed)
stmt = do
   -- A statement may carry modifier annotations (`@Foo(...)`) before its
   -- keyword; skip past any of those to find the keyword without consuming
   -- anything — the chosen branch below re-parses the modifiers for real.
   tok <- lookAhead (many modifier *> stmtKeyword)
   case tok of
      "extern" -> StmtExtern  <$> extern
      "fn"     -> StmtFunc    <$> func
      "type"   -> StmtTypeDef <$> typeDef
      "system" -> StmtSystem  <$> system
      _        -> fail "expected \"extern\", \"fn\", \"type\", \"system\""