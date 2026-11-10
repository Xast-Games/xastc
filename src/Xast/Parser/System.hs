{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.System where

import Control.Applicative (optional)
import Data.Maybe (isJust)
import Text.Megaparsec (between, sepBy1, many, some, MonadParsec (lookAhead), choice, sepEndBy1, sepBy)

import Xast.Parser.Common
import Xast.Parser.Ident (typeIdent)
import Xast.Parser.Type (type')
import Xast.Parser.Expr (expr, atomPattern')
import Xast.AST
import Xast.Parser.Modifier (modifierOn, noRepeatedModifiers)

system :: Parser (System Parsed)
system = do
   hasLabel  <- lookAhead (optional (symbol "@label"))
   modifiers <- many (modifierOn OnSystem) >>= noRepeatedModifiers
   if isJust hasLabel || not (null modifiers)
      then SysDef <$> systemDef modifiers
      else (SysDef <$> systemDef modifiers) <-> (SysImpl <$> systemImpl)

systemDef :: [Modifier] -> Parser SystemDef
systemDef modifiers = withLoc $ do
   _           <- symbol "system"
   name        <- typeIdent
   entities    <- many queriedEntity
   _           <- symbol "->"
   retType     <- located type'
   with        <- optional with'
   _           <- endOfStmt

   return $ \location -> SystemDef {..}

queriedEntity :: Parser QueriedEntity
queriedEntity = QueriedEntity <$>
   between (symbol "#(") (symbol ")") (located type' `sepEndBy1` symbol ",")

with' :: Parser [WithType]
with' = symbol "with" *> (withType `sepBy1` symbol ",")
   where
      withType :: Parser WithType
      withType = choice
         [ WithEvent <$ symbol "event" <* symbol ":" <*> located type'
         , WithRes   <$ symbol "res" <* symbol ":" <*> located type'
         ]

systemImpl :: Parser (SystemImpl Parsed)
systemImpl = withLoc $ do
   _           <- symbol "system"
   name        <- typeIdent
   entities    <- entityPattern `sepBy` symbol ","
   with        <- optional $ symbol "with" *> some atomPattern'
   _           <- symbol "="
   body        <- expr
   _           <- endOfStmt

   return $ \location -> SystemImpl {..}

entityPattern :: Parser (EntityPattern Parsed)
entityPattern = EntityPattern <$> some entPatBinding

entPatBinding :: Parser (EntPatBinding Parsed)
entPatBinding = (`EntPatBinding` AccessRead) <$> atomPattern'