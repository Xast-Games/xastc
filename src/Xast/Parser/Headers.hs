{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Headers where

import Text.Megaparsec (sepBy1, between, (<|>), choice, sepEndBy1)
import Xast.Parser.Ident (typeIdent, fnIdent)
import Xast.Parser.Common (Parser, symbol, located, withLoc)
import Xast.AST

module' :: Parser Module
module' = Module <$> typeIdent `sepBy1` "."

moduleDef :: Parser ModuleDef
moduleDef = withLoc $ do
   _        <- symbol "module"
   name     <- module'
   _        <- symbol "exports"
   export   <- located exportPayload

   return $ \location -> ModuleDef {..}

exportPayload :: Parser ExportPayload
exportPayload = choice
   [ ExpSelect <$> between (symbol "{") (symbol "}") (exportItem `sepEndBy1` symbol ",")
   , ExpFull   <$ symbol "*"
   ]

-- | @inc@, @Vec@, @Option(..)@ or @Player(Player, name)@.
exportItem :: Parser ExportItem
exportItem = choice
   [ ExportItem <$> typeIdent <*> (exportMembers <|> pure NoMembers)
   , ExportItem <$> fnIdent <*> pure NoMembers
   ]

exportMembers :: Parser ExportMembers
exportMembers = between (symbol "(") (symbol ")") $ choice
   [ AllMembers  <$ symbol ".."
   , SomeMembers <$> (typeIdent <|> fnIdent) `sepEndBy1` symbol ","
   ]

importDef :: Parser (Located ImportDef)
importDef = located $ do
   _              <- symbol "use"
   importModule   <- module'
   payload        <- importPayload

   return ImportDef {..}

importPayload :: Parser ImportPayload
importPayload = choice
   [ ImpAlias   <$ symbol "as" <*> located typeIdent
   , ImpSelect  <$> between (symbol "{") (symbol "}") (located importIdent `sepEndBy1` symbol ",")
   , ImpFull    <$ symbol "*"
   ]

importIdent :: Parser Ident
importIdent = typeIdent <|> fnIdent
