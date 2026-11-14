{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Parser.Modifier where

import Text.Megaparsec
import qualified Data.Set as Set
import Xast.Parser.Common (Parser, symbol)
import Xast.AST (Modifier (..), ModifierTarget (..), ComponentDispatchMode (..), displayModifier, displayTarget, modifierTargets)
import Xast.Parser.Ident (fnIdent, typeIdent)
import Xast.Parser.Expr (stringLiteral)

failAt :: Int -> String -> Parser a
failAt offset msg = region (const (FancyError offset (Set.singleton (ErrorFail msg)))) (fail msg)

modifier :: Parser Modifier
modifier = "@" *> choice
   [ ModSharedVariant      <$ symbol "SharedVariant" <*> between (symbol "(") (symbol ")") (fnIdent <?> "shared variant function name")
   , ModMemoize            <$ symbol "Memoize"
   , ModInline             <$ symbol "Inline"
   , ModDeprecated         <$ symbol "Deprecated" <*> between (symbol "(") (symbol ")") (stringLiteral <?> "string literal")
   , ModSupUnreachable     <$ symbol "SuppressUnreachable"
   , ModCompileTime        <$ symbol "CompileTime"
   , ModCompDispatchMode   <$ symbol "Mode" <*> between (symbol "(") (symbol ")") compDispatchMode
   , ModLabel              <$ symbol "Label" <*> between (symbol "(") (symbol ")") (typeIdent <?> "CamelCase system pipeline label ident")
   , ModParallel           <$ symbol "Parallel"
   , ModSingleton          <$ symbol "Singleton"
   , ModCopyable           <$ symbol "Copyable" <*> optional (between (symbol "(") (symbol ")") (fnIdent <?> "copy function name"))
   , ModPod                <$ symbol "Pod"
   , ModDrop               <$ symbol "Drop" <*> between (symbol "(") (symbol ")") (fnIdent <?> "drop function name")
   , ModTag                <$ symbol "Tag"
   , ModNonExhaustive      <$ symbol "NonExhaustive"
   , ModIntrinsic          <$ symbol "Intrinsic"
   ]

-- | A modifier that may be written on the given kind of declaration.
modifierOn :: ModifierTarget -> Parser Modifier
modifierOn target = do
   start <- getOffset
   parsed <- modifier <?> "invalid modifier"
   if target `elem` modifierTargets parsed
      then pure parsed
      else failAt start $
         "modifier `" ++ displayModifier parsed ++ "` cannot be used on " ++ displayTarget target

compDispatchMode :: Parser ComponentDispatchMode
compDispatchMode = choice
   [ CDMDynamic   <$ symbol "Dynamic"
   , CDMSafe      <$ symbol "Safe"
   , CDMStrict    <$ symbol "Strict"
   ] <?> "a valid component dispatch mode (Dynamic, Safe, or Strict)"

-- | Fails the parse if a modifier is written twice, e.g. @\@Label(A) \@Label(B)@.
noRepeatedModifiers :: [Modifier] -> Parser [Modifier]
noRepeatedModifiers mods = case firstRepeated (map displayModifier mods) of
   Just name -> fail ("modifier `" ++ name ++ "` cannot be used more than once")
   Nothing   -> pure mods

firstRepeated :: [String] -> Maybe String
firstRepeated = \case
   []            -> Nothing
   name : rest
      | name `elem` rest -> Just name
      | otherwise        -> firstRepeated rest
