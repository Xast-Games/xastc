{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Expr where

import Control.Monad.Combinators.Expr
import Data.Text (Text, pack)
import Data.List (foldl', foldl1')
import Text.Megaparsec.Char (char, string, char', string')
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Xast.AST
import Xast.Parser.Ident (varIdent, typeIdent)
import Xast.Parser.Common (Parser, lexeme, symbol, located, withLoc)

pattern' :: Parser (Pattern Parsed)
pattern' = choice
   [ try (withLoc ((\ty ps loc -> PatCon (ParsedInfo loc) ty ps) <$> typeIdent <*> some atomPattern'))
   , atomPattern'
   ]

atomPattern' :: Parser (Pattern Parsed)
atomPattern' = choice
   [ tupleOrParensPat
   , withLoc ((PatWildcard . ParsedInfo) <$ symbol "_")
   , withLoc ((\x loc -> PatVar (ParsedInfo loc) x) <$> varIdent)
   , withLoc ((\ty loc -> PatCon (ParsedInfo loc) ty []) <$> typeIdent)
   , withLoc ((\ps loc -> PatList (ParsedInfo loc) ps) <$> between (symbol "[") (symbol "]") (pattern' `sepBy` symbol ","))
   , withLoc ((\lit loc -> PatLit (ParsedInfo loc) lit) <$> literal)
   ]

tupleOrParensPat :: Parser (Pattern Parsed)
tupleOrParensPat = withLoc $ between (symbol "(") (symbol ")") $ do
   ts <- pattern' `sepBy` symbol ","
   case ts of
      [] -> pure (\loc -> PatTuple (ParsedInfo loc) [])
      [t] -> pure (const t)
      manyT -> pure (\loc -> PatTuple (ParsedInfo loc) manyT)

atomExpr :: Parser (Expr Parsed)
atomExpr = do
   base <- withLoc $ choice
      [ tupleOrParens
      , try ((\mm x loc -> ExpVar (ParsedInfo loc) mm x) <$> optional (try (typeIdent <* symbol ".")) <*> varIdent)
      , try ((\rc loc -> ExpRecConstruct (ParsedInfo loc) rc) <$> recConstruct)
      , (\mm x loc -> ExpCon (ParsedInfo loc) mm x)    <$> optional (try (typeIdent <* symbol ".")) <*> typeIdent
      , (\xs loc -> ExpList (ParsedInfo loc) xs)       <$> between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")
      , (\lit loc -> ExpLit (ParsedInfo loc) lit)      <$> literal
      , (\l loc -> ExpLambda (ParsedInfo loc) l)       <$> lambda
      , (\l loc -> ExpLetIn (ParsedInfo loc) l)        <$> letIn
      , (\i loc -> ExpIfThen (ParsedInfo loc) i)       <$> ifThenElse
      , (\m loc -> ExpMatch (ParsedInfo loc) m)        <$> match'
      ]
   getters <- many (located (try (symbol "." *> varGetter)))
   let based = foldl' applyGetter base getters
   updates <- many (located recUpdateBlock)
   pure $ foldl' applyRecUpdate based updates
   where
      applyGetter l (Located (Location _ offR lenR) getter) =
         let Location posL offL _ = exprLoc l
         in ExpVarGetter (ParsedInfo (Location posL offL ((offR + lenR) - offL))) l getter

      applyRecUpdate l (Located (Location _ offR lenR) assigns) =
         let Location posL offL _ = exprLoc l
         in ExpRecUpdate (ParsedInfo (Location posL offL ((offR + lenR) - offL))) (RecUpdate l assigns)

tupleOrParens :: Parser (Location -> Expr Parsed)
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- expr `sepBy` symbol ","
   case ts of
      [] -> pure (\loc -> ExpTuple (ParsedInfo loc) [])
      [t] -> pure (const t)
      manyT -> pure (\loc -> ExpTuple (ParsedInfo loc) manyT)

recConstruct :: Parser (RecConstruct Parsed)
recConstruct = do
   bind    <- optional (try (typeIdent <* symbol "."))
   con     <- typeIdent
   assigns <- between
      (symbol "{")
      (symbol "}")
      (recAssign `sepEndBy1` symbol ",")

   return RecConstruct {..}

recAssign :: Parser (RecAssign Parsed)
recAssign = RecAssign <$> located varIdent <* symbol "=" <*> expr

recUpdateBlock :: Parser [RecAssign Parsed]
recUpdateBlock = between (symbol "{") (symbol "}") (recAssign `sepEndBy1` symbol ",")

varGetter :: Parser Getter
varGetter = choice
   [ GetTupleField <$> decimalLiteral
   , GetField      <$> varIdent
   ]

match' :: Parser (Match Parsed)
match' = do
   _         <- symbol "match"
   baseExpr  <- expr
   _         <- symbol "with"
   matches   <- matchWing `sepBy1` symbol ","
   let exhaustive = False

   return Match {..}

matchWing :: Parser (MatchWing Parsed)
matchWing = MatchWing <$> pattern' <* symbol "->" <*> expr

term :: Parser (Expr Parsed)
term = do
   atoms <- some atomExpr
   pure $ foldl1' app atoms
   where
      app l r =
         let Location posL offL _ = exprLoc l
             Location _ offR lenR = exprLoc r
         in ExpApp (ParsedInfo (Location posL offL ((offR + lenR) - offL))) l r

opIdent :: BuiltinOp -> Ident
opIdent op = case op of
   OpPlus    -> Ident "opAdd"
   OpMinus   -> Ident "opSub"
   OpMul     -> Ident "opMul"
   OpDiv     -> Ident "opDiv"
   OpMod     -> Ident "opMod"
   OpPow     -> Ident "opPow"
   OpEq      -> Ident "opEq"
   OpNeq     -> Ident "opNeq"
   OpLt      -> Ident "opLt"
   OpGt      -> Ident "opGt"
   OpLe      -> Ident "opLe"
   OpGe      -> Ident "opGe"
   OpAnd     -> Ident "opAnd"
   OpOr      -> Ident "opOr"
   OpNot     -> Ident "opNot"
   OpPipe    -> Ident "opPipe"
   OpApply   -> Ident "opApply"
   OpConcat  -> Ident "opConcat"
   OpNeg     -> Ident "opNeg"
   OpBitAnd  -> Ident "opBitwiseAnd"
   OpBitOr   -> Ident "opBitwiseOr"
   OpBitXor  -> Ident "opBitwiseXor"
   OpShl     -> Ident "opShiftLeft"
   OpShr     -> Ident "opShiftRight"

opVar :: Location -> BuiltinOp -> Expr Parsed
opVar loc = ExpVar (ParsedInfo loc) Nothing . opIdent

opToken :: BuiltinOp -> Text
opToken op = case op of
   OpPlus    -> "+"
   OpMinus   -> "-"
   OpNeg     -> "-"
   OpMul     -> "*"
   OpDiv     -> "/"
   OpMod     -> "%"
   OpPow     -> "**"
   OpEq      -> "=="
   OpNeq     -> "!="
   OpLt      -> "<"
   OpGt      -> ">"
   OpLe      -> "<="
   OpGe      -> ">="
   OpAnd     -> "&&"
   OpOr      -> "||"
   OpNot     -> "!"
   OpPipe    -> "|>"
   OpApply   -> "<|"
   OpConcat  -> "<>"
   OpBitAnd  -> "&"
   OpBitOr   -> "|"
   OpBitXor  -> "^"
   OpShl     -> "<<"
   OpShr     -> ">>"

opLen :: BuiltinOp -> Int
opLen op = case op of
   OpPlus    -> 1
   OpMinus   -> 1
   OpNeg     -> 1
   OpMul     -> 1
   OpDiv     -> 1
   OpMod     -> 1
   OpPow     -> 2
   OpEq      -> 2
   OpNeq     -> 2
   OpLt      -> 1
   OpGt      -> 1
   OpLe      -> 2
   OpGe      -> 2
   OpAnd     -> 2
   OpOr      -> 2
   OpNot     -> 1
   OpPipe    -> 2
   OpApply   -> 2
   OpConcat  -> 2
   OpBitAnd  -> 1
   OpBitOr   -> 1
   OpBitXor  -> 1
   OpShl     -> 2
   OpShr     -> 2

binOp :: Location -> BuiltinOp -> Expr Parsed -> Expr Parsed -> Expr Parsed
binOp opLoc op a b =
   -- Span from start of a to end of b
   let Location posA offA _ = exprLoc a
       Location _ offB lenB = exprLoc b
       totalLen = (offB + lenB) - offA
   in ExpApp
         (ParsedInfo (Location posA offA totalLen))
         (ExpApp (ParsedInfo opLoc) (opVar opLoc op) a)
         b

table :: [[Operator Parser (Expr Parsed)]]
table =
   [  [ Prefix (unaryDirect OpNot)
      , Prefix (unaryDirect OpNeg)
      ]

   ,  [ InfixR (binary OpPow) ]

   ,  [ InfixL (binary OpMul)
      , InfixL (binary OpDiv)
      , InfixL (binary OpMod)
      ]

   ,  [ InfixL (binary OpPlus)
      , InfixL (binary OpMinus)
      ]

   ,  [ InfixL (binary OpShl)
      , InfixL (binary OpShr)
      ]

   ,  [ InfixN (binary OpEq)
      , InfixN (binary OpNeq)
      , InfixN (binary OpLe)
      , InfixN (binary OpGe)
      , InfixN (binaryGuarded OpLt "=|><")
      , InfixN (binaryGuarded OpGt "=>")
      ]

   ,  [ InfixL (binaryGuarded OpBitAnd "&") ]
   ,  [ InfixL (binary OpBitXor) ]
   ,  [ InfixL (binaryGuarded OpBitOr ">|") ]

   ,  [ InfixR (binary OpAnd) ]
   ,  [ InfixR (binary OpOr) ]

   ,  [ InfixL (binary OpPipe) ]
   ,  [ InfixL (binary OpConcat) ]

   ,  [ InfixR applyLeft ]
   ]

binary :: BuiltinOp -> Parser (Expr Parsed -> Expr Parsed -> Expr Parsed)
binary op = do
   pos <- getSourcePos
   off <- getOffset
   _ <- symbol (opToken op)
   let opLoc = Location pos off (opLen op)
   pure (binOp opLoc op)

applyLeft :: Parser (Expr Parsed -> Expr Parsed -> Expr Parsed)
applyLeft = do
   _ <- symbol (opToken OpApply)
   pure $ \f x ->
      let Location posF offF _ = exprLoc f
          Location _ offX lenX = exprLoc x
      in ExpApp (ParsedInfo (Location posF offF ((offX + lenX) - offF))) f x

-- | Like `binary`, but the token must not be immediately followed by any of
-- `forbidden` — used to keep `<`/`>` from swallowing the first char of a
-- longer operator that shares their prefix (`<=`, `<|`, `<>`, `>=`).
binaryGuarded :: BuiltinOp -> [Char] -> Parser (Expr Parsed -> Expr Parsed -> Expr Parsed)
binaryGuarded op forbidden = do
   pos <- getSourcePos
   off <- getOffset
   _ <- lexeme (try (string (opToken op) <* notFollowedBy (satisfy (`elem` forbidden))))
   let opLoc = Location pos off (opLen op)
   pure (binOp opLoc op)

unaryDirect :: BuiltinOp -> Parser (Expr Parsed -> Expr Parsed)
unaryDirect op = do
   pos <- getSourcePos
   off <- getOffset
   _ <- symbol (opToken op)
   let opLoc = Location pos off (opLen op)
   pure $ \x ->
      let Location _ offX lenX = exprLoc x
      in ExpApp (ParsedInfo (Location pos off ((offX + lenX) - off))) (opVar opLoc op) x

expr :: Parser (Expr Parsed)
expr = makeExprParser term table

ifThenElse :: Parser (IfThenElse Parsed)
ifThenElse = do
   _        <- symbol "if"
   ifExpr   <- expr
   _        <- symbol "then"
   thenExpr <- expr
   _        <- symbol "else"
   elseExpr <- expr

   return IfThenElse {..}

lambda :: Parser (Lambda Parsed)
lambda = do
   _        <- symbol ".\\"
   args     <- some pattern'
   _        <- symbol "->"
   body     <- expr

   return Lambda {..}

letIn :: Parser (LetIn Parsed)
letIn = do
   bindings <- let' `sepBy1` symbol ","
   _        <- symbol "in"
   bindExpr <- expr

   return LetIn {..}

let' :: Parser (Let Parsed)
let' = do
   _         <- symbol "let"
   pat       <- pattern'
   _         <- symbol "="
   value     <- expr

   return Let {..}

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
