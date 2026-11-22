{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Html
   ( Html(..)
   , render
   , renderDocument
   -- basic elements
   , html, head_, body, title_, style_, script_, meta
   , div_, span_, pre_, p, hr
   , h1, h2, h3
   , text, raw
   -- typed-AST rendering
   , renderTypedSource
   , renderTypedProgram
   , collectTokens
   , collectProgramTokens
   , typedAstStyle
   , typedAstPage
   ) where

import qualified Data.Text as T

import Data.List (sortOn)
import Data.Text (Text)

import Xast.AST

data Html
   = Element Text [(Text, Text)] [Html]
   | VoidElement Text [(Text, Text)]
   | TextNode Text
   | RawHtml Text

-- #### RENDERING ####

escapeText :: Text -> Text
escapeText = T.concatMap $ \case
   '&' -> "&amp;"
   '<' -> "&lt;"
   '>' -> "&gt;"
   c   -> T.singleton c

escapeAttr :: Text -> Text
escapeAttr = T.concatMap $ \case
   '&'  -> "&amp;"
   '"'  -> "&quot;"
   '<'  -> "&lt;"
   '>'  -> "&gt;"
   c    -> T.singleton c

renderAttrs :: [(Text, Text)] -> Text
renderAttrs = foldMap $ \(k, v) -> " " <> k <> "=\"" <> escapeAttr v <> "\""

render :: Html -> Text
render (Element name attrs inner) =
   "<" <> name <> renderAttrs attrs <> ">" <> foldMap render inner <> "</" <> name <> ">"
render (VoidElement name attrs) =
   "<" <> name <> renderAttrs attrs <> "/>"
render (TextNode txt) = escapeText txt
render (RawHtml txt) = txt

renderDocument :: Html -> Text
renderDocument h = "<!DOCTYPE html>" <> render h

-- #### ELEMENTS ####

hr :: Html
hr = VoidElement "hr" []

meta :: [(Text, Text)] -> Html
meta = VoidElement "meta"

html :: [Html] -> Html
html = Element "html" []

head_ :: [Html] -> Html
head_ = Element "head" []

body :: [Html] -> Html
body = Element "body" []

title_ :: Text -> Html
title_ t = Element "title" [] [text t]

style_ :: Text -> Html
style_ css = Element "style" [] [raw css]

script_ :: Text -> Html
script_ js = Element "script" [] [raw js]

div_ :: [(Text, Text)] -> [Html] -> Html
div_ = Element "div"

span_ :: [(Text, Text)] -> [Html] -> Html
span_ = Element "span"

pre_ :: [(Text, Text)] -> [Html] -> Html
pre_ = Element "pre"

p :: [Html] -> Html
p = Element "p" []

h1 :: Text -> Html
h1 t = Element "h1" [] [text t]

h2 :: Text -> Html
h2 t = Element "h2" [] [text t]

h3 :: Text -> Html
h3 t = Element "h3" [] [text t]

text :: Text -> Html
text = TextNode

raw :: Text -> Html
raw = RawHtml

-- #### TYPED AST RENDERING ####

collectTokens :: Expr Typed -> [(Location, Type)]
collectTokens node = case node of
   ExpVar info _ _ ->
      [(info.location, info.ty)]

   ExpCon info _ _ ->
      [(info.location, info.ty)]

   ExpLit info _ ->
      [(info.location, info.ty)]

   ExpTuple _ xs ->
      concatMap collectTokens xs

   ExpList _ xs ->
      concatMap collectTokens xs

   ExpLambda _ (Lambda pats lamBody) ->
      concatMap (collectPatternTokens . (.pat)) pats ++ collectTokens lamBody

   ExpApp _ f x ->
      collectTokens f ++ collectTokens x

   ExpLetIn _ (LetIn binds bodyExpr) ->
      concatMap (\(Let pat value) -> collectPatternTokens pat ++ collectTokens value) binds
         ++ collectTokens bodyExpr

   ExpMatch _ (Match scrut wings _) ->
      collectTokens scrut ++ concatMap (\(MatchWing pat e) -> collectPatternTokens pat ++ collectTokens e) wings

   ExpIfThen _ (IfThenElse c t e) ->
      collectTokens c ++ collectTokens t ++ collectTokens e

   ExpRecConstruct _ (RecConstruct _ _ assigns) ->
      concatMap (\(RecAssign _ e) -> collectTokens e) assigns

   ExpRecUpdate _ (RecUpdate base assigns) ->
      collectTokens base ++ concatMap (\(RecAssign _ e) -> collectTokens e) assigns

   ExpVarGetter _ e _ ->
      collectTokens e

collectPatternTokens :: Pattern Typed -> [(Location, Type)]
collectPatternTokens node = case node of
   PatVar info _ ->
      [(info.location, info.ty)]

   PatWildcard info ->
      [(info.location, info.ty)]

   PatLit info _ ->
      [(info.location, info.ty)]

   PatList _ ps ->
      concatMap collectPatternTokens ps

   PatTuple _ ps ->
      concatMap collectPatternTokens ps

   PatCon _ _ ps ->
      concatMap collectPatternTokens ps

collectProgramTokens :: Program Typed -> [(Location, Type)]
collectProgramTokens (Program _ _ stmts _) = concatMap collectStmtTokens stmts

collectStmtTokens :: Stmt Typed -> [(Location, Type)]
collectStmtTokens = \case
   StmtFunc (FnImpl (FuncImpl _ _ pats funcBody)) ->
      concatMap collectPatternTokens pats ++ collectTokens funcBody

   StmtSystem (SysImpl (SystemImpl _ _ entPats mWith sysBody)) ->
      concatMap (\(EntityPattern bindings) -> concatMap (collectPatternTokens . (.pat)) bindings) entPats
         ++ maybe [] (concatMap collectPatternTokens) mWith
         ++ collectTokens sysBody

   _ -> []

renderSourceWithTokens :: Text -> [(Location, Type)] -> [Html]
renderSourceWithTokens src tokens = go 0 (sortOn ((.offset) . fst) tokens)
   where
      srcLen = T.length src

      go pos [] =
         [text (T.drop pos src) | pos < srcLen]

      go pos ((loc, ty) : rest)
         | off < pos = go pos rest -- overlapping/out-of-order token, skip defensively
         | otherwise = gapHtml ++ [tokenSpan tokTxt (T.pack (typename ty))] ++ go (off + len) rest
         where
            off = loc.offset
            len = loc.length
            gap = T.take (off - pos) (T.drop pos src)
            gapHtml = [text gap | not (T.null gap)]
            tokTxt = T.take len (T.drop off src)

tokenSpan :: Text -> Text -> Html
tokenSpan tokTxt ty = span_ [("class", "tok"), ("data-type", ty), ("tabindex", "0")] [text tokTxt]

renderTypedSource :: Text -> Expr Typed -> Html
renderTypedSource src expr = pre_ [("class", "code")] (renderSourceWithTokens src (collectTokens expr))

renderTypedProgram :: Program Typed -> Html
renderTypedProgram prog = pre_ [("class", "code")] (renderSourceWithTokens prog.src (collectProgramTokens prog))

typedAstStyle :: Text
typedAstStyle = T.unlines
   [ "body { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; background: #1e1e1e; color: #d4d4d4; padding: 2rem; }"
   , "pre.code { white-space: pre-wrap; font-size: 14px; line-height: 1.7; margin: 0; }"
   , ".tok { border-bottom: 1px dashed #569cd6; cursor: pointer; position: relative; border-radius: 2px; }"
   , ".tok:hover, .tok:focus { outline: none; background: rgba(86, 156, 214, 0.18); }"
   , ".tok:hover::after, .tok:focus::after {"
   , "  content: attr(data-type);"
   , "  position: absolute;"
   , "  bottom: 125%;"
   , "  left: 0;"
   , "  background: #252526;"
   , "  color: #4ec9b0;"
   , "  border: 1px solid #569cd6;"
   , "  padding: 2px 8px;"
   , "  border-radius: 4px;"
   , "  white-space: nowrap;"
   , "  font-size: 12px;"
   , "  z-index: 10;"
   , "}"
   ]

typedAstPage :: Text -> Html -> Html
typedAstPage pageTitle body_ = html
   [ head_ [ meta [("charset", "utf-8")], title_ pageTitle, style_ typedAstStyle ]
   , body [ h1 pageTitle, body_ ]
   ]
