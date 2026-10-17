{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Codegen.C.Pretty where

import Xast.Codegen.C.Types
import Prettyprinter (Doc, Pretty (pretty), hardline, (<+>), encloseSep, vsep, indent, enclose)
import Xast.Utils.Pretty (bold, cyan)
import Data.List (intercalate)
import Data.Text (Text)

debugPrograms :: [CProgram] -> String
debugPrograms progs = intercalate "\n\n" $ flip map progs $ \prog ->
   let path = show $ bold $ cyan prog.path
       code = show $ prettyProgram prog
   in path ++ ":\n" ++ code

header :: Doc ann
header = 
   "#include <stdint.h>" <> hardline 
   -- <> "#include <flecs.h>" <> hardline <> hardline

prettyProgram :: CProgram -> Doc ann
prettyProgram prog = 
   header <>
   foldMap prettyDecl prog.declarations

prettyDecl :: CDecl -> Doc ann
prettyDecl (CFunc func) = prettyFunction func <> hardline
prettyDecl (CGlob glob) = prettyGlobal glob <> hardline
prettyDecl (CStructDef struct) = prettyStructDef struct <> hardline
prettyDecl (CEnumDef enum) = prettyEnumDef enum <> hardline
prettyDecl (CTaggedUnionDef taggedUnion) = prettyTaggedUnionDef taggedUnion <> hardline

prettyTaggedUnionDef :: CTaggedUnionDefinition -> Doc ann
prettyTaggedUnionDef taggedUnion = 
   "typedef struct" <+>
   vsep
      [ "{"
      , indent 4 (prettyType taggedUnion.tagType <+> "tag;")
      , hardline
      , indent 4 (prettyUnion taggedUnion.unionTypes)
      , "}"
      ] <+>
   pretty taggedUnion.name <> ";"

prettyUnion :: [(Text, CType)] -> Doc ann
prettyUnion unionTypes = 
   "union" <+>
   vsep
      [ "{"
      , indent 4 (vsep (map prettyField unionTypes))
      , "}"
      ] <+>
   "data;"

prettyEnumDef :: CEnumDefinition -> Doc ann
prettyEnumDef enum = 
   "typedef enum" <+>
   vsep 
      [ "{"
      , indent 4 (vsep (map prettyEnumVariant enum.variants))
      , "}"
      ] <+>
   pretty enum.name <> ";"

prettyEnumVariant :: Text -> Doc ann
prettyEnumVariant variant = 
   pretty variant <> ","

prettyStructDef :: CStructDefinition -> Doc ann
prettyStructDef struct = 
   "typedef struct" <+>
   vsep 
      [ "{"
      , indent 4 (vsep (map prettyField struct.fields))
      , "}"
      ] <+>
   pretty struct.name <> ";"

prettyField :: (Text, CType) -> Doc ann
prettyField (name, ty) = 
   prettyType ty <+> pretty name <> ";"

prettyFunction :: CFunction -> Doc ann
prettyFunction func = 
   prettyType func.ty <+>
   pretty func.name <>
   encloseSep "(" ")" ", " (map prettyArg func.args) <+>
   vsep 
      [ "{"
      , indent 4 (vsep (map prettyStmt func.body))
      , "}"
      ]

prettyStmt :: CStmt -> Doc ann
prettyStmt = \case
   CBlock stmts -> vsep 
      [ "{"
      , indent 4 (vsep (map prettyStmt stmts))
      , "}"
      ]
   CReturn Nothing -> "return;"
   CReturn (Just e) -> "return" <+> prettyExpr e <> ";"
   CExprStmt e -> prettyExpr e <> ";"
   CDeclStmt ty name Nothing -> prettyType ty <+> pretty name <> ";"
   CDeclStmt ty name (Just e) -> prettyType ty <+> pretty name <+> "=" <+> prettyExpr e <> ";"
   CIf cond if' Nothing ->
      "if" <+> enclose "(" ")" (prettyExpr cond) <+> vsep 
         [ "{"
         , indent 4 (vsep (map prettyStmt if'))
         , "}"
         ]
   CIf cond if' (Just else') ->
      "if" <+> enclose "(" ")" (prettyExpr cond) <+> vsep 
         [ "{"
         , indent 4 (vsep (map prettyStmt if'))
         , "}"
         ] <+>
      "else" <+> vsep 
         [ "{"
         , indent 4 (vsep (map prettyStmt else'))
         , "}"
         ]
   CWhile _ _ -> undefined

   CSwitch scrut cases defaultBranch ->
      "switch" <+> enclose "(" ")" (prettyExpr scrut) <+> vsep
         [ "{"
         , indent 4 (vsep (map prettyCase cases ++ [prettyDefault defaultBranch]))
         , "}"
         ]
      where
         prettyCase (lbl, stmts) = vsep
            [ "case" <+> prettyExpr lbl <> ": {"
            , indent 4 (vsep (map prettyStmt stmts ++ ["break;"]))
            , "}"
            ]
         prettyDefault CDefaultUnreachable = vsep
            [ "default:"
            , indent 4 "__builtin_unreachable();"
            ]
         prettyDefault (CDefaultStmts stmts) = vsep
            [ "default: {"
            , indent 4 (vsep (map prettyStmt stmts ++ ["break;"]))
            , "}"
            ]


prettyExpr :: CExpr -> Doc ann
prettyExpr = \case
   CVar name            -> pretty name
   CIntLit int          -> pretty int
   CFloatLit float      -> pretty float
   CInvoke caller args  -> prettyExpr caller <> encloseSep "(" ")" ", " (map prettyCArg args)
   CBinary op a b       -> prettyExpr a <+> prettyBinOp op <+> prettyExpr b
   CUnary op a          -> prettyUnOp op <> prettyExpr a
   CAssign left right   -> prettyExpr left <+> "=" <+> prettyExpr right

prettyCArg :: CArg -> Doc ann
prettyCArg = \case
   CExprArg expr -> prettyExpr expr
   CTypeArg ty -> prettyType ty

prettyBinOp :: CBinOp -> Doc ann
prettyBinOp = \case
   Add -> "+"
   Sub -> "-"
   Mul -> "*"
   Div -> "/"
   Eq -> "=="
   Ne -> "!="
   Lt -> "<"
   Le -> "<="
   Gt -> ">"
   Ge -> ">="
   And -> "&&"
   Or -> "||"

prettyUnOp :: CUnOp -> Doc ann
prettyUnOp = \case
   Neg -> "-"
   Not -> "!"
   AddrOf -> "&"
   Deref -> "*"

prettyArg :: CFuncArg -> Doc ann
prettyArg arg = prettyType arg.ty <+> pretty arg.name

prettyGlobal :: CGlobal -> Doc ann
prettyGlobal _global = "/* globals are not implemented */"

prettyType :: CType -> Doc ann
prettyType = \case
   CVoid    -> "void"
   CSize    -> "ptrdiff_t"
   CUSize   -> "size_t"
   CLong    -> "int64_t"
   CInt     -> "int32_t"
   CShort   -> "int16_t"
   CByte    -> "int8_t"
   CULong   -> "uint64_t"
   CUInt    -> "uint32_t"
   CUShort  -> "uint16_t"
   CUByte   -> "uint8_t"
   CFloat   -> "float"
   CDouble  -> "double"
   CPointer ty    -> prettyType ty <> "*"
   CStruct name   -> pretty name