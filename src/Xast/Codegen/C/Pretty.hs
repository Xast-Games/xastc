{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Codegen.C.Pretty where

import Data.List (intercalate)
import Data.Text (Text)
import Prettyprinter

import qualified Xast.Utils.List as L
import Xast.Utils.Pretty (bold, cyan)
import Xast.Lowerer.Types (KirName(..))
import Xast.Codegen.C.Types
import Xast.Utils.Generic ((|>))
import Xast.Codegen.C.Pass (typeDefsFilename)

debugPrograms :: [CProgram] -> String
debugPrograms programs = intercalate "\n\n" $ 
   L.forEach programs $ \program ->
      let path = show $ bold $ cyan program.path
          code = show $ prettyProgram program
      in path ++ ":\n" ++ code

header :: Doc ann
header = 
   "#pragma once" <> hardline <>
   "#include <stdint.h>" <> hardline <>
   "#include <flecs.h>" <> hardline

includeTypes :: Doc ann
includeTypes = prettyInclude typeDefsFilename

-- | E.g. @#include "src/Geometry.h"@.
prettyInclude :: FilePath -> Doc ann
prettyInclude path = "#include \"" <> pretty path <> "\"" <> hardline

prettyProgram :: CProgram -> Doc ann
prettyProgram program = 
   let typeHeader = 
         if program.path /= typeDefsFilename then
            includeTypes
         else
            mempty
       moduleHeaders = foldMap prettyInclude program.includes
   in header 
   <> typeHeader
   <> moduleHeaders
   <> foldMap prettyDecl program.declarations

prettyDecl :: CDecl -> Doc ann
prettyDecl (CFunc func) = prettyFunction func <> hardline
prettyDecl (CFuncPrototype func) = prettySignature func <> ";" <> hardline
prettyDecl (CGlob global) = prettyGlobal global <> hardline
prettyDecl (CTagDef ecsTag) = prettyEcsTag ecsTag <> hardline
prettyDecl (CStructDef struct) = prettyStructDef struct <> hardline
prettyDecl (CEnumDef enum) = prettyEnumDef enum <> hardline
prettyDecl (CTaggedUnionDef taggedUnion) = prettyTaggedUnionDef taggedUnion <> hardline

prettyEcsTag :: EcsTag -> Doc ann
prettyEcsTag tag = "ECS_TAG(" <> pretty tag.text <> ");"

prettyTaggedUnionDef :: CTaggedUnionDefinition -> Doc ann
prettyTaggedUnionDef taggedUnion = 
   "typedef struct" <+>
   vsep
      [ "{"
      , indent 4 (prettyType taggedUnion.tagType <+> "tag;")
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
   "as;"

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
prettyField (name, fieldType) = 
   prettyType fieldType <+> pretty name <> ";"

prettyFunction :: CFunction -> Doc ann
prettyFunction func = 
   prettySignature func <+>
   vsep 
      [ "{"
      , indent 4 (vsep (map prettyStmt func.body))
      , "}"
      ]

prettySignature :: CFunction -> Doc ann
prettySignature func =
   let args = func.args
         |> map prettyArg
         |> commaSeparated
   in prettyLinkage func.linkage <> prettyType func.ty <+> pretty func.name <> parens args

prettyLinkage :: CLinkage -> Doc ann
prettyLinkage = \case
   CExternal     -> mempty
   CStaticInline -> "static inline "

prettyStmt :: CStmt -> Doc ann
prettyStmt = \case
   CBlock stmts -> vsep 
      [ "{"
      , indent 4 (vsep (map prettyStmt stmts))
      , "}"
      ]
   CReturn Nothing -> "return;"
   CReturn (Just expr) -> "return" <+> prettyExpr expr <> ";"
   CExprStmt expr -> prettyExpr expr <> ";"
   CDeclStmt declType name Nothing -> prettyType declType <+> pretty name <> ";"
   CDeclStmt declType name (Just expr) -> prettyType declType <+> pretty name <+> "=" <+> prettyExpr expr <> ";"
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
         prettyCase (label, stmts) = vsep
            [ "case" <+> prettyExpr label <> ": {"
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
   CVar name             -> pretty name
   CIntLit int           -> pretty int
   CFloatLit float       -> pretty float
   CInvoke caller args   -> prettyExpr caller <> encloseSep "(" ")" ", " (map prettyCArg args)
   CBinary op left right -> prettyOperand left <+> prettyBinOp op <+> prettyOperand right
   CUnary op operand     -> prettyUnOp op <> prettyOperand operand
   CAssign left right    -> prettyExpr left <+> "=" <+> prettyExpr right
   CMember object member -> prettyMemberObject object <> "." <> pretty member
   CCompound compoundType initializers  -> prettyCompound compoundType initializers
   CConditional condition whenTrue whenFalse -> prettyConditional condition whenTrue whenFalse

-- | A compound literal, e.g. @(Point){ ._0 = x, ._1 = y }@,
--   or without a type, e.g. @{ ._0 = x }@ inside another one.
prettyCompound :: Maybe CType -> [(Text, CExpr)] -> Doc ann
prettyCompound compoundType initializers =
   let typePrefix = maybe mempty prettyCast compoundType
       fields = initializers
         |> map prettyInitializer
         |> commaSeparated
   in typePrefix <> "{" <+> fields <+> "}"

-- | E.g. @(Point)@.
prettyCast :: CType -> Doc ann
prettyCast cType = parens (prettyType cType)

-- | E.g. @._0 = x@ or @.as.Some = { ._0 = x }@.
prettyInitializer :: (Text, CExpr) -> Doc ann
prettyInitializer (designator, value) =
   "." <> pretty designator <+> "=" <+> prettyExpr value

-- | E.g. @(a < b) ? x : y@.
prettyConditional :: CExpr -> CExpr -> CExpr -> Doc ann
prettyConditional condition whenTrue whenFalse =
   prettyOperand condition <+> "?" <+> prettyOperand whenTrue <+> ":" <+> prettyOperand whenFalse

-- | E.g. @a, b, c@.
commaSeparated :: [Doc ann] -> Doc ann
commaSeparated docs = docs
   |> punctuate ","
   |> hsep

-- | The object of a member access, in parentheses unless it is a name or
--   another member access: @(*_b0).x@.
prettyMemberObject :: CExpr -> Doc ann
prettyMemberObject object = case object of
   CVar _     -> prettyExpr object
   CMember {} -> prettyExpr object
   _          -> parens (prettyExpr object)

-- | A subexpression, parenthesized if it has a binary operator.
--   Unary operators bind tightest and need none.
prettyOperand :: CExpr -> Doc ann
prettyOperand expr
   | isCompound expr = enclose "(" ")" (prettyExpr expr)
   | otherwise       = prettyExpr expr

isCompound :: CExpr -> Bool
isCompound = \case
   CBinary {}      -> True
   CAssign {}      -> True
   CConditional {} -> True
   _               -> False

prettyCArg :: CArg -> Doc ann
prettyCArg = \case
   CExprArg expr -> prettyExpr expr
   CTypeArg argType -> prettyType argType

prettyBinOp :: CBinOp -> Doc ann
prettyBinOp = \case
   Add -> "+"
   Sub -> "-"
   Mul -> "*"
   Div -> "/"
   Mod -> "%"
   Eq -> "=="
   Ne -> "!="
   Lt -> "<"
   Le -> "<="
   Gt -> ">"
   Ge -> ">="
   And -> "&&"
   Or -> "||"
   BitAnd -> "&"
   BitOr -> "|"
   BitXor -> "^"
   Shl -> "<<"
   Shr -> ">>"

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
   CVoid            -> "void"
   CSize            -> "ptrdiff_t"
   CUSize           -> "size_t"
   CLong            -> "int64_t"
   CInt             -> "int32_t"
   CShort           -> "int16_t"
   CByte            -> "int8_t"
   CULong           -> "uint64_t"
   CUInt            -> "uint32_t"
   CUShort          -> "uint16_t"
   CUByte           -> "uint8_t"
   CFloat           -> "float"
   CDouble          -> "double"
   CPointer pointee -> prettyType pointee <> "*"
   CStruct name     -> pretty name