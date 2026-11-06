{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Xast.Codegen.C.Pass where

import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad (forM)
import Data.Text (Text)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T

import Xast.AST
import Xast.Utils.Generic (todo__, unreachableWith, (|>))
import Xast.Lowerer.Types
import Xast.Codegen.C.Types
import Xast.Codegen.C.Monad (CCodegen, runCodegen, CCodegenEnv (..))
import qualified Xast.Utils.List as L
import qualified Data.Map as M
import Xast.Codegen.C.Intrinsics (IntrinsicDefinition (..), IntrinsicSite (..), intrinsicDefinitions)

codegen 
   :: [Kira] 
   -> [KirTypeDef]
   -> [EcsTag]
   -> [KirIntrinsicDef]
   -> [CProgram]
codegen kira typeDefs ecsTags intrinsicDefs = 
   let env = CCodegenEnv typeDefs ecsTags intrinsicDefs
       generated = do
         types    <- codegenTypeDefs
         programs <- forM kira codegenOne
         return (types : programs)
   in runIdentity (runCodegen generated env)

typeDefsFilename :: FilePath
typeDefsFilename = "xast_types.h"

codegenTypeDefs :: CCodegen CProgram  
codegenTypeDefs = do
   env <- ask

   let tagDeclarations = env.ecsTags |> map CTagDef
   let typeDeclarations = 
         L.forFlat env.types $ \td -> if
            | isPlainStruct td -> codegenStruct td
            | isEnum td        -> codegenEnum td
            | otherwise        -> codegenADT td

   -- After the types, since comparisons return Bool.
   -- static inline, as every module includes this header.
   let intrinsicFunctions = env.intrinsics |> map (CFunc . codegenIntrinsic)

   return CProgram 
      { path = typeDefsFilename
      , declarations = tagDeclarations ++ typeDeclarations ++ intrinsicFunctions
      } 

codegenStruct :: KirTypeDef -> [CDecl]
codegenStruct td =
   let name = td.name.text
       fields = fieldsToC (head td.ctors).fields

   in [CStructDef CStructDefinition {..}]

codegenEnum :: KirTypeDef -> [CDecl]
codegenEnum td =
   let name = td.name.text
       variants = td.ctors
         |> map (\ctor -> ctor.name.inner)

   in [CEnumDef CEnumDefinition {..}]

codegenADT :: KirTypeDef -> [CDecl]
codegenADT td =
   let tagName = tagEnumName td.name
       tagEnum = CEnumDef CEnumDefinition
         { name     = tagName
         , variants = td.ctors |> map (\ctor -> ctor.name.inner)
         }
       payloadCtors = td.ctors |> filter carriesData
       payloadStructs = payloadCtors |> map codegenPayload
       unionMembers = L.forEach payloadCtors $ \ctor ->
         (unionMemberName td.name ctor.name, CStruct (payloadStructName ctor.name))
       taggedUnion = CTaggedUnionDef CTaggedUnionDefinition
         { name       = td.name.text
         , tagType    = CStruct tagName
         , unionTypes = unionMembers
         }

   in tagEnum : payloadStructs ++ [taggedUnion]

codegenPayload :: KirCtor -> CDecl
codegenPayload ctor = CStructDef CStructDefinition
   { name   = payloadStructName ctor.name
   , fields = fieldsToC ctor.fields
   }

fieldsToC :: [KirFieldDef] -> [(Text, CType)]
fieldsToC fields = fields
   |> map (\field -> (field.name.inner, typeToCType field.ty))

-- | A constructor without fields is only a tag: it has no payload struct and no union member.
carriesData :: KirCtor -> Bool
carriesData ctor = not (null ctor.fields)

tagEnumName :: KirName -> Text
tagEnumName typeName = typeName.text <> "_Tag"

payloadStructName :: CtorName -> Text
payloadStructName ctorName = ctorName.inner <> "_Payload"

-- | The constructor's own name, without the type prefix 'mangleCtor' gave it:
--   the union is already scoped to the type, so @Main_Shape_Circle@ is just @Circle@.
unionMemberName
   :: KirName   -- ^ the tagged union's type, e.g. @Main_Shape@
   -> CtorName  -- ^ the mangled constructor, e.g. @Main_Shape_Circle@
   -> Text
unionMemberName typeName ctorName =
   let typePrefix = typeName.text <> "_"
   in fromMaybe ctorName.inner (T.stripPrefix typePrefix ctorName.inner)

codegenOne :: Kira -> CCodegen CProgram
codegenOne kira = do
   systemFns <- forM kira.systems codegenSystem
   pureFns <- forM kira.functions codegenFn

   let declarations = map CFunc (systemFns ++ pureFns)
   let path = moduleToPath kira.moduleName ".h"

   return CProgram {..}

codegenFn :: KirFunction -> CCodegen CFunction
codegenFn func = do
   let linkage = CExternal
   let ty = typeToCType func.retTy
   let KirName name = func.name
   let args = [CFuncArg { ty = typeToCType param.ty, name = paramName } | param <- func.params, let KirName paramName = param.name]

   body <- codegenBlock func.body

   return CFunction {..}

codegenSystem :: KirSystem -> CCodegen CFunction
codegenSystem system = do
   let linkage = CExternal
   let ty = CVoid
   let KirName name = system.name
   let args = [CFuncArg
         { ty = CPointer (CStruct "ecs_iter")
         , name = "it"
         }]

   let bindingDecls = concat (zipWith declareBinding [0..] system.bindings)
   blockStmts <- codegenBlock system.body
   let body = bindingDecls ++ blockStmts

   return CFunction {..}

-- | Declares the pointer to a binding's component data. A tag component has
--   no data (`ecs_field` would return NULL), so it declares nothing; the
--   index is the binding's own, so the other bindings keep theirs.
declareBinding :: Int -> KirBinding -> [CStmt]
declareBinding index binding
   | isUnitType binding.bindType = []
   | otherwise =
      let bindingType = typeToCType binding.bindType
      in [CDeclStmt 
         (CPointer bindingType) 
         (bindingName (KirBindingId index)) 
         (Just $
            CInvoke (CVar "ecs_field")
               [ CExprArg (CVar "it")
               , CTypeArg bindingType
               , CExprArg (CIntLit (toInteger index))
               ]
         )]

-- | The type of something erased: it has no C representation to declare or assign.
isUnitType :: Type -> Bool
isUnitType (TyTuple []) = True
isUnitType _            = False

bindingName :: KirBindingId -> Text
bindingName (KirBindingId index) = "_b" <> T.pack (show index)

codegenBlock :: KirBlock -> CCodegen [CStmt]
codegenBlock block = do
   stmts <- codegenInstructs block.instructs
   pure (stmts ++ codegenTerm block.term)

codegenTerm :: KirTerm -> [CStmt]
codegenTerm (KirReturn maybeValue) = [CReturn (valueToExpr <$> maybeValue)]

codegenInstructs :: [KirInstruct] -> CCodegen [CStmt]
codegenInstructs = fmap concat . mapM codegenInstruct

codegenInstruct :: KirInstruct -> CCodegen [CStmt]
codegenInstruct = \case
   KirCall 
      { ty = retType
      , callee = KirName fnName
      , args = callArgs
      , out = out 
      } -> do
         let callArguments = callArgs |> map (CExprArg . valueToExpr)
         let invocation = CInvoke (CVar fnName) callArguments
         case out of
            Just (KirName resultName) ->
               pure [CDeclStmt (typeToCType retType) resultName (Just invocation)]
            Nothing ->
               pure [CExprStmt invocation]
   KirAssign bindingId value ->
      pure [CExprStmt (CAssign (CVar (bindingName bindingId)) (valueToExpr value))]
   KirStore storedType dest value -> pure $ case dest of
      DestLocal localId -> [CDeclStmt (typeToCType storedType) (localName localId) (Just (valueToExpr value))]
      DestBinding _ -> [assignToDest dest value]
   KirField {} ->
      todo__ "payload field access needs an ADT layout in the C backend"
   KirMatch { ty = matchType, scrutinee = scrutinee, arms = arms, fallback = fallback, dest = dest, exhaustive = exhaustive } ->
      codegenMatch matchType scrutinee arms fallback dest exhaustive

-- | A static inline C function for one intrinsic instance.
--   The body comes from 'intrinsicDefinitions'.
codegenIntrinsic :: KirIntrinsicDef -> CFunction
codegenIntrinsic instance' =
   let paramNames = zipWith (\index _ -> intrinsicParamName index) [0 ..] instance'.params
       args = L.forEach (zip instance'.params paramNames) $ \(paramType, paramName) ->
         CFuncArg { ty = typeToCType paramType, name = paramName }
       operandType = case instance'.params of
         first : _ -> first
         []        -> instance'.retTy
       site = IntrinsicSite
         { params      = map CVar paramNames
         , operandType = operandType
         , resultType  = instance'.retTy
         }
       definition = case M.lookup instance'.intrinsic intrinsicDefinitions of
         Just known -> known
         Nothing    -> unreachableWith ("the semantic analyzer let through unknown intrinsic " ++ show instance'.intrinsic)
   in CFunction
      { linkage = CStaticInline
      , ty      = typeToCType instance'.retTy
      , name    = instance'.name.text
      , args    = args
      , body    = definition.body site
      }

intrinsicParamName :: Int -> Text
intrinsicParamName index = "_a" <> T.pack (show index)

codegenMatch
   :: Type
   -> KirValue
   -> [KirMatchArm]
   -> Maybe KirBranch
   -> KirDest
   -> Bool
   -> CCodegen [CStmt]
codegenMatch matchType scrutinee arms fallback dest exhaustive = do
   armStmts <- if canSwitch then codegenSwitch else fromMaybe [] <$> goIf arms
   pure (declStmt ++ armStmts)
   where
      scrutExpr = valueToExpr scrutinee

      declStmt = case dest of
         _ | isUnitType matchType -> []
         DestBinding _     -> []
         DestLocal localId -> [CDeclStmt (typeToCType matchType) (localName localId) Nothing]

      switchableTag (TagLit (LitInt _))  = True
      switchableTag (TagLit (LitChar _)) = True
      switchableTag (TagCtor _)          = True
      switchableTag _                    = False

      canSwitch =
         not (null arms)
         && all (switchableTag . (.tag)) arms
         && (isJust fallback || exhaustive)

      codegenSwitch = do
         cases <- forM arms $ \(KirMatchArm { tag = tag, branch = branch }) -> do
            caseStmts <- codegenInstructs branch.instructs
            pure (tagToExpr tag, caseStmts ++ assignDest branch.value)
         defaultBranch <- case fallback of
            Just branch -> do
               defStmts <- codegenInstructs branch.instructs
               pure (CDefaultStmts (defStmts ++ assignDest branch.value))
            Nothing -> pure CDefaultUnreachable
         pure [CSwitch scrutExpr cases defaultBranch]

      goIf :: [KirMatchArm] -> CCodegen (Maybe [CStmt])
      goIf [] = traverse codegenBranch fallback
      goIf (KirMatchArm { tag = tag, branch = branch } : rest) = do
         armStmts <- codegenInstructs branch.instructs
         elseStmt <- goIf rest
         pure $ Just
            [ CIf
               (CBinary Eq scrutExpr (tagToExpr tag))
               (armStmts ++ assignDest branch.value)
               elseStmt
            ]

      codegenBranch branch = do
         defStmts <- codegenInstructs branch.instructs
         pure (defStmts ++ assignDest branch.value)

      -- a match with an erased result has nothing to assign
      assignDest branchValue
         | isUnitType matchType = []
         | otherwise            = [assignToDest dest branchValue]

assignToDest :: KirDest -> KirValue -> CStmt
assignToDest dest value = case dest of
   DestBinding bindingId -> CExprStmt (CAssign (CUnary Deref (CVar (bindingName bindingId))) (valueToExpr value))
   DestLocal localId     -> CExprStmt (CAssign (CVar (localName localId)) (valueToExpr value))

localName :: KirLocalId -> Text
localName (KirLocalId index) = "_l" <> T.pack (show index)

valueToExpr :: KirValue -> CExpr
valueToExpr (KirConst constTag)       = tagToExpr constTag
valueToExpr (KirVar (KirName name))   = CVar name
valueToExpr (KirBindingRef bindingId) = CUnary Deref (CVar (bindingName bindingId))
valueToExpr (KirLocalRef localId)     = CVar (localName localId)
valueToExpr KirUnit                   = unreachableWith "KirUnit reached codegen, erasure should have removed it"

tagToExpr :: KirTag -> CExpr
tagToExpr (TagLit lit) = literalToExpr lit
tagToExpr (TagCtor ctor) = CVar ctor.inner

literalToExpr :: Literal -> CExpr
literalToExpr (LitInt int)     = CIntLit int.value
literalToExpr (LitFloat float) = CFloatLit (realToFrac float)
literalToExpr (LitDouble double) = CFloatLit double
literalToExpr lit              = todo__ ("no C representation for literal " ++ show lit)

typeToCType :: Type -> CType
typeToCType = \case
   TyTuple []            -> CVoid
   TyCon qualified       -> namedCType qualified.name.inner
   other                 -> todo__ ("no C representation for type " ++ typename other)

-- | C type of a named type. Extern types keep their bare name, while user types
--   arrive monomorphized, already named with their module prefix.
namedCType :: Text -> CType
namedCType = \case
   "Size"   -> CSize
   "USize"  -> CUSize
   "Long"   -> CLong
   "ULong"  -> CULong
   "Int"    -> CInt
   "UInt"   -> CUInt
   "Short"  -> CShort
   "UShort" -> CUShort
   "Byte"   -> CByte
   "UByte"  -> CUByte
   "Float"  -> CFloat
   "Double" -> CDouble
   name     -> CStruct name
