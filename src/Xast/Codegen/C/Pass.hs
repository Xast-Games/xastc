{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Codegen.C.Pass where

import Xast.Codegen.C.Types
import Xast.Lowerer.Types
import Xast.Codegen.C.Monad (CCodegen, runCodegen)
import Control.Monad (forM)
import Control.Monad.Identity (Identity(runIdentity))
import Xast.Utils.Generic (todo__)
import Xast.AST (moduleToPath, IntLiteral(..), Literal(..), Type(..), Ident(..), typename)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isJust)
import Control.Monad.Reader (MonadReader(ask))

codegen 
   :: [Kira] 
   -> [KirTypeDef]
   -> [CProgram]
codegen kira typeDefs = 
   let generated = do
         types    <- codegenTypeDefs
         programs <- forM kira codegenOne
         return (types : programs)
   in runIdentity (runCodegen generated typeDefs)

codegenTypeDefs :: CCodegen CProgram  
codegenTypeDefs = do
   types <- ask
   -- decls <- forM types $ \td ->
      -- if 

   undefined

codegenOne :: Kira -> CCodegen CProgram
codegenOne kira = do
   systemFns <- forM kira.systems codegenSystem
   pureFns <- forM kira.functions codegenFn

   let declarations = map CFunc (systemFns ++ pureFns)
   let path = moduleToPath kira.moduleName ".h"

   return CProgram {..}

codegenFn :: KirFunction -> CCodegen CFunction
codegenFn func = do
   let ty = typeToCType func.retTy
   let KirName name = func.name
   let args = [CFuncArg { ty = typeToCType param.ty, name = paramName } | param <- func.params, let KirName paramName = param.name]

   body <- codegenBlock func.body

   return CFunction {..}

codegenSystem :: KirSystem -> CCodegen CFunction
codegenSystem system = do
   let ty = CVoid
   let KirName name = system.name
   let args = [CFuncArg
         { ty = CPointer (CStruct "ecs_iter")
         , name = "it"
         }]

   let bindingDecls = zipWith declareBinding [0..] system.bindings
   blockStmts <- codegenBlock system.body
   let body = bindingDecls ++ blockStmts

   return CFunction {..}

declareBinding :: Int -> KirBinding -> CStmt
declareBinding index binding = 
   let bindingType = typeToCType binding.bindType
   in CDeclStmt 
      (CPointer bindingType) 
      (bindingName (KirBindingId index)) 
      (Just $
         CInvoke (CVar "ecs_field")
            [ CExprArg (CVar "it")
            , CTypeArg bindingType
            , CExprArg (CIntLit (toInteger index))
            ]
      )

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
   KirCall retType (KirName fnName) callArgs (KirName resultName) ->
      pure [CDeclStmt (typeToCType retType) resultName (Just (CInvoke (CVar fnName) (map (CExprArg . valueToExpr) callArgs)))]
   KirAssign bindingId value ->
      pure [CExprStmt (CAssign (CVar (bindingName bindingId)) (valueToExpr value))]
   KirStore storedType dest value -> pure $ case dest of
      DestLocal localId -> [CDeclStmt (typeToCType storedType) (localName localId) (Just (valueToExpr value))]
      DestBinding _ -> [assignToDest dest value]
   KirField {} ->
      todo__ "payload field access needs an ADT layout in the C backend"
   KirMatch { ty = matchType, scrutinee = scrutinee, arms = arms, fallback = fallback, dest = dest, exhaustive = exhaustive } ->
      codegenMatch matchType scrutinee arms fallback dest exhaustive

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
            pure (tagToExpr tag, caseStmts ++ [assignDest branch.value])
         defaultBranch <- case fallback of
            Just branch -> do
               defStmts <- codegenInstructs branch.instructs
               pure (CDefaultStmts (defStmts ++ [assignDest branch.value]))
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
               (armStmts ++ [assignDest branch.value])
               elseStmt
            ]

      codegenBranch branch = do
         defStmts <- codegenInstructs branch.instructs
         pure (defStmts ++ [assignDest branch.value])

      assignDest = assignToDest dest

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

tagToExpr :: KirTag -> CExpr
tagToExpr (TagLit lit) = literalToExpr lit
tagToExpr (TagCtor ctor) = CVar ctor.inner

literalToExpr :: Literal -> CExpr
literalToExpr (LitInt int)     = CIntLit int.value
literalToExpr (LitFloat float) = CFloatLit (realToFrac float)
literalToExpr lit              = todo__ ("no C representation for literal " ++ show lit)

typeToCType :: Type -> CType
typeToCType = \case
   TyCon (Ident "Int")   -> CInt
   TyCon (Ident "Float") -> CFloat
   TyCon (Ident "Long")  -> CLong
   TyCon name            -> CStruct name.inner
   other                 -> todo__ ("no C representation for type " ++ typename other)