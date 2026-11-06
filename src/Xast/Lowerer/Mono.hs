{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Lowerer.Mono where

import Xast.AST
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readMaybe)
import Control.Monad (forM, forM_, unless)
import Control.Monad.State (gets, modify)
import Control.Monad.Writer (tell)
import Xast.Lowerer.Monad (Lowerer)
import Xast.Lowerer.Types
import Xast.Utils.Generic ((|>))

splitApp :: Type -> (Type, [Type])
splitApp = go []
   where
      go accumulator (TyApp func arg) = go (arg : accumulator) func
      go accumulator other            = (other, accumulator)

subst :: M.Map Ident Type -> Type -> Type
subst substitution = \case
   TyGnr name         -> M.findWithDefault (TyGnr name) name substitution
   TyApp func arg     -> TyApp (subst substitution func) (subst substitution arg)
   TyTuple elements   -> TyTuple (map (subst substitution) elements)
   TyFn params result -> TyFn (map (subst substitution) params) (subst substitution result)
   other              -> other

monoType :: Type -> Lowerer Type
monoType type' = case splitApp type' of
   (TyCon qualified, args) -> do
      isUser <- gets (M.member qualified . (.typeTmpls))
      if not isUser then 
         pure type'
      else do
         mapM_ monoType args
         kirName <- requestType (TypeInst qualified args)
         pure (monomorphicType kirName)

   _ -> case type' of
      TyTuple [single] -> monoType single
      TyTuple elementTypes@(_ : _) -> do
         mapM_ monoType elementTypes
         let tupleInst = TypeInst (tupleName (length elementTypes)) elementTypes
         tupleKirName <- requestType tupleInst
         return (monomorphicType tupleKirName)

      TyFn params result -> TyFn <$> mapM monoType params <*> monoType result
      _                  -> pure type'

-- | The type of a monomorphized instance. Its name is already the final flat name,
--   module prefix included, so it is not qualified again.
monomorphicType :: KirName -> Type
monomorphicType kirName = TyCon QualifiedIdent { moduleName = Nothing, name = Ident kirName.text }

requestType :: TypeInst -> Lowerer KirName
requestType instance' = gets (M.lookup instance' . (.typeInsts)) >>= \case
   Just existingName -> pure existingName
   Nothing -> do
      let kirName = KirName (mangle instance')
      modify $ \state -> state
         { typeInsts = M.insert instance' kirName state.typeInsts
         , typeQueue = instance' : state.typeQueue
         }
      pure kirName

-- | Requests every @Tag type up front. Other types are instantiated only when
--   a function or system uses them, but a tag is an ECS component that must
--   exist even if no code mentions it.
requestTagTypes :: Lowerer ()
requestTagTypes = do
   templates <- gets (M.toList . (.typeTmpls))
   let tagTypeNames = templates
         |> filter (\(_, template) -> isTagTemplate template)
         |> map fst
   forM_ tagTypeNames $ \typeName ->
      requestType TypeInst { name = typeName, args = [] }

-- | A non-generic type marked @Tag (generic tags are rejected by the semantic analyzer).
isTagTemplate :: TypeDef -> Bool
isTagTemplate template =
   let tag = TypeMod ModTag `elem` template.modifiers
   in tag && null template.generics

drainTypes :: Lowerer ()
drainTypes = gets (.typeQueue) >>= \case
   []                 -> pure ()
   (instance' : rest) -> do
      modify $ \state -> state { typeQueue = rest }
      typeDef <- buildTypeDef instance'
      tell [typeDef]
      drainTypes

buildTypeDef :: TypeInst -> Lowerer KirTypeDef
buildTypeDef instance' = case tupleArity instance'.name of
   Just arity -> buildTupleDef arity instance'
   Nothing    -> buildUserTypeDef instance'

buildTupleDef :: Int -> TypeInst -> Lowerer KirTypeDef
buildTupleDef arity instance' = do
   unless (length instance'.args == arity) $
      error ("wrong number of elements for tuple type " ++ show instance'.name)

   fieldTypes <- mapM monoType instance'.args
   let fieldNames = map tupleFieldName [0 ..]
   let fields = zipWith (\name fieldType -> KirFieldDef { name = name, ty = fieldType }) fieldNames fieldTypes
   let mangledName = mangle instance'
   let ctor = KirCtor { name = Ident mangledName, fields = fields }

   return KirTypeDef
      { name  = KirName mangledName
      , ctors = [ctor]
      , mods = []
      }

-- | Tuples are not declared in any module, so their name has none.
tupleName :: Int -> QualifiedIdent
tupleName arity = QualifiedIdent { moduleName = Nothing, name = Ident ("tup" <> tshow arity) }

tupleArity :: QualifiedIdent -> Maybe Int
tupleArity qualified = case qualified.moduleName of
   Just _  -> Nothing
   Nothing -> do
      digits <- T.stripPrefix "tup" qualified.name.inner
      readMaybe (T.unpack digits)

buildUserTypeDef :: TypeInst -> Lowerer KirTypeDef
buildUserTypeDef instance' = do
   template <- gets (M.lookup instance'.name . (.typeTmpls)) >>= \case
      Just found -> pure found
      Nothing    -> error ("no template for type " ++ show instance'.name)

   unless (length template.generics == length instance'.args) $
      error ("wrong number of type arguments for type " ++ show instance'.name)

   let substitution = M.fromList (zip template.generics instance'.args)
   ctors <- forM template.ctors $ \ctor -> do
      fields <- forM (payloadFields ctor.payload) $ \(fieldName, fieldType) -> do
         monoFieldType <- monoType (subst substitution fieldType)
         pure KirFieldDef { name = fieldName, ty = monoFieldType }
      pure KirCtor
         { name   = mangleCtor (foldl TyApp (TyCon instance'.name) instance'.args) ctor.name
         , fields = fields
         }
   
   return KirTypeDef 
      { name = KirName (mangle instance')
      , ctors = ctors 
      , mods = template.modifiers |> map asTypeModifier
      }

asTypeModifier :: Modifier -> TypeModifier
asTypeModifier = \case
   TypeMod m -> m
   _ -> error "Cannot extract non-type modifier as type"

-- | Name of a constructor once its type has been instantiated: the name of that
--   type instance followed by the constructor's own name (@A Int@, @B@ -> @Main_A_Int_B@),
--   so it never clashes with its type or with constructors of other instances.
mangleCtor
   :: Type      -- ^ the constructor's result type, e.g. @A Int@
   -> CtorName  -- ^ the constructor's own name, e.g. @B@
   -> CtorName
mangleCtor resultType ctorName = case splitApp resultType of
   (TyCon typeName, typeArgs) ->
      let typeInstance = TypeInst { name = typeName, args = typeArgs }
          mangledText = mangle typeInstance <> "_" <> ctorName.inner
      in Ident mangledText
   _ -> ctorName

-- | A constructor qualified with the module of its type, which also declares it.
qualifiedCtor
   :: Type      -- ^ the constructor's result type, e.g. @A Int@
   -> CtorName
   -> QualifiedIdent
qualifiedCtor resultType ctorName = case splitApp resultType of
   (TyCon typeName, _) -> QualifiedIdent { moduleName = typeName.moduleName, name = ctorName }
   _                   -> QualifiedIdent { moduleName = Nothing, name = ctorName }

ctorResultTy :: Type -> Type
ctorResultTy = \case
   TyFn _ result -> result
   other         -> other

mangle :: TypeInst -> Text
mangle instance' = T.intercalate "_" (flatName instance'.name : map mangleArg instance'.args)

-- | A declared name flattened into one identifier: 
--   prefixed with its module, if it has one.
flatName :: QualifiedIdent -> Text
flatName qualified = case qualified.moduleName of
   Just module' -> namespacedName module' qualified.name
   Nothing      -> qualified.name.inner

mangleArg :: Type -> Text
mangleArg type' = case splitApp type' of
   (TyCon qualified, [])   -> flatName qualified
   (TyCon qualified, args) -> wrap (mangle (TypeInst qualified args))
   _                  -> case type' of
      TyTuple elements   -> wrap (mangle (TypeInst (tupleName (length elements)) elements))
      TyFn params result -> wrap (joinToken ("fn"  <> tshow (length params)) (params ++ [result]))
      _                  -> error ("mangling: unexpected type " ++ show type')

wrap :: Text -> Text
wrap text = "_" <> text <> "_"

joinToken :: Text -> [Type] -> Text
joinToken prefix types = T.intercalate "_" (prefix : map mangleArg types)

tshow :: Int -> Text
tshow = T.pack . show
