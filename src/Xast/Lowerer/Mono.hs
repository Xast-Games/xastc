{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Lowerer.Mono where

import Xast.AST
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readMaybe)
import Control.Monad (forM, unless)
import Control.Monad.State (gets, modify)
import Control.Monad.Writer (tell)
import Xast.Lowerer.Monad (Lowerer)
import Xast.Lowerer.Types

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
   (TyCon name, args) -> do
      isUser <- gets (M.member name . (.typeTmpls))
      if not isUser then 
         pure type'
      else do
         mapM_ monoType args
         kirName <- requestType (TypeInst name args)
         pure (TyCon (Ident kirName.text))

   _ -> case type' of
      TyTuple [single] -> monoType single
      TyTuple elementTypes@(_ : _) -> do
         mapM_ monoType elementTypes
         let tupleInst = TypeInst (tupleName (length elementTypes)) elementTypes
         tupleKirName <- requestType tupleInst
         return $ TyCon (Ident tupleKirName.text)

      TyFn params result -> TyFn <$> mapM monoType params <*> monoType result
      _                  -> pure type'

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
      }

tupleName :: Int -> Ident
tupleName arity = Ident ("tup" <> tshow arity)

tupleArity :: Ident -> Maybe Int
tupleArity name = do
   digits <- T.stripPrefix "tup" name.inner
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
      }

mangleCtor :: Type -> Ident -> Ident
mangleCtor resultType ctorName = Ident (mangle (TypeInst ctorName (snd (splitApp resultType))))

ctorResultTy :: Type -> Type
ctorResultTy = \case
   TyFn _ result -> result
   other         -> other

mangle :: TypeInst -> Text
mangle instance' = T.intercalate "_" (instance'.name.inner : map mangleArg instance'.args)

mangleArg :: Type -> Text
mangleArg type' = case splitApp type' of
   (TyCon name, [])   -> name.inner
   (TyCon name, args) -> wrap (mangle (TypeInst name args))
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
