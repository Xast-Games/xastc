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
      go acc (TyApp f x) = go (x : acc) f
      go acc t           = (t, acc)

subst :: M.Map Ident Type -> Type -> Type
subst m = \case
   TyGnr i    -> M.findWithDefault (TyGnr i) i m
   TyApp a b  -> TyApp (subst m a) (subst m b)
   TyTuple ts -> TyTuple (map (subst m) ts)
   TyFn ps r  -> TyFn (map (subst m) ps) (subst m r)
   t          -> t

monoType :: Type -> Lowerer Type
monoType t = case splitApp t of
   (TyCon n, args) -> do
      isUser <- gets (M.member n . (.typeTmpls))
      if not isUser then 
         pure t
      else do
         mapM_ monoType args
         nm <- requestType (TypeInst n args)
         pure (TyCon (Ident nm.text))

   _ -> case t of
      TyTuple [single] -> monoType single
      TyTuple elementTypes@(_ : _) -> do
         mapM_ monoType elementTypes
         let tupleInst = TypeInst (tupleName (length elementTypes)) elementTypes
         tupleKirName <- requestType tupleInst
         return $ TyCon (Ident tupleKirName.text)

      TyFn ps r  -> TyFn <$> mapM monoType ps <*> monoType r
      _          -> pure t

requestType :: TypeInst -> Lowerer KirName
requestType inst = gets (M.lookup inst . (.typeInsts)) >>= \case
   Just nm -> pure nm
   Nothing -> do
      let nm = KirName (mangle inst)
      modify $ \s -> s
         { typeInsts = M.insert inst nm s.typeInsts
         , typeQueue = inst : s.typeQueue
         }
      pure nm

drainTypes :: Lowerer ()
drainTypes = gets (.typeQueue) >>= \case
   []            -> pure ()
   (inst : rest) -> do
      modify $ \s -> s { typeQueue = rest }
      def <- buildTypeDef inst
      tell [def]
      drainTypes

buildTypeDef :: TypeInst -> Lowerer KirTypeDef
buildTypeDef inst = case tupleArity inst.name of
   Just arity -> buildTupleDef arity inst
   Nothing    -> buildUserTypeDef inst

buildTupleDef :: Int -> TypeInst -> Lowerer KirTypeDef
buildTupleDef arity inst = do
   unless (length inst.args == arity) $
      error ("wrong number of elements for tuple type " ++ show inst.name)

   fieldTypes <- mapM monoType inst.args
   let fieldNames = map tupleFieldName [0 ..]
   let fields = zipWith (\name fieldType -> KirFieldDef { name = name, ty = fieldType }) fieldNames fieldTypes
   let mangledName = mangle inst
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
buildUserTypeDef inst = do
   tpl <- gets (M.lookup inst.name . (.typeTmpls)) >>= \case
      Just tpl -> pure tpl
      Nothing  -> error ("no template for type " ++ show inst.name)

   unless (length tpl.generics == length inst.args) $
      error ("wrong number of type arguments for type " ++ show inst.name)

   let sub = M.fromList (zip tpl.generics inst.args)
   ctors <- forM tpl.ctors $ \c -> do
      fields <- forM (payloadFields c.payload) $ \(fieldName, fieldType) -> do
         monoFieldType <- monoType (subst sub fieldType)
         pure KirFieldDef { name = fieldName, ty = monoFieldType }
      pure KirCtor
         { name   = mangleCtor (foldl TyApp (TyCon inst.name) inst.args) c.name
         , fields = fields
         }
   
   return KirTypeDef 
      { name = KirName (mangle inst)
      , ctors = ctors 
      }

mangleCtor :: Type -> Ident -> Ident
mangleCtor resTy c = Ident (mangle (TypeInst c (snd (splitApp resTy))))

ctorResultTy :: Type -> Type
ctorResultTy = \case
   TyFn _ r -> r
   t        -> t

mangle :: TypeInst -> Text
mangle inst = T.intercalate "_" (inst.name.inner : map mangleArg inst.args)

mangleArg :: Type -> Text
mangleArg t = case splitApp t of
   (TyCon i, [])   -> i.inner
   (TyCon i, args) -> wrap (mangle (TypeInst i args))
   _ -> case t of
      TyTuple ts -> wrap (mangle (TypeInst (tupleName (length ts)) ts))
      TyFn ps r  -> wrap (joinToken ("fn"  <> tshow (length ps)) (ps ++ [r]))
      _          -> error ("mangling: unexpected type " ++ show t)

wrap :: Text -> Text
wrap s = "_" <> s <> "_"

joinToken :: Text -> [Type] -> Text
joinToken m ts = T.intercalate "_" (m : map mangleArg ts)

tshow :: Int -> Text
tshow = T.pack . show
