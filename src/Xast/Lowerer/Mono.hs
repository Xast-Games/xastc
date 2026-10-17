{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Lowerer.Mono where

import Xast.AST
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
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

payloadTypes :: Payload -> [Type]
payloadTypes = \case
   PUnit      -> []
   PTuple ts  -> map (.node) ts
   PRecord fs -> map (\f -> f.ty.node) fs

monoType :: Type -> Lowerer Type
monoType t = case splitApp t of
   (TyCon n, args) -> do
      isUser <- gets (M.member n . (.typeTmpls))
      if not isUser then 
         pure t
      else do
         mapM_ monoType args
         nm <- requestType (TypeInst n args)
         pure (TyCon (identFromKirName nm))

   _ -> case t of
      TyTuple ts -> TyTuple <$> mapM monoType ts
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
buildTypeDef inst = do
   tpl <- gets (M.lookup inst.name . (.typeTmpls)) >>= \case
      Just tpl -> pure tpl
      Nothing  -> error ("no template for type " ++ show inst.name)

   unless (length tpl.generics == length inst.args) $
      error ("wrong number of type arguments for type " ++ show inst.name)

   let sub = M.fromList (zip tpl.generics inst.args)
   ctors <- forM tpl.ctors $ \c -> do
      tys <- mapM (monoType . subst sub) (payloadTypes c.payload)
      pure KirCtor
         { name   = Ident (mangle (TypeInst c.name inst.args))
         , fields = tys
         }
   pure KirTypeDef { name = KirName (mangle inst), ctors = ctors }

identFromKirName :: KirName -> Ident
identFromKirName (KirName t) = Ident t

mangle :: TypeInst -> Text
mangle inst = T.intercalate "_" (inst.name.inner : map mangleArg inst.args)

mangleArg :: Type -> Text
mangleArg t = case splitApp t of
   (TyCon i, [])   -> i.inner
   (TyCon i, args) -> wrap (mangle (TypeInst i args))
   _ -> case t of
      TyTuple ts -> wrap (joinToken ("tup" <> tshow (length ts)) ts)
      TyFn ps r  -> wrap (joinToken ("fn"  <> tshow (length ps)) (ps ++ [r]))
      _          -> error ("mangling: unexpected type " ++ show t)

wrap :: Text -> Text
wrap s = "_" <> s <> "_"

joinToken :: Text -> [Type] -> Text
joinToken m ts = T.intercalate "_" (m : map mangleArg ts)

tshow :: Int -> Text
tshow = T.pack . show
