{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.Definition where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, freshKirLocalId)
import Xast.Lowerer.Types
import Xast.Lowerer.Mono (monoType)
import Control.Monad (forM, when)
import Xast.Utils.Generic (todo__)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Xast.Lowerer.Match (lowerMatchTo, patTy)

lowerSystem :: Module -> SystemImpl Desugared -> Lowerer KirSystem
lowerSystem module' impl = do
   when (isJust impl.with) $
      todo__ "`with` entities are not supported yet"

   when (length impl.entities /= 1) $
      todo__ "0 or 2+ entities are not supported yet"

   let EntityPattern rawBindings = head impl.entities
   let mkBinding bid (EntPatBinding pat access) =
         (bid, KirBinding
            { bindType   = patTy pat
            , bindSrc    = SrcEntity
            , bindAccess = access
            })

   bs <- forM (zipWith mkBinding (map KirBindingId [0..]) rawBindings) $ \(bid, b) -> do
      ty <- monoType b.bindType
      pure (bid, b { bindType = ty })
   let env = M.fromList
         [ (lid, KirBindingRef bid)
         | (bid, (pat, _)) <- zip (map fst bs) (map (\(EntPatBinding p a) -> (p, a)) rawBindings)
         , Just (ResLocal lid) <- [(patAnnotation pat).res]
         ]

   (target, targetTy) <- case [(bid, b.bindType) | (bid, b) <- bs, b.bindAccess == AccessWrite] of
      [pair] -> pure pair
      _      -> todo__ "a system must write exactly one binding for now"

   bodyInstrs <- case impl.body of
      ExpMatch _ match -> lowerMatchTo env targetTy (DestBinding target) match
      _ -> todo__ "only a top-level `match` system body is supported yet"

   let name = KirName (namespacedName module' impl.name)
   let bindings = map snd bs
   let body = KirBlock { instructs = bodyInstrs, term = KirReturn Nothing }

   return KirSystem {..}

lowerFunction :: Module -> FuncImpl Desugared -> Lowerer KirFunction
lowerFunction module' impl = do
   match <- case impl.body of
      ExpMatch _ m -> pure m
      _ -> todo__ "only a top-level `match` function body is supported yet"

   paramExprs <- case match.baseExpr of
      ExpTuple _ es -> pure es
      _ -> todo__ "function bodies must match on their argument tuple"

   paramInfos <- forM paramExprs $ \case
      ExpVar info _ ident ->
         case info.res of
            Just (ResLocal l) -> pure (l, ident, info.ty)
            _ -> todo__ ("unresolved function parameter " ++ show ident)
      _ -> todo__ "function parameters must be plain variables"

   let env = M.fromList [(lid, KirVar (KirName ident.inner)) | (lid, ident, _) <- paramInfos]
   retTy <- monoType (exprAnnotation impl.body).ty

   resultId <- freshKirLocalId
   bodyInstrs <- lowerMatchTo env retTy (DestLocal resultId) match

   let name = KirName (namespacedName module' impl.name)
   params <- forM paramInfos $ \(_, ident, pty) -> do
      ty <- monoType pty
      pure KirParam { ty = ty, name = KirName ident.inner }
   let body = KirBlock { instructs = bodyInstrs, term = KirReturn (Just (KirLocalRef resultId)) }

   return KirFunction {..}
