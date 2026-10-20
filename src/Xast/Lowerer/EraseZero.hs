{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.EraseZero where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Xast.Utils.List as L
import Xast.Lowerer.Types
import Xast.Utils.Generic ((|>), unreachableWith)
import Xast.AST

eraseZeroSized :: [Kira] -> [KirTypeDef] -> ([Kira], [KirTypeDef])
eraseZeroSized kira typeDefs =
   let zstNames = zstSet typeDefs
       erasedTypeDefs =
         L.forEach typeDefs $ \typeDef ->
            let realCtors =
                  L.forEach typeDef.ctors $ \ctor ->
                     let realFields = ctor.fields
                           |> filter (\field -> field.ty
                              |> not . isZeroSized zstNames
                           )
                     in ctor { fields = realFields }
            in KirTypeDef
               { name = typeDef.name
               , ctors = realCtors
               }

   -- TODO: ZST-erase KIRA
   in (kira, erasedTypeDefs)

isZeroSized :: S.Set KirName -> Type -> Bool
isZeroSized zstNames = \case
   TyCon name -> KirName name.inner `S.member` zstNames
   TyTuple [] -> True
   TyFn _ _   -> False
   other      -> unreachableWith $ "invalid type in EraseZero phase: " <> show other

zstSet :: [KirTypeDef] -> S.Set KirName
zstSet typeDefs =
   let typeDefMap = L.intoMap typeDefs (.name)
       candidates = typeDefs
         |> filter (\td ->
            let isSingleCtor = L.isSingleton td.ctors
                ctorsSuitable = td.ctors
                  |> filter (\ctor -> any (unhandyField typeDefMap . (.ty)) ctor.fields)
                  |> null
            in isSingleCtor && ctorsSuitable
         )

       waitingOn =
         L.forEach candidates $ \typeDef ->
            let count = (head typeDef.ctors).fields
                  |> filter (notUnit . (.ty))
                  |> length
            in (typeDef.name, count)

       pairs =
         L.forFlat candidates $ \container ->
            L.forMaybe (head container.ctors).fields $ \field ->
               fmap (, container.name) (fieldTypeName field.ty)

       dependents =
         M.fromListWith (++)
            [ (typeName, [containerName]) | (typeName, containerName) <- pairs ]

       queue = waitingOn
         |> filter (\(_, count) -> count == 0)
         |> map fst

   in goZst queue dependents (M.fromList waitingOn) S.empty

goZst
   :: [KirName]               -- ^ queue of ZST being processed
   -> M.Map KirName [KirName] -- ^ map of types to dependent types, waiting on
   -> M.Map KirName Int       -- ^ map of types to dependent types count 
   -> S.Set KirName           -- ^ initial set of ZST
   -> S.Set KirName           -- ^ final set of ZST
goZst [] _ _ final = final
goZst (zst:queueRest) deps waitingOn initial =
   let newSet = S.insert zst initial
       dependentTypes = M.findWithDefault [] zst deps
       newWaitingOn = L.updateKeys dependentTypes (subtract 1) waitingOn
       dependentDeduped = L.dedup dependentTypes
       newZsts = flip filter dependentDeduped $ \name ->
         let count = newWaitingOn M.! name
         in count == 0
       newQueue = newZsts ++ queueRest
   in goZst newQueue deps newWaitingOn newSet

notUnit :: Type -> Bool
notUnit (TyTuple []) = False
notUnit _ = True

fieldTypeName :: Type -> Maybe KirName
fieldTypeName = \case
   TyCon name -> Just (KirName name.inner)
   TyFn _ _   -> Nothing
   TyTuple [] -> Nothing
   other      -> unreachableWith $ "invalid type in EraseZero phase: " <> show other

unhandyField :: M.Map KirName KirTypeDef -> Type -> Bool
unhandyField typeDefs ty = case fieldTypeName ty of
   Just name ->
      let foundType = M.lookup (KirName name.text) typeDefs
      in maybe True isMultiCtor foundType -- maybe True to support builtins

   Nothing -> False

isMultiCtor :: KirTypeDef -> Bool
isMultiCtor = not . L.isSingleton . (.ctors)