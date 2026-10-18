{-# LANGUAGE LambdaCase #-}
module Xast.Lowerer.EraseZero where

import qualified Data.Set as S
import Xast.Lowerer.Types
import Xast.Utils.Generic ((|>), unreachableWith)
import qualified Xast.Utils.List as L
import Xast.AST
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Foldable (Foldable(foldr'))
import Data.List (partition)

zstSet :: [KirTypeDef] -> S.Set KirName
zstSet typeDefs =
   let typeDefMap = L.intoMap typeDefs (.name)
       candidates = typeDefs
         |> filter (\td ->
            let isSingleCtor = L.isSingleton td.ctors
                ctorsSuitable = td.ctors
                  |> filter (\ctor -> any (unhandyField typeDefMap) ctor.fields)
                  |> null
            in isSingleCtor && ctorsSuitable
         )

       waitingOn =
         L.forEach candidates $ \typeDef ->
            let count = (head typeDef.ctors).fields
                  |> filter notUnit
                  |> length
            in (typeDef.name, count)

       pairs =
         L.forFlat candidates $ \container ->
            L.forMaybe (head container.ctors).fields $ \field ->
               fmap (, container.name) (fieldTypeName field)

       dependents =
         M.fromListWith (++)
            [ (typeName, [containerName]) | (typeName, containerName) <- pairs ]

       queue = waitingOn
         |> filter (\(_, count) -> count == 0)
         |> map fst

   in goZst queue dependents (M.fromList waitingOn) S.empty

goZst
   :: [KirName]               -- ^ queue of processed ZST
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