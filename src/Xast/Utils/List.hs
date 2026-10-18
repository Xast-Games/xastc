module Xast.Utils.List where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

forEach :: [a] -> (a -> b) -> [b]
forEach = flip map

forFlat :: Foldable t => t a -> (a -> [b]) -> [b]
forFlat = flip concatMap

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

updateKeys :: Ord k => [k] -> (v -> v) -> M.Map k v -> M.Map k v
updateKeys ks f m = foldr (M.adjust f) m ks

dedup :: Ord a => [a] -> [a]
dedup = S.toList . S.fromList

intoMap :: Ord k => [v] -> (v -> k) -> M.Map k v
intoMap xs f = 
   let tuples = map (\x -> (f x, x)) xs
   in M.fromList tuples

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs

allEqual :: Ord a => [a] -> Bool
allEqual xs =
   all
      (uncurry (==))
      (S.cartesianProduct
         (S.fromList xs)
         (S.fromList xs)
      )

anyEqual :: Ord a => [a] -> Bool
anyEqual xs =
   any
      (uncurry (==))
      (S.cartesianProduct
         (S.fromList xs)
         (S.fromList xs)
      )