module Xast.Lowerer.DecisionTree where

import Xast.AST (LocalId, Type)
import Xast.Lowerer.Types (KirTag)
import Xast.Utils.Generic ((|>), unreachableWith)
import Data.Maybe (catMaybes, isJust, mapMaybe, fromJust)
import Data.List (findIndex, nub)

data Occ = Occ Int [Int]
   deriving (Eq, Ord, Show)

occField :: Occ -> Int -> Occ
occField (Occ root path) i = Occ root (path ++ [i])

type Binds = [(LocalId, Occ)]

data DecisionTree
   = DTLeaf Int Binds
   | DTFail
   | DTSwitch Occ [(Con, DecisionTree)] (Maybe DecisionTree)
   | DTSwap Int DecisionTree
   deriving Show

compilePatterns :: [Occ] -> PatMatrix -> DecisionTree
compilePatterns occs pm
   | null pm = DTFail
   | rowIsUndoubted (head pm) =
      let Row pats binds action = head pm
          binds' = binds ++ [(lid, occ) | (PVar lid, occ) <- zip pats occs]
      in DTLeaf action binds'

   | otherwise =
      let Row firstPats _ _ = head pm
          i = fromJust (findIndex isCon firstPats)
      in if i == 0 then
         let headCons = patMatrixHeadCons pm
             -- check pattern matching constructors set for
             -- completeness, so that default value doesn't
             -- fall into Nothing
             complete = length headCons == (head headCons).span
             def
               | complete = Nothing
               | otherwise = Just (compilePatterns (tail occs) (defaultD (head occs) pm))
             caseList = headCons
               |> map (\con ->
                     let pmSpecialized = specializeS (head occs) con pm
                         occs' = [occField (head occs) k | k <- [0 .. con.arity - 1]] ++ tail occs
                     in (con, compilePatterns occs' pmSpecialized)
                  )
         in DTSwitch (head occs) caseList def
      else
         let pmSwapped = patMatrixSwap i pm
             occsSwapped = swapElements 0 i occs
         in DTSwap i (compilePatterns occsSwapped pmSwapped)

data Con = Con
   { tag      :: KirTag
   , arity    :: Int
   , span     :: Int
   , fieldTys :: [Type]
   } deriving Show

instance Eq Con where
   (==) :: Con -> Con -> Bool
   a == b = a.tag == b.tag

data Pat
   = PWildCard
   | PVar LocalId
   | PCon Con [Pat]
   deriving (Eq, Show)

patCon :: Pat -> Maybe Con
patCon (PCon con _) = Just con
patCon _ = Nothing

isCon :: Pat -> Bool
isCon = isJust . patCon

isWild :: Pat -> Bool
isWild = not . isCon

data Row = Row [Pat] Binds Int

rowIsUndoubted :: Row -> Bool
rowIsUndoubted (Row pats _ _) = all isWild pats

rowHeadIs :: Con -> Row -> Bool
rowHeadIs conA (Row (PCon conB _ : _) _ _) = conA == conB
rowHeadIs _ _ = False

rowHeadIsWild :: Row -> Bool
rowHeadIsWild (Row (p : _) _ _) = isWild p
rowHeadIsWild _ = False

headBinds :: Occ -> Row -> Binds
headBinds occ (Row (PVar lid : _) binds _) = binds ++ [(lid, occ)]
headBinds _ (Row _ binds _) = binds

type PatMatrix = [Row]

patMatrixSwap :: Int -> PatMatrix -> PatMatrix
patMatrixSwap index = map swapRow
   where
      swapRow (Row pats binds n) = Row (swapElements 0 index pats) binds n

swapElements :: Int -> Int -> [a] -> [a]
swapElements i j xs
   | i == j = xs
   | i < 0 || j < 0 || i >= length xs || j >= length xs =
      unreachableWith "row swapped indices cannot be out of bounds"
   | otherwise =
      let a = xs !! i
          b = xs !! j
      in replace i b (replace j a xs)
   where
      replace :: Int -> a -> [a] -> [a]
      replace _ _ [] = []
      replace 0 x (_:ys) = x : ys
      replace k x (y:ys) = y : replace (k - 1) x ys

patMatrixHeadCons :: PatMatrix -> [Con]
patMatrixHeadCons pm = pm
   |> map (\(Row pats _ _) -> pats |> head |> patCon)
   |> catMaybes
   |> nub

specializeS :: Occ -> Con -> PatMatrix -> PatMatrix
specializeS occ con = mapMaybe (specializeRow occ con)

specializeRow :: Occ -> Con -> Row -> Maybe Row
specializeRow occ con row@(Row pats0 _ index)
  | rowHeadIs con row =
      let pats1 = case head pats0 of
            PCon _ args -> args
            _ -> unreachableWith "head must be a constructor"
      in Just (Row (pats1 ++ tail pats0) (headBinds occ row) index)

  | rowHeadIsWild row =
      let pats1 = replicate con.arity PWildCard
      in Just (Row (pats1 ++ tail pats0) (headBinds occ row) index)

  | otherwise = Nothing

defaultD :: Occ -> PatMatrix -> PatMatrix
defaultD occ = mapMaybe (defaultRow occ)

defaultRow :: Occ -> Row -> Maybe Row
defaultRow occ row@(Row pats0 _ index)
   | rowHeadIsWild row = Just (Row (tail pats0) (headBinds occ row) index)
   | otherwise = Nothing
