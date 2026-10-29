module Xast.Lowerer.DecisionTree where

import Xast.AST (LocalId, Type, FieldName)
import Xast.Lowerer.Types (KirTag)
import Xast.Utils.Generic ((|>), unreachableWith)
import Data.Maybe (catMaybes, isJust, mapMaybe, fromJust)
import Data.List (findIndex, nub)
import qualified Xast.Utils.List as L

data Occ = Occ
   { root :: Int
   , path :: [Int]
   }
   deriving (Eq, Ord, Show)

occField :: Occ -> Int -> Occ
occField (Occ root path) i = Occ root (path ++ [i])

type Binds = [(LocalId, Occ)]

data DecisionTree
   = DTLeaf
      { action :: Int
      , binds  :: Binds
      }
   | DTFail
   | DTSwitch
      { occurrence :: Occ
      , cases      :: [(Con, DecisionTree)]
      , fallback   :: Maybe DecisionTree
      }
   | DTSwap
      { column :: Int
      , tree   :: DecisionTree
      }
   deriving Show

compilePatterns :: [Occ] -> PatMatrix -> DecisionTree
compilePatterns occurrences patternMatrix
   | null patternMatrix = DTFail
   | rowIsUndoubted (head patternMatrix) =
      let Row patterns binds action = head patternMatrix
          binds' = binds ++ [(localId, occ) | (PVar localId, occ) <- zip patterns occurrences]
      in DTLeaf action binds'

   | otherwise =
      let Row firstPats _ _ = head patternMatrix
          columnIndex = fromJust (findIndex isCon firstPats)
      in if columnIndex == 0 then
         let headCons = patMatrixHeadCons patternMatrix
             -- check pattern matching constructors set for
             -- completeness, so that default value doesn't
             -- fall into Nothing
             complete = length headCons == (head headCons).span
             defaultTree
               | complete = Nothing
               | otherwise = Just (compilePatterns (tail occurrences) (defaultD (head occurrences) patternMatrix))
             caseList =
               L.forEach headCons $ \constructor ->
                  let specialized = specializeS (head occurrences) constructor patternMatrix
                      childOccurrences = [occField (head occurrences) fieldIndex | fieldIndex <- [0 .. constructor.arity - 1]] ++ tail occurrences
                  in (constructor, compilePatterns childOccurrences specialized)
         in DTSwitch (head occurrences) caseList defaultTree
      else
         let swappedMatrix = patMatrixSwap columnIndex patternMatrix
             swappedOccurrences = swapElements 0 columnIndex occurrences
         in DTSwap columnIndex (compilePatterns swappedOccurrences swappedMatrix)

data Con = Con
   { tag        :: KirTag
   , arity      :: Int
   , span       :: Int
   , fieldTys   :: [Type]
   , fieldNames :: [FieldName]
   } deriving Show

instance Eq Con where
   (==) :: Con -> Con -> Bool
   a == b = a.tag == b.tag

data Pat
   = PWildCard
   | PVar LocalId
   | PCon 
      { con :: Con
      , subpatterns :: [Pat] 
      }
   deriving (Eq, Show)

patCon :: Pat -> Maybe Con
patCon (PCon constructor _) = Just constructor
patCon _ = Nothing

isCon :: Pat -> Bool
isCon = isJust . patCon

isWild :: Pat -> Bool
isWild = not . isCon

data Row = Row
   { pats   :: [Pat]
   , binds  :: Binds
   , action :: Int
   }

rowIsUndoubted :: Row -> Bool
rowIsUndoubted (Row patterns _ _) = all isWild patterns

rowHeadIs :: Con -> Row -> Bool
rowHeadIs expected (Row (PCon actual _ : _) _ _) = expected == actual
rowHeadIs _ _ = False

rowHeadIsWild :: Row -> Bool
rowHeadIsWild (Row (headPattern : _) _ _) = isWild headPattern
rowHeadIsWild _ = False

headBinds :: Occ -> Row -> Binds
headBinds occ (Row (PVar localId : _) binds _) = binds ++ [(localId, occ)]
headBinds _ (Row _ binds _) = binds

type PatMatrix = [Row]

patMatrixSwap :: Int -> PatMatrix -> PatMatrix
patMatrixSwap index = map swapRow
   where
      swapRow (Row patterns binds action) = Row (swapElements 0 index patterns) binds action

swapElements :: Int -> Int -> [a] -> [a]
swapElements i j elements
   | i == j = elements
   | i < 0 || j < 0 || i >= length elements || j >= length elements =
      unreachableWith "row swapped indices cannot be out of bounds"
   | otherwise =
      let atI = elements !! i
          atJ = elements !! j
      in replace i atJ (replace j atI elements)
   where
      replace :: Int -> a -> [a] -> [a]
      replace _ _ [] = []
      replace 0 replacement (_:rest) = replacement : rest
      replace k replacement (current:rest) = current : replace (k - 1) replacement rest

patMatrixHeadCons :: PatMatrix -> [Con]
patMatrixHeadCons patternMatrix = patternMatrix
   |> map (\(Row patterns _ _) -> patterns |> head |> patCon)
   |> catMaybes
   |> nub

specializeS :: Occ -> Con -> PatMatrix -> PatMatrix
specializeS occ constructor = mapMaybe (specializeRow occ constructor)

specializeRow :: Occ -> Con -> Row -> Maybe Row
specializeRow occ constructor row@(Row originalPatterns _ action)
  | rowHeadIs constructor row =
      let headPatterns = case head originalPatterns of
            PCon _ args -> args
            _ -> unreachableWith "head must be a constructor"
      in Just (Row (headPatterns ++ tail originalPatterns) (headBinds occ row) action)

  | rowHeadIsWild row =
      let headPatterns = replicate constructor.arity PWildCard
      in Just (Row (headPatterns ++ tail originalPatterns) (headBinds occ row) action)

  | otherwise = Nothing

defaultD :: Occ -> PatMatrix -> PatMatrix
defaultD occ = mapMaybe (defaultRow occ)

defaultRow :: Occ -> Row -> Maybe Row
defaultRow occ row@(Row originalPatterns _ action)
   | rowHeadIsWild row = Just (Row (tail originalPatterns) (headBinds occ row) action)
   | otherwise = Nothing
