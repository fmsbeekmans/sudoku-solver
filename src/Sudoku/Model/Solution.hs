module Sudoku.Model.Solution
  ( Solution (..)
  , complete
  , conflict
  , canBe
  , get
  , put
  , unknown
  ) where

import Data.List ( (\\), nub, find )
import Data.List.Split ( chunksOf )
import Data.Map.Strict ( (!?), toList, insert )
import Data.Maybe ( isNothing, fromMaybe, maybeToList, catMaybes ) 
import qualified Data.Map.Strict as Map

import qualified Sudoku.Model.Point as P
import qualified Sudoku.Model.Value as V
import Sudoku.Model.MutexGroup ( mutexGroups, points, allGroups, mutex, Row )

data Solution = Solution (Map.Map P.Point V.Value)

get :: Solution -> P.Point -> Maybe V.Value
get (Solution m) p = m !? p

put :: Solution -> P.Point -> V.Value -> Maybe Solution
put s@(Solution m) p v =
  if (conflict s p v)
  then Nothing
  else Just . Solution $ insert p v m

conflict :: Solution -> P.Point -> V.Value -> Bool
conflict s p v =
  let ps = mutex p
      vs = nub $ fmap (get s) ps
  in  elem (Just v) vs

complete :: Solution -> Bool
complete s = all (9 ==) $ fmap (length . nub . (getValues s)) mutexGroups

getValues :: Solution -> [P.Point] -> [V.Value]
getValues s ps = foldl (\acc p ->
                          case get s p of
                            Just v -> v:acc
                            Nothing -> acc
                            ) [] ps

unknown :: Solution -> [P.Point]
unknown s =
  let
    pvs = fmap (\p -> (p, get s p)) P.allPoints
    empty = filter (isNothing . snd) pvs
  in fmap fst empty

canBe :: Solution -> P.Point -> [V.Value]
canBe s p =
  let
    related = mutex p
    cantBe = nub $ related >>= (maybeToList . get s)
  in V.allValues \\ cantBe

instance Show Solution where
  show s = unlines $ do
    row <- fmap points (allGroups :: [Row])
    return $ foldl (\acc p ->
                      acc ++ case get s p of
                               Just v -> show v
                               Nothing -> " "
                   ) "" row
  
