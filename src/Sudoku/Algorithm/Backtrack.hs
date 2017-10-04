module Sudoku.Algorithm.Backtrack
  ( solve
  , solve'
  ) where

import Control.Monad (join)
import Sudoku.Model.Solution
import Data.Maybe (maybeToList, listToMaybe)

solve :: Solution -> Maybe Solution
solve s =
  if (complete s)
  then Just s
  else
    let
      steps = do
        p <- unknown s
        v <- canBe s p
        s' <- maybeToList $ put s p v
        maybeToList $ solve s'
    in listToMaybe steps
    
solve' :: Solution -> [Solution]
solve' s =
  if (complete s)
  then return s
  else
    let
      steps = do
        p <- unknown s
        v <- canBe s p
        s' <- maybeToList $ put s p v
        solve' s'
    in steps
    
