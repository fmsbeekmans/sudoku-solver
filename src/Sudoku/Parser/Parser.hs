module Sudoku.Parser.Parser
  ( parse
  ) where


import Data.Map.Strict ( fromList )

import Sudoku.Model.Solution
import Sudoku.Model.Value
import Sudoku.Model.MutexGroup


parse :: String -> Solution
parse s =
  let
    ps   = concat $ fmap points (allGroups :: [Row])
    vs   = concat $ fmap toValues (lines s)
    pmvs = zip ps vs
    pvs  = pmvs >>= (\x ->
                   case x of
                     (p, Just v) -> [(p, v)]
                     _ -> []
                  )
  in Solution . fromList $ pvs

toValues :: String -> [Maybe Value]
toValues = fmap toValue
  
toValue :: Char -> (Maybe Value)
toValue '1' = Just V1
toValue '2' = Just V2
toValue '3' = Just V3
toValue '4' = Just V4
toValue '5' = Just V5
toValue '6' = Just V6
toValue '7' = Just V7
toValue '8' = Just V8
toValue '9' = Just V9
toValue _ = Nothing
