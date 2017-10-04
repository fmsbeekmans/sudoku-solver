module Sudoku.Model.Value
    ( Value(..)
    , allValues
    ) where

data Value = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving ( Eq, Ord, Enum, Bounded )

allValues :: [Value]
allValues = [ V1, V2, V3, V4, V5, V6, V7, V8, V9 ]

instance Show Value where
  show V1 = "1"
  show V2 = "2"
  show V3 = "3"
  show V4 = "4"
  show V5 = "5"
  show V6 = "6"
  show V7 = "7"
  show V8 = "8"
  show V9 = "9"
