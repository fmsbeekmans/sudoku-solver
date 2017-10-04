module Sudoku.Model.Point
  ( X(..)
  , Y(..)
  , xs
  , ys
  , Point(..)
  , allPoints
  ) where


import Data.Semigroup ( (<>) )


data X = X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 deriving ( Eq, Ord, Enum, Bounded )

instance Show X where
  show X1 = "1"
  show X2 = "2"
  show X3 = "3"
  show X4 = "4"
  show X5 = "5"
  show X6 = "6"
  show X7 = "7"
  show X8 = "8"
  show X9 = "9"

xs :: [X]
xs = [minBound..maxBound]

data Y = Y1 | Y2 | Y3 | Y4 | Y5 | Y6 | Y7 | Y8 | Y9 deriving ( Eq, Ord, Enum, Bounded )

instance Show Y where
  show Y1 = "1"
  show Y2 = "2"
  show Y3 = "3"
  show Y4 = "4"
  show Y5 = "5"
  show Y6 = "6"
  show Y7 = "7"
  show Y8 = "8"
  show Y9 = "9"

ys :: [Y]
ys = [minBound..maxBound]

data Point = Point X Y deriving ( Eq )

instance Show Point where
  show (Point x y) = "(" ++ (show x) ++ ", " ++ (show y)
    ++ ")"

instance Enum Point where
  fromEnum (Point x y) = 9 * (fromEnum x) + (fromEnum y)
  toEnum i = Point (toEnum (i `div` 9)) (toEnum (i `mod` 9))

instance Bounded Point where
  minBound = toEnum 0
  maxBound = toEnum 80

instance Ord Point where
  compare (Point x1 y1) (Point x2 y2) = (compare x1 x2) <> (compare y1 y2)

allPoints :: [Point]
allPoints = [minBound..maxBound]
