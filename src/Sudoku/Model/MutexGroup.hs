module Sudoku.Model.MutexGroup
  ( MutexGroup
  , mutexGroups
  , allGroups
  , points
  , Row (..)
  , Column (..)
  , Block (..)
  , mutex
  ) where


import Data.List ( nub )
import Data.List.Split ( chunksOf )
import Data.Map.Strict ( Map, (!) )
import qualified Data.Map.Strict as Map ( fromList )
import Data.Semigroup ( (<>) )

import Sudoku.Model.Point ( Point (..), X (..), Y (..), xs, ys)


data Row = Row [Point] deriving ( Show )

data Column = Column [Point] deriving ( Show )

columnPoints :: Column -> [Point]
columnPoints (Column ps) = ps

data Block = Block [Point] deriving ( Show )

class MutexGroup a where
  allGroups :: [a]
  
  points :: a -> [Point]
  
  belongsTo :: Point -> a
  belongsTo p = mapping ! p

  mapping :: Map Point a
  mapping = Map.fromList $ do
    group <- allGroups
    p' <- points group 
    [(p', group)]

instance MutexGroup Row where
  points (Row ps) = ps
  
  allGroups = fmap (\y -> Row $ fmap (\x -> Point x y) xs) ys

instance MutexGroup Column where
  points (Column ps) = ps
  
  allGroups = fmap (\x -> Column $ fmap (\y -> Point x y) ys) xs

instance MutexGroup Block where
  points (Block ps) = ps
  
  allGroups = 
    let
      xss = chunksOf 3 xs
      yss = chunksOf 3 ys
      block xs' ys' = Block $ do
        x <- xs'
        fmap (Point x) ys'
    in
      do
        xs' <- xss
        fmap (\ys' -> Block $ do
                 x <- xs'
                 fmap (Point x) ys') yss

mutexGroups :: [[Point]]
mutexGroups =
  let
    rows = fmap points (allGroups :: [Row])
    columns = fmap points (allGroups :: [Column])
    blocks = fmap points (allGroups :: [Block])
  in rows ++ columns ++ blocks

mutex :: Point -> [Point]
mutex p = nub $
  (points ((belongsTo p) :: Row))
  ++ (points ((belongsTo p) :: Column))
  ++ (points ((belongsTo p) :: Block))
