module Main where


import qualified Data.Set as Set
import Data.List ( zip )
import Data.Map.Strict ( fromList )

import Sudoku.Model.Value
import Sudoku.Model.Point
import Sudoku.Model.MutexGroup
import Sudoku.Model.Solution
import Sudoku.Parser.Parser
import Sudoku.Algorithm.Backtrack



main :: IO ()
main =
  let vs = unlines $
        [ "471695382"
        , "8 6 71549"
        , "593842671"
        
        , "3821   95"
        , "965783124"
        , "714259863"
        
        , "659418237"
        , "138927456"
        , "247536918"
        ]
      s  = parse vs
--  in putStrLn . show $ canBe s ( Point X2 Y2 )
    in putStrLn . show $ solve s
