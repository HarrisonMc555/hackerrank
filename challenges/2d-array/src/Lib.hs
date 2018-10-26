{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Lib
    ( maxHourglassSum
    ) where

import Prelude hiding (map)
import Data.List


-- Complete the hourglassSum function below.
maxHourglassSum :: [[Int]] -> Int
maxHourglassSum arr = maximum . Data.List.map (hourglassSum arr) $ topLeftCorners
  where topLeftCorners = [(i, j) | i <- [0..length arr - 3],
                                   j <- [0..length (head arr) - 3]]


hourglassSum :: [[Int]] -> (Int, Int) -> Int
hourglassSum arr (i, j) = sum . Data.List.map (arr !!!) $ indices
    where indices = Data.List.map adddij dijs
          dijs = [(0, 0), (0, 1), (0, 2), (1, 1), (2, 0), (2, 1), (2, 2)]
          adddij (di, dj) = (i + di, j + dj)

(!!!) :: [[a]] -> (Int, Int) -> a
list !!! (i, j) = list !! i !! j
