module Main where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad (replicateM)
import Data.List (sort, findIndex, sortBy)
import Data.Maybe (fromMaybe)

main :: IO ()
main =
    do _ <- readLn :: IO Int
       line <- getLine
       let xs = map read . words $ line :: [Int]
           -- xsSorted = sortDesc xs
       t <- readLn :: IO Int
       ss <- replicateM t readLn :: IO [Int]
       let solutions = solveAll xs ss
       print solutions
       -- print $ map (solve xsSorted) ss
       -- replicateM_ t $
       --   do s <- readLn :: IO Int
       --      print $ solve xsSorted s

solveAll :: [Int] -> [Int] -> [Int]
solveAll xs ss =
  let xsSorted = sortDesc xs
      subSums = scanl (+) 0 xsSorted
      subSumsV = V.fromList subSums
  in map (solve subSumsV) ss

solve :: Vector Int -> Int -> Int
solve subSums s =
-- TODO Need to do reverse binary search
  let index = binarySearch subSums s
  in if index >= V.length subSums
     then (-1)
     else index
  -- fromJust (-1) $ binarySearch 
-- solve :: [Int] -> Int -> Int
-- -- solve xsSorted s = fromMaybe (-1) $ lengthOfGreaterSubset xsSorted s
-- solve xsSorted s = length xsSorted + s
                   -- 
lengthOfGreaterSubset :: [Int] -> Int -> Maybe Int
lengthOfGreaterSubset xsSorted s =
  findIndex (>= s) (scanl (+) 0 xsSorted)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

-- TODO Need to do reverse binary search
binarySearch :: Ord a => Vector a -> a -> Int
binarySearch v x =
  let binarySearchHelper lo hi
        | lo >= hi = mid
        | midElem == x = mid
        | midElem < x = binarySearchHelper (mid + 1) hi
        | otherwise = binarySearchHelper lo (hi - 1)
        where mid = (lo + hi) `div` 2
              midElem = v V.! mid
  in binarySearchHelper 0 (V.length v)


-- binarySearch :: Ord a => Vector a -> a -> Int
-- binarySearch v x =
--   let binarySearchHelper lo hi
--         | lo < 0 = 0
--         | loElem < hiElem = mid
--         | mid >= V.length v = V.length v
--         | midElem == x = mid
--         | midElem > x = binarySearchHelper (mid + 1) hi
--         | otherwise = binarySearchHelper lo (hi - 1)
--         where mid = (lo + hi) `div` 2
--               midElem = v V.! mid
--               loElem = v V.! lo
--               hiElem = v V.! (hi - 1)
--   in binarySearchHelper 0 (V.length v)
