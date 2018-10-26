module Lib
    ( solve
    ) where

solve :: Int -> Int -> Int
solve x n = let maxN = maxNum x n
                powered = [i^n | i <- [1..maxN]]
                combos = findCombos x powered
                results = map fst combos
            in length $ filter (== x) results

findCombos :: Int -> [Int] -> [(Int, [Int])]
findCombos x = foldr (addN x) []

addN :: Int -> Int -> [(Int, [Int])] -> [(Int, [Int])]
addN x n combos = (n, [n]) : nadded ++ combos
  where nadded = [ (sum' + n, n:nums)
                 | (sum', nums) <- combos
                 , sum' + n <= x]

maxNum :: Int -> Int -> Int
maxNum x n = floor $ x' ** (1.0 / n')
  where x' = fromIntegral x :: Double
        n' = fromIntegral n :: Double
