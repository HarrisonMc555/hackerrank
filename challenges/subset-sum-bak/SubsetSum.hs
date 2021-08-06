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
  in map (solve subSums) ss

solve :: [Int] -> Int -> Int
solve subSums s = -1
-- solve :: [Int] -> Int -> Int
-- -- solve xsSorted s = fromMaybe (-1) $ lengthOfGreaterSubset xsSorted s
-- solve xsSorted s = length xsSorted + s
                   -- 
lengthOfGreaterSubset :: [Int] -> Int -> Maybe Int
lengthOfGreaterSubset xsSorted s =
  findIndex (>= s) (scanl (+) 0 xsSorted)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)
