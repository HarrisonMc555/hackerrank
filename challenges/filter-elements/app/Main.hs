module Main where

import Lib (solve)
import Control.Monad (replicateM_)

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    nk_str <- getLine
    let [_, k] = map read . words $ nk_str :: [Int]
    nums_str <- getLine
    let nums = map read . words $ nums_str :: [Int]
        answers = solve k nums
        answers_strings = unwords . map show $ answers
        printAnswer = if null answers then "-1" else answers_strings
    putStrLn printAnswer
