module Main where

import Lib (solve)

main :: IO ()
main = do
    _x <- getLine
    _n <- getLine
    let x = read _x :: Int
        n = read _n :: Int
    print $ solve x n
