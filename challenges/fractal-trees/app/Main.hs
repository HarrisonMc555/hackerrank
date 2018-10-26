module Main where

import Lib (fractalString)

main :: IO ()
main = do
  n <- readLn :: IO Int
  putStr . fractalString $ n
