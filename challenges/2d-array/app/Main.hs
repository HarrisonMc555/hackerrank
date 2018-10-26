module Main where

import Lib

-- import Control.Monad
-- import Data.Array
-- import Data.Bits
import Data.List
-- import Data.List.Split
-- import Data.Set
-- import Debug.Trace
import System.Environment
import System.IO
-- import System.IO.Unsafe

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    arrTemp <- readMultipleLinesAsStringArray 6
    let arr = Data.List.map (\x -> Data.List.map (read :: String -> Int) . words $ x) arrTemp

    let result = maxHourglassSum arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
