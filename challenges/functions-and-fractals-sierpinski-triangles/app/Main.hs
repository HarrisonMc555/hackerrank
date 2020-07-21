module Main where

import Text.Read (readMaybe) 

maxN :: Int
maxN = 5

filled, empty :: Char
filled = '1'
empty = '_'

main :: IO ()
main = do xS <- getLine
          case readMaybe xS of
            Just x -> printResult x
            Nothing -> putStrLn $ "Invalid input " ++ show xS

printResult :: Int -> IO ()
printResult x =
  mapM_ putStrLn $ sierpinski x

sierpinski :: Int -> [String]
sierpinski x =
  sierpinskiTile (createTriangle x) x

createTriangle :: Int -> [String]
createTriangle x =
  let n = (maxN - x)
      numRows = 2 ^ n
  in [ replicate (numRows - i) empty ++
       replicate (2 * i - 1) filled ++
       replicate (numRows - i) empty
     | i <- [1..numRows] ]

sierpinskiTile :: [String] -> Int -> [String]
sierpinskiTile tile x =
  if x == 0 then tile
  else let prev = sierpinskiTile tile (x - 1)
           -- numColumns = 2 ^ (x - 1)
           numColumns = length prev
           top = map (surroundWith $ replicate numColumns empty) prev
           bottom = zipWith (insertBetween [empty]) prev prev
       in top ++ bottom

surroundWith :: [a] -> [a] -> [a]
surroundWith surround middle =
  surround ++ middle ++ surround

insertBetween :: [a] -> [a] -> [a] -> [a]
insertBetween middle beginning end =
  beginning ++ middle ++ end
