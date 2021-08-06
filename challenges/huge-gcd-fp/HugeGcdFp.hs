import Control.Monad (replicateM_)

main :: IO ()
main = do _ <- getLine
          aNumsS <- getLine
          _ <- getLine
          bNumsS <- getLine
          let aNums = map read $ words aNumsS :: [Int]
              bNums = map read $ words bNumsS :: [Int]
          print $ solve aNums bNums

solve :: [Int] -> [Int] -> Int
solve = const . const 1

myGcd :: Int -> Int -> Int
myGcd a b =
  if b == 0
  then a
  else myGcd b (a `mod` b)
