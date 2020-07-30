import Control.Monad (replicateM_)

main :: IO ()
main =
    do t <- readLn :: IO Int
       replicateM_ t $
         do n <- readLn :: IO Int
            print $ solve n

solve :: Int -> Int
solve = fibMod hackerRankLimit


fibMod :: Int -> Int -> Int
fibMod limit n = fibsMod limit !! n

fibsMod :: Int -> [Int]
fibsMod limit =
  let fibs = 0 : scanl addMod 1 fibs
      addMod x y = (x + y) `mod` limit
  in fibs

hackerRankLimit :: Int
hackerRankLimit = 10 ^ 8 + 7
