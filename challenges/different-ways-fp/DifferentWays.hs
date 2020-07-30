import Control.Monad (replicateM_)

main :: IO ()
main =
    do t <- readLn :: IO Int
       replicateM_ t $
         do line <- getLine
            let [n, k] = map read . words $ line
            print $ solve n k

solve :: Integer -> Integer -> Integer
solve n k = nchoosek n k `mod` hackerRankLimit

nchoosek :: Integer -> Integer -> Integer
nchoosek n k = product [n-k+1..n] `div` product [1..k]

hackerRankLimit :: Integer
hackerRankLimit = 10 ^ 8 + 7
