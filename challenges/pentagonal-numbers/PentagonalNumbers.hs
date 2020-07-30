import Control.Monad (replicateM_)

main :: IO ()
main = do t <- readLn :: IO Int
          replicateM_ t $ do
            n <- readLn :: IO Int
            print $ solve n

solve :: Int -> Int
solve = pentagonalNum

pentagonalNum :: Int -> Int
pentagonalNum n = n * (3 * n - 1) `div` 2
