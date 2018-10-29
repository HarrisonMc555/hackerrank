import Control.Monad (replicateM_)

validPreorder :: (Ord a, Bounded a) => [a] -> Bool
validPreorder = go minBound maxBound []
  where go :: (Ord a, Bounded a) => a -> a -> [a] -> [a] -> Bool
        go _ _ _ [] = True
        go minVal maxVal stack xs@(x:rest)
          | x < minVal = False
          | x >= maxVal = let minVal' = maxVal
                              (maxVal', stack') = boundPop stack
                          in go minVal' maxVal' stack' xs
          | otherwise = let maxVal' = x
                            stack' = boundPush stack maxVal
                        in go minVal maxVal' stack' rest

        boundPop :: (Bounded a) => [a] -> (a, [a])
        boundPop (x:xs) = (x, xs)
        boundPop [] = (maxBound, [])

        boundPush :: (Bounded a, Eq a) => [a] -> a -> [a]
        boundPush xs x
          | x == maxBound = xs
          | otherwise = x:xs


main :: IO ()
main = do
  n <- readLn :: IO Int
  replicateM_ n $ do
    _ <- readLn :: IO Int
    preorder_s <- getLine
    let preorder = map read . words $ preorder_s :: [Int]
        out_s = if validPreorder preorder then "YES" else "NO"
    putStrLn out_s
