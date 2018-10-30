import Control.Monad (replicateM)

gcdList :: [[(Int, Int)]] -> [(Int, Int)]
gcdList [] = []
gcdList (first:rest) = go first rest
  where go factors [] = factors
        go gcds (factors:rest) = go gcds' rest
          where gcds' = combine gcds factors
        combine :: Ord a => [(a, a)] -> [(a, a)] -> [(a, a)]
        combine _ [] = []
        combine [] _ = []
        combine l1@((f1,n1):xs1) l2@((f2,n2):xs2) =
          case compare f1 f2 of
            EQ -> (f1, min n1 n2) : combine xs1 xs2
            LT -> combine xs1 l2
            GT -> combine l1 xs2

toPairs :: [a] -> [(a, a)]
toPairs [] = []
toPairs (x:[]) = error "Odd list"
toPairs (x1:x2:xs) = (x1, x2) : toPairs xs

fromPairs :: [(a, a)] -> [a]
fromPairs [] = []
fromPairs ((x1, x2):xs) = x1:x2:fromPairs xs

main :: IO ()
main = do
  q <- readLn :: IO Int
  pairs <- replicateM q $ do
    nums_s <- getLine
    let nums = map read . words $ nums_s :: [Int]
        pairs = toPairs nums
    return pairs
  let answer = gcdList pairs
  putStrLn . unwords . map show . fromPairs $ answer
