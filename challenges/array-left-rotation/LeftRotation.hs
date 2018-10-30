rotateLeft :: Int -> [a] -> [a]
rotateLeft k l = let (beg, end) = splitAt k l
                 in end ++ beg

main :: IO ()
main = do
  nd_string <- getLine
  arr_s <- getLine
  let [_, d] = map read . words $ nd_string :: [Int]
      arr = words arr_s
      arr_shifted = rotateLeft d arr
  putStrLn . unwords $ arr_shifted
