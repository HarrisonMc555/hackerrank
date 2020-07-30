import Control.Monad (replicateM_)
import Data.Ord (comparing, Ordering(..))
import Data.Foldable (maximumBy)

main :: IO ()
main = do t <- readLn :: IO Int
          replicateM_ t solveIO

solveIO :: IO ()
solveIO = do line <- getLine
             let [mario, luigi] = map read . words $ line :: [Int]
             print $ solve mario luigi

solve :: Int -> Int -> Int
solve x y =
  let num = gcd x y
      primeCounts = map snd $ primeFactorNums num
  in product $ map (+1) primeCounts

isPrime :: Integral int => int -> Bool
isPrime = flip elemSorted primes

primeFactorNums :: Integral int => int -> [(int, Int)]
primeFactorNums =
  let alterPair (prime, list) = (prime, length list)
  in map alterPair . chunkBy id . primeFactors

primeFactors :: Integral int => int -> [int]
primeFactors x
  | x <= 1 = []
  | otherwise = loop x $ takeWhile (<= squareRoot x) primes
  where loop x' [] = if x' == 1 then [] else [x']
        loop x' primes'@(prime:rest) =
          if prime `divides` x'
          then prime : loop (x' `div` prime) primes'
          else loop x' rest

primes :: Integral int => [int]
primes = 2 : filter isPrime [3..]
  where isPrime x = none (x `isDivisibleBy`) (possiblePrimeFactors x)

possiblePrimeFactors :: Integral int => int -> [int]
possiblePrimeFactors x = takeWhile (<= squareRoot x) primes

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Integral int => int -> int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
  let twoPows = iterate (^!2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n>=) . snd) $ zip (1:twoPows) twoPows
      newtonStep x = div (x + div n x) 2
      iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
      isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in head $ dropWhile (not . isRoot) iters

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x =
  let iterateMaybe' _ Nothing = []
      iterateMaybe' f' (Just x') = x' : iterateMaybe' f' (f' x')
  in iterateMaybe' f (Just x)

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (comparing f)

elemSorted :: Ord a => a -> [a] -> Bool
elemSorted x = loop
  where loop [] = False
        loop (y:ys) = case compare x y of
                        LT -> False
                        EQ -> True
                        GT -> loop ys

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f = all (not . f)

isDivisibleBy :: Integral int => int -> int -> Bool
isDivisibleBy x y = x `rem` y == 0

divides :: Integral int => int -> int -> Bool
divides = flip isDivisibleBy

chunkBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
chunkBy _ [] = []
chunkBy f (x:xs) =
  let chunkByHelper cur acc [] = [(cur, reverse acc)]
      chunkByHelper cur acc (y:ys) =
        let next = f y
        in if next == cur
           then chunkByHelper cur (y:acc) ys
           else (cur, reverse acc) : chunkByHelper next [y] ys
  in chunkByHelper (f x) [x] xs
