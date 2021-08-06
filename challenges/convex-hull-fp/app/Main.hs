module Main where

import Data.Maybe (fromMaybe)
import Data.List (sort, splitAt)
import Text.Printf

type Point = (Int, Int)

-- A hull starts at the left-most point and goes clockwise around. The "top"
-- part contains the left-most point up until just before the right-most point
-- (going clockwise). The "bottom" part contains the right-most point until just
-- before the left-most point (still going clockwise).
--
-- If there is only one point, then the point will be in the "top" part and the
-- "bottom" part will be empty.
data Hull = Hull { top :: ListNE Point
                 , bottom :: [Point]
                 } deriving (Show, Eq)

solve :: ListNE Point -> Double
solve points =
  let hull = convexHull . sortNE $ points
  in distance $ leftCW hull

convexHull :: ListNE Point -> Hull
convexHull points =
  let result = do (left, right) <- splitInHalfNE $ toListNE points
                  let leftHull = convexHull left
                      rightHull = convexHull right
                  return $ combine leftHull rightHull
      solo = Hull points []
  in fromMaybe solo result

-- The hull starts at the left-most point and goes clockwise around
combine :: Hull -> Hull -> Hull
combine left right =
  let newTop = findTop left right
      newBottom = findBottom left right
  in Hull newTop (toListNE newBottom)

findTop, findBottom :: Hull -> Hull -> ListNE Point
findTop left right = top left
  -- findTopHelper [] [] (leftCW left) (rightCCW right)

-- findTopHelper :: [Point] -> [Point] -> [Point] -> [Point] -> [Point]
-- findTopHelper
-- -- findTopHelper accumLeft accumRight
-- findTopHelper accumLeft accumRight left [] =
--   finishFindTopHelper accumLeft accumRight left []
-- findTopHelper accumLeft accumRight [] right =
--   finishFindTopHelper accumLeft accumRight [] right
-- findTopHelper accumLeft accumRight left right@[_] =
--   finishFindTopHelper accumLeft accumRight left right
-- findTopHelper accumLeft accumRight left@[_] right =
--   finishFindTopHelper accumLeft accumRight left right
-- findTopHelper accumLeft accumRight (l1:l2:ls) (r1:r2:rs) =
--   let curSlope = slope l1 r1
--   in if slope l1 r2 > curSlope
--      then findTopHelper accumLeft (r1:accumRight) (l1:l2:ls) (r2:rs)
--      else if slope l2 r1 < curSlope
--           then findTopHelper (l1:accumLeft) accumRight (l2:ls) (r1:r2:rs)
--           else finishFindTopHelper accumLeft accumRight [] []

finishFindTopHelper :: [Point] -> [Point] -> [Point] -> [Point] -> [Point]
finishFindTopHelper accumLeft accumRight left right =
  reverse accumLeft ++ accumRight

findBottom left right = top left

leftCW, leftCCW, rightCW, rightCCW :: Hull -> ListNE Point
leftCW hull =
  (top hull) `appendNEL` (bottom hull)
rightCW hull =
  (bottom hull) `appendLNE` (top hull)
leftCCW hull =
  let (ListNE headTop tailTop) = top hull
  in ListNE headTop $ reverse (bottom hull) ++ tailTop
rightCCW hull =
  take 1 (bottom hull) `appendLNE` reverseNE (top hull) 
  let (ListNE headTop tailTop) = top hull
  in ListNE headTop $ reverse (bottom hull) ++ tailTop
  take 1 (bottom hull) ++ reverse (top hull) ++ reverse (drop 1 $ bottom hull)

splitInHalf :: [a] -> ([a], [a])
splitInHalf list =
  let len = length list
      halfLen = len `div` 2
  in splitAt halfLen list

distance :: [Point] -> Double
distance =
  sum . map (uncurry pointDistance) . circularPairs

pointDistance :: Point -> Point -> Double
pointDistance (x1, y1) (x2, y2) =
  let xd = abs $ x1 - x2
      yd = abs $ y1 - y2
      xd2 = fromIntegral xd ** 2
      yd2 = fromIntegral yd ** 2
  in sqrt $ xd2 + yd2

slope :: Point -> Point -> Float
slope (x1, y1) (x2, y2) =
  let xd = x2 - x1
      yd = y2 - y1
  in fromIntegral yd / fromIntegral xd

circularPairs :: [a] -> [(a, a)]
circularPairs [] = []
circularPairs [x] = []
circularPairs [x1, x2] = [(x1, x2)]
circularPairs (x1:xs@(x2:x3:_)) =
  let last = lastSafe x3 xs
  in (last, x1) : (x1, x2) : pairs xs

pairs :: [a] -> [(a, a)]
pairs (x:xs@(y:_)) = (x, y) : pairs xs
pairs _ = []

lastSafe :: a -> [a] -> a
lastSafe x [] = x
lastSafe _ [x] = x
lastSafe x (_:xs) = lastSafe x xs

itemAtSafe :: [a] -> Int -> Maybe a
itemAtSafe [] _ = Nothing
itemAtSafe (x:xs) i
  | i < 0 = Nothing
  | i == 0 = Just x
  | otherwise = itemAtSafe xs (i - 1)

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans


{-
5     x
4   x   x
3 x       x
2   x   x
1     x
+ 1 2 3 4 5


-}

sample1 :: Hull
sample1 = Hull (ListNE (1,3) [(2,4),(3,5),(4,4)]) [(5,3),(4,2),(3,1),(2,2)]

sampleList1 :: [Point]
sampleList1 = [(1,3),(2,2),(2,4),(3,1),(3,5),(4,2),(4,4),(5,3)]


{-
5     x
4   x   x
3 x       x
2   x   x
1     x
+ 6 7 8 9 10


-}

sample2 :: Hull
sample2 = Hull (ListNE (6,3) [(7,4),(8,5),(9,4)]) [(10,3),(9,2),(8,1),(7,2)]

sampleList2 :: [Point]
sampleList2 = [(6,3),(7,2),(7,4),(8,1),(8,5),(9,2),(9,4),(10,3)]

--------------------------------------------------------------------------------

data ListNE a = ListNE a [a] deriving (Eq, Show)

headNE :: ListNE a -> a
headNE (ListNE x _) = x

tailNE :: ListNE a -> [a]
tailNE (ListNE _ xs) = xs

consNE :: a -> ListNE a -> ListNE a
consNE x (ListNE y ys) = ListNE x (y : ys)

unconsNE :: ListNE a -> (a, [a])
unconsNE (ListNE x xs) = (x, xs)

fromListNE :: [a] -> Maybe (ListNE a)
fromListNE [] = Nothing
fromListNE (x:xs) = Just $ ListNE x xs

toListNE :: ListNE a -> [a]
toListNE (ListNE x xs) = x : xs

splitInHalfNE :: [a] -> Maybe (ListNE a, ListNE a)
splitInHalfNE xs =
  do let (left, right) = splitInHalf xs
     leftNE <- fromListNE left
     rightNE <- fromListNE right
     return (leftNE, rightNE)

sortNE :: Ord a => ListNE a -> ListNE a
sortNE (ListNE x xs) =
  let sortedXs = sort xs
  in insertIntoSortedNE x sortedXs

insertIntoSortedNE :: Ord a => a -> [a] -> ListNE a
insertIntoSortedNE x [] = ListNE x []
insertIntoSortedNE x yAll@(y:ys) =
  if x < y
  then ListNE x yAll
  else ListNE y $ sort $ x : ys

appendNENE :: ListNE a -> ListNE a -> ListNE a
appendNENE (ListNE x xs) (ListNE y ys) = ListNE x (xs ++ [y] ++ ys)

appendNEL :: ListNE a -> [a] -> ListNE a
appendNEL (ListNE x xs) ys = ListNE x (xs ++ ys)

appendLNE :: [a] -> ListNE a -> ListNE a
appendLNE [] ne = ne
appendLNE (x:xs) (ListNE y ys) = ListNE x (xs ++ y : ys)

reverseNE :: ListNE a -> ListNE a
reverseNE (ListNE x xs) = appendLNE (reverse xs) (ListNE x [])
