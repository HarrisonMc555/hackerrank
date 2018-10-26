module Lib
    ( solve
    ) where

import Data.List (sortBy)
import Data.Map (empty, toList, insertWith, size)

solve :: Int -> [Int] -> [Int]
solve k xs = [head nums | nums <- group xs, length nums >= k]

group :: Ord a => [a] -> [[a]]
group = map snd . groupBy id

groupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupBy f = map removeIndex . sortBy comp . toList . foldl add empty
  where add m a = insertWith append (f a) (size m, [a]) m
        append (_, a) (index, a') = (index, a ++ a')
        removeIndex (b, (_, a)) = (b, a)
        comp (_, (i1, _)) (_, (i2, _)) = compare i1 i2

-- insertWith :: Ord k => (a -> a -> a) -> k -> a -> [(k, a)] -> [(k, a)]
-- insertWith f k a l = insert (k, result)
--   where result = case lookup k l of
--           Just a' -> f a a'
--           Nothing -> a
