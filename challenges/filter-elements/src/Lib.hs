module Lib
    ( solve
    ) where

import Data.Map.Ordered (OMap, empty, assocs, (|>))
import qualified Data.Map.Ordered (lookup)

solve :: Int -> [Int] -> [Int]
solve k xs = [head nums | nums <- group xs, length nums >= k]

group :: Ord a => [a] -> [[a]]
group = map snd . groupBy id

groupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupBy f = assocs . foldl add empty
  where add m a = insertWith (flip (++)) (f a) [a] m

insertWith :: Ord k => (a -> a -> a) -> k -> a -> OMap k a -> OMap k a
insertWith f k a m = m |> (k, result)
  where result = case Data.Map.Ordered.lookup k m of
          Just a' -> f a a'
          Nothing -> a
  
