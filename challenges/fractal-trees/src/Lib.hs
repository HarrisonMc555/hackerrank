module Lib
    ( fractalString
    ) where

import Prelude hiding (Left, Right)
import Data.List (intersperse)

data Direction = Left | Right
data Cell = Filled | Blank deriving (Show)

branchChar, blankChar :: Char
branchChar = '1'
blankChar = '_'

maxRows, maxCols :: Int
maxRows = 63
maxCols = 100

maxIteration :: Int
maxIteration = 5

fractalString :: Int -> String
fractalString = unlines . fractalToString . fractalTree

fractalTree :: Int -> [[Cell]]
fractalTree iteration = blankRows ++ map center (concat rows)
  where ns = [iteration, iteration-1..1]
        rows = map fractalRow ns
        numRows = maxRows - sum (map length rows)
        blankRows = blanks numRows maxCols

fractalRow :: Int -> [[Cell]]
fractalRow iteration = foldr alongside initial treesAndBlanks
  where size = 2 ^ (maxIteration - iteration)
        numTrees = 2^(iteration - 1)
        trees = replicate numTrees (tree size)
        numRows = size*2
        treesAndBlanks = edgeBlanks : intersperse blankColumns trees
                         ++ [edgeBlanks]
        initial = replicate numRows []
        blankColumns = blanks numRows numBlankCols
        numBlankCols = size*2 - 1
        numEdgeBlanks = numBlankCols `div` 2
        edgeBlanks = blanks numRows numEdgeBlanks

fractalToString :: [[Cell]] -> [String]
fractalToString = map (map toChar)
  where toChar c = case c of
          Filled -> branchChar
          Blank -> blankChar

tree :: Int -> [[Cell]]
tree size = leftBranch `alongside` blanks size 1 `alongside` rightBranch ++
            trunk'
  where leftBranch = branch size Left
        rightBranch = branch size Right
        trunk' = trunk size

branch :: Int -> Direction -> [[Cell]]
branch size d = case d of
               Right -> map reverse cells
               Left -> cells
  where cells = map toCells (enumerate1 $ replicate size [1..size])
        toCells (index, nums) = map (toCell index) nums
        toCell index num = if index == num then Filled else Blank

trunk :: Int -> [[Cell]]
trunk size = replicate size (blanks' ++ [Filled] ++ blanks')
  where blanks' = replicate size Blank

alongside :: [[a]] -> [[a]] -> [[a]]
a `alongside` b = [as ++ bs | (as, bs) <- zip a b]

enumerate1 :: [a] -> [(Int, a)]
enumerate1 = zip [1..]

blanks :: Int -> Int -> [[Cell]]
blanks numRows numCols = replicate numRows (replicate numCols Blank)

center :: [Cell] -> [Cell]
center cells = leftEdge ++ cells ++ rightEdge
  where leftEdge = replicate leftEdgeLength Blank
        rightEdge = replicate rightEdgeLength Blank
        leftEdgeLength = (maxCols - length cells) `div` 2
        rightEdgeLength = leftEdgeLength + 1
