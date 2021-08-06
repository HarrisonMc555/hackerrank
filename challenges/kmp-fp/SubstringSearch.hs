import Control.Monad (replicateM_)
import Data.Array (Array, listArray)

main :: IO ()
main =
    do t <- readLn :: IO Int
       replicateM_ t $
         do text <- getLine
            pat <- getLine
            print $ solve text pat

solve :: String -> String -> String
solve text pat =
  if pat `isSubstringOf` text
  then "YES"
  else "NO"

isSubstringOf :: String -> String -> Bool
text `isSubstringOf` pat = False

generateLps :: String -> Array Int Int
generateLps pat =
  let pat' = fromList pat
  in fromList []


fromList :: [a] -> Array Int a
fromList list =
  listArray (0, length list - 1) list
