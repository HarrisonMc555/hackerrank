{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (maxHourglassSum)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "maxHourglassSum" $ for_ cases test
  where
    test Case{..} = it description $ maxHourglassSum input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: [[Int]]
                 , expected    :: Int
                 }

cases :: [Case]
cases = [ Case { description = "one hourglass"
               , input = [[1, 1, 1], [1, 1, 1], [1, 1, 1]]
               , expected = 7
               }
        , Case { description = "two identical hourglasses vertically"
               , input = [[1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1]]
               , expected = 7
               }
        , Case { description = "two identical hourglasses horizontally"
               , input = [[1, 1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1]]
               , expected = 7
               }
        , Case { description = "two different hourglasses vertically"
               , input = [[1, 1, 1], [2, 2, 2], [3, 3, 3], [4, 4, 4]]
               , expected = 21
               }
        , Case { description = "two different hourglasses horizontally"
               , input = [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]]
               , expected = 21
               }
        ]
