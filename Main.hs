module Main where

import qualified PreprocessData
import qualified CompareWeeks

main :: IO ()
main = do
    PreprocessData.main
    CompareWeeks.main
