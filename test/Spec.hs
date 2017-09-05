module Main (main) where

import Test.Hspec (hspec)
import Test1 (specTest1)

main :: IO ()
main = hspec specTest1
