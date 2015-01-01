-- | Test spelling corrector
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Corrector

main :: IO ()
main = hspec $ describe "Testing Corrector" $ do

  -- example quickcheck test in hspec.
  describe "correct" $ do
    it "is an identity" $ property $
      \x -> (id x) == (x :: String)
