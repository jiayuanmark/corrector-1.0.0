-- | Test spelling corrector
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.ByteString.Lazy.Char8 as B

import Corrector

main :: IO ()
main = hspec $ describe "Testing Corrector" $ do

  -- example quickcheck test in hspec.
  describe "example" $ do
    it "is an identity" $ property $
      \x -> (id x) == (x :: String)
  
  -- edit distance to itself should be zero.
  describe "edit distance" $ do
  	it "same string "  $ property $
  	  editDist (B.pack "hello") (B.pack "hello") == 0

