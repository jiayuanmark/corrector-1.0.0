-- | Runner for Corrector.
module Main (main) where

import Corrector
import System.IO


-- | Run our corrector.
main :: IO ()
main = do
	s <- getLine
	putStrLn $ correct s


