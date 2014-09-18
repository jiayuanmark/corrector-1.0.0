module Corrector (correct) where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import Data.Binary
import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
import System.IO
import Data.Array
import Data.Int

type UnigramModel = M.Map B.ByteString Float
type BigramModel = M.Map (B.ByteString, B.ByteString) Float

data LangModel = Model (UnigramModel, BigramModel) deriving (Show)

instance Binary LangModel where
  put (Model m) = put m
  get = fmap Model get

correct :: String -> String
correct x = x

-- | Edit distance
editDist :: B.ByteString -> B.ByteString -> Int64
editDist s1 s2 = table ! (m, n)
  where
    m = B.length s1
    n = B.length s2
    table = array ((0,0), (m,n)) [ (idx, dp idx) | idx <- range((0,0), (m,n)) ]
    dp (0,j) = j
    dp (i,0) = i
    dp (i,j) = minimum $ [ 1 + table ! (i-1,j), 1 + table ! (i, j-1), sub i j ]
    sub i j = if B.index s1 (i-1) == B.index s2 (j-1) then table ! (i-1,j-1) else 1 + table ! (i-1, j-1)

-- | Load language model
loadLangModel :: IO LangModel
loadLangModel = do
  let modelname = "../data/model"
  hasmodel <- doesFileExist modelname
  if hasmodel
    then B.readFile modelname >>= return . decode
    else do
      model <- buildLangModel
      B.writeFile modelname (encode model)
      return model


-- | Build bigram model
buildLangModel :: IO LangModel
buildLangModel = do
  list <- getDirectoryContents corpus
  buildModel M.empty M.empty $ map ((++) (corpus ++ "/")) $ filter (L.isSuffixOf ".txt") list
  where
    corpus = "../data/corpus"
    tokenize = (B.words <$>) . B.readFile
    counterize tokens = map (flip (,) 1) tokens
    bigramize tokens = zipWith (,) tokens (tail tokens)

    buildModel ugram bgram fn | ugram `seq` bgram `seq` False = undefined 
    buildModel ugram bgram [] = return $ Model (ugram, bgram)
    buildModel ugram bgram (f:fs) = do
      tklist <- tokenize f
      let nugram = M.fromList $ counterize tklist
      let nbgram = M.fromList $ counterize $ bigramize tklist 
      buildModel (M.unionWith (+) ugram nugram) (M.unionWith (+) bgram nbgram) fs


-- | Generate candidates









