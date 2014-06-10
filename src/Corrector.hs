module Corrector (correct) where


import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
import System.IO
import Data.Array

type BigramModel = Map.Map (T.Text, T.Text) Float
type UnigramModel = Map.Map T.Text Float



corpus :: String
corpus = "../data/corpus"


correct :: String -> String
correct x = x


-- | Edit distance
editDist :: String -> String -> Int
editDist s1 s2 = table ! (m, n) 
  where
    m = length s1
    n = length s2
    table = array ((0,0), (m,n)) [ (idx, dp idx) | idx <- range((0,0), (m,n)) ]
    dp (0, j) = j
    dp (i, 0) = i
    dp (i, j) = minimum $ [ 1 + table ! (i-1,j), 1 + table ! (i, j-1), sub i j ]
    sub i j = if s1 !! (i-1) == s2 !! (j-1) then table ! (i-1,j-1) else 1 + table ! (i-1, j-1)


-- | Load language model
loadLangModel :: IO LangModel
loadLangModel = do
  let modelname = "../data/model"
  hasmodel <- doesFileExist modelname
  case hasmodel of
    True -> read <$> (readFile modelname)
    _ -> do
      model <- buildLangModel
      writeFile modelname $ show model
      return model


-- | Build bigram model
buildLangModel :: IO LangModel
buildLangModel = do
  list <- getDirectoryContents corpus
  foldM buildModel Map.empty $ map ((corpus ++ "/") ++) $ filter (\x -> ".txt" `L.isSuffixOf` x) list
  where
    countBigram w = map (\x -> (x, 1)) $ zipWith ((,)) w $ tail w
    tokenize fn = T.words <$> T.readFile fn
    buildModel mp fn = do
      w <- tokenize fn
      return $ Map.unionWith (+) mp (Map.fromList $ countBigram w)










