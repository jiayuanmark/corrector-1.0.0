module Corrector (correct) where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import qualified System.FilePath.Posix as Path

import Data.Binary
import Control.Applicative
import System.Directory
import Data.Array
import Data.Int

type UnigramModel  = M.Map B.ByteString Double
type BigramModel   = M.Map (B.ByteString, B.ByteString) Double
type CharGramIndex = M.Map (Char, Char) [B.ByteString]

data LangModel = Model (UnigramModel, BigramModel)
                 deriving (Show)

instance Binary LangModel where
  put (Model m) = put m
  get = fmap Model get

data Threshold = Threshold { jaccard :: Double
                           , levedit :: Int64
                           }
                 deriving (Eq, Show)

-- | Top-level configuration
config :: Threshold
config = Threshold 0.7 2

-- | Spelling correction
correct :: IO ()
correct = do
  langmodel <- loadLangModel "../data/model" "../data/corpus"
  let charmodel = buildCharGramIndex langmodel
  input <- B.words . B.pack <$> getLine
  putStrLn $ B.unpack $ B.unwords $ inference input langmodel charmodel config

-- | Edit distance
editDist :: B.ByteString -> B.ByteString -> Int64
editDist s1 s2 = table ! (m, n)
  where m        = B.length s1
        n        = B.length s2
        table    = array ((0,0), (m,n)) [ (idx, dp idx) | idx <- range((0,0), (m,n)) ]
        dp (0,j) = j
        dp (i,0) = i
        dp (i,j) = minimum $ [ 1 + table ! (i-1,j), 1 + table ! (i, j-1), sub i j ]
        sub i j  = if B.index s1 (i-1) == B.index s2 (j-1)
                   then table ! (i-1,j-1)
                   else 1 + table ! (i-1, j-1)

-- | Load language model
loadLangModel :: FilePath -> FilePath -> IO LangModel
loadLangModel modelname corpus = do
  hasmodel <- doesFileExist modelname
  case hasmodel of
    True  -> B.readFile modelname >>= return . decode
    False -> do model <- buildLangModel corpus
                B.writeFile modelname (encode model)
                return model

-- | Build bigram model
buildLangModel :: FilePath -> IO LangModel
buildLangModel dir = do
  files <- filter (L.isSuffixOf ".txt") <$> getDirectoryContents dir
  buildModel M.empty M.empty $ map (Path.combine dir) files
  where
    tokenize = (B.words <$>) . B.readFile
    counterize tokens = map (flip (,) 1) tokens
    bigramize tokens = zipWith (,) tokens (tail tokens)

    buildModel ugram bgram _ | ugram `seq` bgram `seq` False = undefined 
    buildModel ugram bgram [] = return $ Model (ugram, bgram)
    buildModel ugram bgram (f:fs) = do
      tklist <- tokenize f
      let nugram = M.fromList $ counterize tklist
      let nbgram = M.fromList $ counterize $ bigramize tklist
      buildModel (M.unionWith (+) ugram nugram) (M.unionWith (+) bgram nbgram) fs

-- | Build character gram index
buildCharGramIndex :: LangModel -> CharGramIndex
buildCharGramIndex (Model (ugram, _)) = M.fromListWith (++) $ concatMap flat $ M.keys ugram
  where flat token = map (\g -> (g, [token])) $ B.zip token (B.tail token)

-- | Generate candidates
getCandidate :: B.ByteString -> CharGramIndex -> Threshold -> [B.ByteString]
getCandidate word indx t = filter editflt $ map fst $ filter jacdflt score
  where gram      = B.zip word (B.tail word)
        score     = M.toList $ M.fromListWith (+) $ concatMap find gram
        find g    = case M.lookup g indx of
                      Nothing -> []
                      Just ws -> map (\w ->(w,1)) ws
        norm x    = ((/) `F.on` fromIntegral) x (B.length word)
        jacdflt x = norm (snd x) >= jaccard t
        editflt x = editDist word x <= levedit t

-- | Markov chain inference
inference :: [B.ByteString] -> LangModel -> CharGramIndex -> Threshold -> [B.ByteString]
inference [] _ _ _ = []
inference wds (Model (ugram,bgram)) char t = infer (tail wds) $ initprob
  where
    n                = sum $ M.elems ugram
    find table g     = case M.lookup g table of
                         Nothing -> 0
                         Just c  -> c
    prob  w          = (find ugram w) / n 
    cprb  w2 w1      = (find bgram (w1, w2)) / (find ugram w1)
    initprob         = map (\x->(x, prob x)) $ getCandidate (head wds) char t
    infer [] table   = [fst $ L.maximumBy (compare `F.on` snd) table]
    infer (w:ws) table = sol : backtrack
      where
        rank w'      = compare `F.on` \(w'',s) -> s * cprb w' w''
        cw           = getCandidate w char t  
        res          = map (\x -> (x, L.maximumBy (rank x) table)) cw
        eval (w',(w'',s)) = (w', s * cprb w' w'')
        backtrack    = infer ws $ map eval res
        sol          = (fst . snd . head) $ dropWhile (\(w',_) -> w' /= head backtrack) res




