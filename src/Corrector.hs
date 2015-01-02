{-# LANGUAGE BangPatterns #-}

module Corrector (correct) where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Data.Map.Strict as M
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
config = Threshold 0.5 1

-- | Spelling correction
correct :: IO ()
correct = do
  putStrLn "Enter model and corpus directory..."
  [mdir, cdir] <- words <$> getLine
  langmdl <- loadLangModel mdir cdir
  let charidx = buildCharIndex langmdl
  putStrLn "Model loaded."
  input <- B.words . B.pack <$> getLine
  putStrLn $ B.unpack $ B.unwords $ inference langmdl charidx config input

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
buildCharIndex :: LangModel -> CharGramIndex
buildCharIndex (Model (!ugram, _)) = M.fromListWith (++) $ concatMap flat $ M.keys ugram
  where flat token = map (\g -> (g, [token])) $ B.zip token (B.tail token)

-- | Generate candidates
getCandidate :: CharGramIndex -> Threshold -> B.ByteString -> [B.ByteString]
getCandidate indx t word = filter editflt $ map fst $ filter jacdflt score
  where grams     = B.zip word (B.tail word)
        score     = map (\x->(head x, length x)) $! (L.group . L.sort) $ concatMap find grams
        find g    = case M.lookup g indx of
                      Nothing -> []
                      Just wl -> wl
        n         = fromIntegral $ length grams
        jacdflt x = fromIntegral (snd x) >= (jaccard t) * n
        editflt x = editDist word x <= levedit t

-- | Markov chain inference
inference :: LangModel -> CharGramIndex -> Threshold -> [B.ByteString] -> [B.ByteString]
inference _ _ _ [] = []
inference (Model (!ugram,!bgram)) !chidx !param wds = infer (tail wds) $ initprob
  where
    n                = sum $ M.elems ugram
    score table g    = case M.lookup g table of
                         Nothing -> 0
                         Just c  -> c
    prob  w          = score ugram w / n 
    cprb  w2 w1      = score bgram (w1,w2) / score ugram w1
    candidates       = getCandidate chidx param
    -- table structured as a list of (score, word)
    initprob         = map (\x->(prob x, x)) $! candidates (head wds)
    -- forward-backward inference
    infer []     !table   = [ snd $ L.foldl1' max table ]
    infer (w:ws) !table   = sol : sols
      where
        rank w' = \(s, w'') -> (s * cprb w' w'', w'')
        res     = map (\x -> (x, L.foldl1' max $ map (rank x) table)) $! candidates w
        sols    = infer ws $! flip map res (\(w',(s,_)) -> (s,w'))
        sol     = let match = head sols
                  in (snd . snd . head) $ dropWhile ((/=) match . fst) res




