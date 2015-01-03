{-# LANGUAGE BangPatterns #-}

module Corrector (correct, editDist) where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.FilePath.Posix as Path

import Data.Array (Array)
import Data.Array.IArray (array, (!), range)
import Data.Array.Unboxed (UArray)
import Data.Binary
import Data.Function (on)
import Debug.Trace (trace)
import Control.Applicative
import System.Directory


type UnigramModel  = M.Map B.ByteString Double
type BigramModel   = M.Map (B.ByteString, B.ByteString) Double
type CharGramIndex = M.Map (Char, Char) [B.ByteString]

data LangModel = Model UnigramModel BigramModel
                 deriving (Show)

instance Binary LangModel where
  put (Model u b) = put u >> put b
  get = do u <- get :: Get UnigramModel
           b <- get :: Get BigramModel
           return $ Model u b 

data Threshold = Threshold { jaccard :: Double
                           , levedit :: Int
                           , lambda  :: Double
                           } deriving (Eq, Show)

-- | Top-level configuration
config :: Threshold
config = Threshold { jaccard = 0.3
                   , levedit = 2
                   , lambda  = 0.8
                   }

-- | Spelling correction
correct :: IO ()
correct = do
  putStrLn "Enter model and corpus directory..."
  [mdir, cdir] <- L.words <$> getLine
  langmdl <- loadLangModel mdir cdir
  let chidx = buildCharIndex langmdl
  putStrLn "Model loaded."
  input <- B.words . B.pack <$> getLine
  putStrLn . B.unpack . B.unwords $ inference langmdl chidx config input

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
    buildModel ugram bgram [] = return $ Model ugram bgram
    buildModel ugram bgram (f:fs) = do
      tklist <- tokenize f
      let nugram = M.fromList $ counterize tklist
      let nbgram = M.fromList $ counterize $ bigramize tklist
      buildModel (M.unionWith (+) ugram nugram) (M.unionWith (+) bgram nbgram) fs

-- | Edit distance
editDist :: B.ByteString -> B.ByteString -> Int
editDist b1 b2 | trace ("edit distance between " ++ (show b1) ++ " and " ++ (show b2)) False = undefined
editDist b1 b2 = table ! (m,n)
  where s1       = B.unpack b1
        s2       = B.unpack b2
        m        = length s1
        n        = length s2

        a1      :: UArray Int Char
        a1       = array (1,m) (zip [1..m] s1)

        a2      :: UArray Int Char
        a2       = array (1,n) (zip [1..n] s2)

        bnds     = ((0,0), (m,n))
        table   :: Array (Int,Int) Int
        table    = array bnds [ (ij, dp ij) | ij <- range bnds ]
        
        dp (0,j) = j
        dp (i,0) = i
        dp (i,j) = L.foldl1' min [ 1 + table ! (i-1,j), 1 + table ! (i,j-1), sub i j ]
        sub i j  = if a1 ! i == a2 ! j
                   then table ! (i-1,j-1)
                   else 1 + table ! (i-1, j-1)

-- | Build character gram index
buildCharIndex :: LangModel -> CharGramIndex
buildCharIndex (Model !ugram _) = M.fromListWith (++) $ concatMap flat $ M.keys ugram
  where flat token = map (\g -> (g, [token])) $ B.zip token (B.tail token)

-- | Generate candidates
getCandidate :: CharGramIndex -> Threshold -> B.ByteString -> [B.ByteString]
getCandidate _ _ w | trace ("candidate generation for: " ++ show w) False = undefined
getCandidate indx t word = filter eflt (map fst (filter jflt score))
  where gram w      = S.fromList $ B.zip w (B.tail w)
        find g      = case M.lookup g indx of
                        Nothing -> []
                        Just wl -> zip wl [1,1..]
        grams       = gram word
        -- (w, intersection)
        score      :: [ (B.ByteString, Int) ]
        score       = M.toList $ M.fromListWith (+) (concatMap find (S.toList grams))
        denom w     = S.size $ S.union grams (gram w)
        -- jaccard distance filter
        jflt  (w,i) = ((/) `on` fromIntegral) i (denom w) >= jaccard t
        -- edit distance filter
        eflt  w     = editDist word w <= levedit t

-- | Markov chain inference
inference :: LangModel -> CharGramIndex -> Threshold -> [B.ByteString] -> [B.ByteString]
inference _ _ _ w | trace ("inference of: " ++ show w) False = undefined
inference _ _ _ [] = []
inference (Model !ugram !bgram) !chidx !param wds = infer (tail wds) $ initprob
  where
    n                = sum $ M.elems ugram
    count table g    = case M.lookup g table of
                         Nothing -> 0
                         Just c  -> c
    c1               = lambda param
    c2               = 1 - lambda param
    -- smoothed bigram probability
    prb  w           = count ugram w / n 
    cprb w2 w1       = c1 * (count bgram (w1,w2)) / (count ugram w1) + c2 * (prb w1)
    -- table structured as a list of (score, word)
    candidates       = getCandidate chidx param
    initprob         = map (\x->(log (prb x), x)) $! candidates (head wds)
    -- forward-backward inference
    infer _ t | trace ("infer: " ++ show t) False = undefined
    infer []     !table   = [ snd $ L.foldl1' max table ]
    infer (w:ws) !table   = sol : sols
      where
        rank w' = \(s, w'') -> (s + log (cprb w' w''), w'')
        res     = map (\x -> (x, L.foldl1' max $ map (rank x) table)) $! candidates w
        sols    = infer ws $! flip map res (\(w',(s,_)) -> (s,w'))
        sol     = let match = head sols
                  in (snd . snd . head) $ dropWhile ((/=) match . fst) res



