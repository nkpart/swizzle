module Main where

import Data.List
import Data.Ord
import System.Process
import Control.Applicative ((<$>))
import Data.Maybe
import Data.Foldable (forM_)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import System.Exit
import System.IO
import Data.Char (isLower)

data Puzzle = Puzzle String [String] deriving (Eq, Read)

instance Show Puzzle where
  show (Puzzle word vs) = "[" ++ show word ++ ", " ++ show vs ++ "]"

newtype SortedWord = SortedWord (String, String) deriving Eq
instance Show SortedWord where show (SortedWord (a,_)) = show a
originalWord (SortedWord (s, _)) = s
sortedWord s = SortedWord (s, sort s)

isSubset :: SortedWord -> String -> Bool
isSubset (SortedWord (_, little)) = isSubset' little

isSubset' :: String -> String -> Bool
isSubset' [] _ = True
isSubset' (_:_) [] = False
isSubset' (x:xs) (y:ys) = (x == y && isSubset' xs ys) || (x > y && isSubset' (x:xs) ys)

goodWord [] = False
goodWord (x:[]) = False
goodWord (x:y:[]) = False
goodWord xs = isLower $ head xs -- this is the best check for improving performance

type WordIndex = IM.IntMap (M.Map Char [SortedWord])

addWord' :: String -> M.Map Char [SortedWord] -> M.Map Char [SortedWord]
addWord' word m = M.alter (Just . z (sortedWord word)) (head word) m
  where z f = maybe [f] (f:)

addWord :: WordIndex -> String -> WordIndex
addWord index word = IM.alter (Just . z word) (length word) index
  where z word maybeMap = addWord' word $ fromMaybe M.empty maybeMap

sizeIndex :: [String] -> WordIndex
sizeIndex words = foldl' addWord IM.empty $ filter goodWord words

findSize :: Int -> WordIndex -> M.Map Char [SortedWord]
findSize n = fromMaybe M.empty . IM.lookup n 

v n foo k = M.findWithDefault [] k $ findSize n foo

grabJust :: String -> Int -> WordIndex -> [[SortedWord]]
grabJust xs n foo = map (v n foo) xs

grabAll :: Int -> WordIndex -> [SortedWord]
grabAll n foo = concat . M.elems $ findSize n foo 

checkWord word acc w = if (w `isSubset` sort word) then w:acc else acc 

doWord index h word = 
  let rest = map (\n -> grabJust word n index) [3..6]
      g = foldl' (checkWord word)
      children = (foldl' (foldl' g) [] rest) :: [SortedWord]
   in hPrint h $ Puzzle word (map originalWord $ nub children) -- TODO, how do dupes happen 
  
getWords = lines <$> readFile "good-words"

buildIndex words = do
  -- TODO: this word list is pretty bogus
  let index = sizeIndex words
  let sixers = filter (\w -> goodWord w && length w == 6)  words
  return (index, sixers)

processIt f (index, sixers) = withFile f WriteMode $ forM_ sixers . doWord index

main = do
  getWords >>= buildIndex >>= processIt "puzzles"
  putStrLn "Done."

