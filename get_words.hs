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
import Data.List ((\\), sort)

newtype SortedWord = SortedWord (String, String) deriving Eq
instance Show SortedWord where show (SortedWord (a,_)) = show a
sortedWord s = SortedWord (s, sort s)

isSubset :: SortedWord -> String -> Bool
isSubset (SortedWord (_, little)) big = isSubset' little big

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

addWord :: String -> WordIndex -> WordIndex
addWord word index = IM.alter (Just . z word) (length word) index
  where z word maybeMap = addWord' word $ fromMaybe M.empty maybeMap

sizeIndex :: [String] -> WordIndex
sizeIndex words = foldl' (\m word -> addWord word m) IM.empty $ filter goodWord words

findSize :: Int -> WordIndex -> M.Map Char [SortedWord]
findSize n = fromMaybe M.empty . IM.lookup n 

grabJust :: String -> Int -> WordIndex -> [SortedWord]
grabJust xs n foo = (concat $ map (\k -> M.findWithDefault [] k $ findSize n foo) xs)

grabAll :: Int -> WordIndex -> [SortedWord]
grabAll n foo = concat . M.elems $ findSize n foo 

main = do
  -- TODO: this word list is pretty bogus
  words <- lines <$> readFile "/usr/share/dict/words"
  let index = sizeIndex words
  let sixers = filter (\w -> goodWord w && length w == 6)  words
  withFile "data" WriteMode $ \h -> do
    forM_ (sixers) $ \word -> do
      let rest = map (\n -> grabJust word n index) [3..5]
      hPrint h (word, map (filter (`isSubset` (sort word))) rest)

  putStrLn "Done."

