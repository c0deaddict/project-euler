module Part1.Problem22 where

import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char                  (ord)
import           Data.List                  (sort)
import           Network.HTTP.Conduit


--
-- Problem 22: Names scores
--
-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
-- containing over five-thousand first names, begin by sorting it into
-- alphabetical order. Then working out the alphabetical value for each name,
-- multiply this value by its alphabetical position in the list to obtain a name
-- score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which
-- is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
-- would obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?

problem22 :: IO Integer
problem22 = do
  str <- fetchNames
  let names = sort (parseNames str)
      worths = map nameWorth names
      values = zipWith (*) worths [1..]
  return $ sum values

nameWorth :: String -> Integer
nameWorth = sum . map (fromIntegral . letterWorth) where
  letterWorth ch = ord ch - 64

fetchNames :: IO String
fetchNames = do
  bs <- simpleHttp "https://projecteuler.net/project/resources/p022_names.txt"
  return $ L.unpack bs

parseNames :: String -> [String]
parseNames = wordsWhen (== ',') . unquote

-- https://stackoverflow.com/a/4981265/248948
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

unquote :: String -> String
unquote = filter (/= '"')
