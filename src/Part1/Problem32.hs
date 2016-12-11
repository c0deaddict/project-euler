module Part1.Problem32 where

import           Data.Char (digitToInt)
import           Data.List (group, permutations, sort, subsequences)

--
-- Problem 32: Pandigital products
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
-- through 5 pandigital.
--
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.
--
-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.
--
-- HINT: Some products can be obtained in more than one way so be sure to only
-- include it once in your sum.

problem32 :: Int
problem32 = sum . map head . group . sort $
  [ ab
  | alen <- [1..4]
  , a <- ordCombinations alen [1..9]
  , blen <- [1..(5-alen)]
  , b <- ordCombinations blen $ filter (`notElem` a) [1..9]
  , let ab = fromDigits a * fromDigits b
  , let allDigits = digits ab ++ a ++ b
  , isPandigital allDigits && length allDigits == 9
  ]

digits :: Int -> [Int]
digits = map digitToInt . show

fromDigits :: [Int] -> Int
fromDigits = read . concatMap show

isPandigital :: [Int] -> Bool
isPandigital xs = all (`elem` xs) [1..(length xs)]

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) (subsequences ns)

ordCombinations :: Int -> [a] -> [[a]]
ordCombinations k ns = concat $ permutations <$> combinations k ns
