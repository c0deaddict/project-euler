module Part1.Problem24 where

import           Data.List       (permutations)

--
-- Problem 24: Lexicographic permutations
--
-- A permutation is an ordered arrangement of objects. For example, 3124 is one
-- possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
-- are listed numerically or alphabetically, we call it lexicographic order.
-- The lexicographic permutations of 0, 1 and 2 are:
--
-- 012   021   102   120   201   210
--
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
-- 5, 6, 7, 8 and 9?


problem24 :: String
problem24 = concatMap show digits where
  digits = lexperms [0..9] !! 999999

-- |
-- >>> extract [0,1,2]
-- [(0,[1,2]), (1,[0,2]), (2,[0,1])]
extract :: [a] -> [(a, [a])]
extract = extract' [] where
  extract' _ []       = []
  extract' acc (x:xs) = (x, acc ++ xs) : extract' (acc ++ [x]) xs

lexperms :: [a] -> [[a]]
lexperms = lexperms' [] where
  lexperms' _ []    = []
  lexperms' acc [x] = [acc ++ [x]]
  lexperms' acc xs  = concatMap (lexpermsRec acc) (extract xs)
  lexpermsRec acc (x, xs) = lexperms' (acc ++ [x]) xs
