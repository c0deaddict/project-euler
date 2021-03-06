module Part2.Problem45 where

import           Part1.Problem3  (squareRoot)
import           Part2.Problem42 (isTriangle)

--
-- Problem 45: Triangular, pentagonal, and hexagonal
--
-- Triangle, pentagonal, and hexagonal numbers are generated by the following
-- formulae:
--
-- Triangle	 	  Tn=n(n+1)/2	  	1, 3, 6, 10, 15, ...
-- Pentagonal	 	Pn=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
-- Hexagonal	 	Hn=n(2n−1)	 	  1, 6, 15, 28, 45, ...
-- It can be verified that T285 = P165 = H143 = 40755.
--
-- Find the next triangle number that is also pentagonal and hexagonal.

problem45 :: Int
problem45 = head $ dropWhile (<= 40755) domain where
  domain = filter cond pentagonals
  cond n = isTriangle n && isHexagonal n

pentagonals :: [Int]
pentagonals = [ n * (3*n - 1) `div` 2 | n <- [1..] ]

isHexagonal :: Int -> Bool
isHexagonal x = x == hex n || x == hex (n + 1) where
  hex n = n*(2*n - 1)
  hx = x `div` 2
  n = squareRoot hx
