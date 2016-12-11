module Problem35 where

import           Data.List (permutations)
import           Problem27 (isPrime)
import           Problem32 (digits, fromDigits)

--
-- Problem 35: Circular primes
--
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
-- 73, 79, and 97.
--
-- How many circular primes are there below one million?

problem35 :: Int
problem35 = length
  [ p
  | p <- [2..999999]
  , isPrime p
  , isCircularPrime p
  ]

-- |
-- Determine if a number is circular prime
--
-- >>> isCircularPrime 197
-- True
isCircularPrime :: Int -> Bool
isCircularPrime = all (isPrime . fromDigits) . rotations . digits

-- |
-- All rotations of a List
--
-- >>> rotations [1,2,3]
-- [[1,2,3],[2,3,1],[3,1,2]]
rotations :: [a] -> [[a]]
rotations = rotations' [] where
  rotations' _ []          = []
  rotations' acc ys@(z:zs) = (ys ++ acc) : rotations' (acc ++ [z]) zs
