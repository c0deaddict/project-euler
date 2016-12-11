module Part2.Problem41 where

import           Data.List       (find)
import           Data.Maybe      (mapMaybe)
import           Part1.Problem24 (lexperms)
import           Part1.Problem27 (isPrime)
import           Part1.Problem32 (digits, fromDigits, isPandigital)

--
-- Problem 41: Pandigital prime
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
-- also prime.
--
-- What is the largest n-digit pandigital prime that exists?

problem41 :: Int
problem41 = head domain where
  domain = mapMaybe largestPrime (reverse [1..9])
  largestPrime n = find isPrime (pandigitals n)
  pandigitals n = map fromDigits $ lexperms (reverse [1..n])
