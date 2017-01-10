module Part2.Problem50 where

import           Data.Function   (on)
import           Data.List       (maximumBy)
import           Data.Maybe      (catMaybes)
import           Part1.Problem27 (isPrime, primes)

--
-- Problem 50: Consecutive prime sum
--
-- The prime 41, can be written as the sum of six consecutive primes:
--
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
--
-- This is the longest sum of consecutive primes that adds to a prime below
-- one-hundred.
--
-- The longest sum of consecutive primes below one-thousand that adds to a
-- prime, contains 21 terms, and is equal to 953.
--
-- Which prime, below one-million, can be written as the sum of the most
-- consecutive primes?

problem50 :: (Int, Int)
problem50 = maximumBy (compare `on` snd) domain where
  domain = catMaybes $ consecutivePrimeSums 1000000 primes

consecutivePrimeSums :: Int -> [Int] -> [Maybe (Int, Int)]
consecutivePrimeSums limit xs@(y:ys) =
  if y >= limit then [] else
    consecutivePrimeSum limit xs : consecutivePrimeSums limit ys

consecutivePrimeSum :: Int -> [Int] -> Maybe (Int, Int)
consecutivePrimeSum limit (x:xs) = recurse x xs 1 where
  recurse acc _ _ | acc >= limit = Nothing
  recurse acc (y:ys) n =
    case recurse (acc + y) ys (n + 1) of
      Nothing -> if isPrime acc then Just (acc, n) else Nothing
      x       -> x
