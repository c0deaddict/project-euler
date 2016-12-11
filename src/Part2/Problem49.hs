module Part2.Problem49 where

import qualified Data.Map        as Map

import           Data.List       (find, permutations, sort)
import           Part1.Problem12 (combinations)
import           Part1.Problem27 (isPrime, primes)
import           Part1.Problem32 (digits)

--
-- Problem 49: Prime permutations
--
-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways:
-- (i) each of the three terms are prime, and,
-- (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
-- exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?

problem49 :: String
problem49 = concatMap show $ solutions !! 1 where
  fourDigitPrimes = dropWhile (< 10^3) $ takeWhile (< 10^4) primes
  indexedByDigits = map (\p -> (sort $ digits p, p)) fourDigitPrimes
  groupedOnDigits = sortAndGroup indexedByDigits
  enoughPermutations = Map.filter (\xs -> length xs >= 3) groupedOnDigits
  candidatesList = sort . snd <$> Map.toList enoughPermutations
  allCombinations = concatMap (combinations 3) candidatesList
  solutions = filter allDiffsEqual allCombinations
  allDiffsEqual xs = let d = diffs xs in all (== head d) d
  diffs xs = tail $ zipWith (-) xs (0:xs)

-- https://stackoverflow.com/a/12398993/248948
sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]
