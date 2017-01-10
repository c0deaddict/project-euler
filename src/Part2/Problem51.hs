module Part2.Problem51 where

import           Data.List       (find)
import           Data.Maybe      (catMaybes, isJust, mapMaybe)
import           Part1.Problem27 (isPrime, primes)
import           Part1.Problem32 (digits, fromDigits)

--
-- Problem 51: Prime digit replacements
--
-- By replacing the 1st digit of the 2-digit number *3, it turns out that six
-- of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
--
-- By replacing the 3rd and 4th digits of 56**3 with the same digit, this
-- 5-digit number is the first example having seven primes among the ten
-- generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
-- 56773, and 56993. Consequently 56003, being the first member of this family,
-- is the smallest prime with this property.
--
-- Find the smallest prime which, by replacing part of the number (not
-- necessarily adjacent digits) with the same digit, is part of an eight prime
-- value family.

-- NOTE: result is not the smallest, run the answer through primeFamilies to
-- find the smallest one (with length 8).
problem51 :: Maybe Int
problem51 = find (isCandidate 8) primes where
  isCandidate size p = isJust $ find (size ==) $ map length $ primeFamilies p

primeFamilies :: Int -> [[Int]]
primeFamilies p = filter isPrime . mapSpots [1..9] <$> spots (digits p)

spots :: [Int] -> [[Maybe Int]]
spots digits = tail $ spots' digits [] where -- tail skips the one without spots
  spots' [] acc = [reverse acc]
  spots' (d:ds) acc =
    spots' ds (Just d:acc) ++ spots' ds (Nothing:acc)

fillSpots :: [Maybe Int] -> Int -> [Int]
fillSpots [] _           = []
fillSpots (Just d:ds) x  = d:fillSpots ds x
fillSpots (Nothing:ds) x = x:fillSpots ds x

mapSpots :: [Int] -> [Maybe Int] -> [Int]
mapSpots range spots = map (fromDigits . fillSpots spots) range
