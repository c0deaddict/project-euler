module Part2.Problem60 where

import           Data.List       (group, sort)
import           Part1.Problem12 (combinations)
import           Part1.Problem27 (isPrime, primes)
import           Part1.Problem32 (digits, fromDigits)

--
-- Problem 60: Prime pair sets
--
-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
-- and concatenating them in any order the result will always be prime.
-- For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these
-- four primes, 792, represents the lowest sum for a set of four primes with
-- this property.
--
-- Find the lowest sum for a set of five primes for which any two primes
-- concatenate to produce another prime.

problem60 :: [[Int]]
problem60 = iterateConcatablePrimes (take 100000 concatablePrimes) 3

-- >>> isPrimePairSet [3,7,109,673]
-- True
isPrimePairSet :: [Int] -> Bool
isPrimePairSet = all pairIsPrime . combinations 2 where
  pairIsPrime [x,y] = isPrime (concatDigits x y) && isPrime (concatDigits y x)
  concatDigits x y = fromDigits (digits x ++ digits y)

--
-- Infinite list of pairs of primes (a,b) that satisfy the properties:
--   c = digits of a and b concatenated --> c is prime
--   d = digits of b and a concatenated --> d is prime
--
-- Duplicates (due to the commutative property) are filtered out. Only the
-- first occurence is outputted, e.g. (3,7) not (7,3).
--
-- >>> take 5 concatablePrimes
-- [(3,7),(3,11),(3,17),(3,31),(3,73)]
concatablePrimes :: [(Int, Int)]
concatablePrimes = concatMap decompose primes where
  decompose = fmap joinDigits . decomposePairs
  decomposePairs = filter pairIsPrime . validSplits . digits
  pairIsPrime (x,y) =
    fromDigits x < fromDigits y && -- prevent duplicates
    isPrime (fromDigits x) && isPrime (fromDigits y) &&
    isPrime (fromDigits (y ++ x)) -- x ++ y is implicitly prime
  splits xs = flip splitAt xs <$> [1..length xs - 1]
  zeroPrefixed (0:xs) = True
  zeroPrefixed _      = False
  pairZeroPrefixed (x,y) = zeroPrefixed x || zeroPrefixed y
  validSplits = filter (not . pairZeroPrefixed) . splits
  joinDigits (x,y) = (fromDigits x, fromDigits y)

combineConcatablePrimes :: [(Int, Int)] -> [[Int]] -> [[Int]]
combineConcatablePrimes cPrimes = concatMap expand where
  expand xs = (: xs) <$> candidates xs
  candidates xs = map head
    $ filter ((==) (length xs) . length)
    $ group $ sort $ concatMap (mapOther xs) cPrimes
  mapOther xs (a,b)
    | a `elem` xs && b `notElem` xs = [b]
    | b `elem` xs && b `notElem` xs = [a]
    | otherwise = []

iterateConcatablePrimes :: [(Int, Int)] -> Int -> [[Int]]
iterateConcatablePrimes cPrimes = go (untupled <$> cPrimes) where
  go xs 0   = xs
  go xs len = go (combineConcatablePrimes cPrimes xs) (len - 1)
  untupled (a,b) = [a,b]
