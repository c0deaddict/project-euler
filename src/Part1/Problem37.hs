module Problem37 where

import           Problem27 (isPrime)
import           Problem32 (digits, fromDigits)

--
-- Problem 37: Truncatable primes
--
-- The number 3797 has an interesting property. Being prime itself, it is
-- possible to continuously remove digits from left to right, and remain prime
-- at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
-- left: 3797, 379, 37, and 3.
--
-- Find the sum of the only eleven primes that are both truncatable from left to
-- right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

-- problem37 :: [Int]
-- problem37 =
--   [ p
--   | p <- [2..999999]
--   , isTruncatablePrime p
--   ]

problem37 :: Int
problem37 = sum $ take 11 $ filter isRtlTruncatablePrime truncatablePrimes

truncatablePrimes :: [Int]
truncatablePrimes = map fromDigits . recurse $ expand singleDigits where
  singleDigits = map (:[]) [2,3,5,7]
  -- Expand while new numbers are found
  recurse acc = acc ++ case expand acc of
    []   -> []
    acc' -> recurse acc'

  expand acc =
    [ ds
    | x <- acc
    , d <- [1..9]
    , let ds = d:x
    , let n = fromDigits ds
    , isPrime n
    ]

-- |
-- Test is p is a right to left (rtl) truncatable prime
--
-- >>> isRtlTruncatablePrime 3979
-- True
-- >>> isRtlTruncatablePrime 7
-- False
isRtlTruncatablePrime :: Int -> Bool
isRtlTruncatablePrime p
  | p < 10 = False
  | otherwise = go p where
    go p = isPrime p && (p < 10 || go (p `div` 10))

isLtrTruncatablePrime :: Int -> Bool
isLtrTruncatablePrime p
  | p < 10 = False
  | otherwise = go (digits p) where
    go [x]       = isPrime x
    go xs@(y:ys) = isPrime (fromDigits xs) && go ys
