module Part1.Problem23 where

import           Data.List      (find, group)
import           Data.Maybe     (isJust)
import qualified Data.Set       as Set

import           Part1.Problem3 (trialDivision)

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype Small = Small Int deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 1000) <$> arbitrary

--
-- Problem 23: Non-abundant sums
--
-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors of
-- 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
--
-- A number n is called deficient if the sum of its proper divisors is less than
-- n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
-- number that can be written as the sum of two abundant numbers is 24. By
-- mathematical analysis, it can be shown that all integers greater than 28123
-- can be written as the sum of two abundant numbers. However, this upper limit
-- cannot be reduced any further by analysis even though it is known that the
-- greatest number that cannot be expressed as the sum of two abundant numbers
-- is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the sum
-- of two abundant numbers.

problem23Sets :: Int -> [Int]
problem23Sets upperLimit = filter (not . isAbundantSum) [1..upperLimit] where
  isAbundantSum x = twiceAbundant x || not (Set.null (abundantRemainders x))
  twiceAbundant x = x `mod` 2 == 0 && isAbundant' (x `div` 2)
  remainders x = Set.fromList $ (x -) <$> abundants (x - 1)
  abundantRemainders x = remainders x `Set.intersection` abundantsSet
  isAbundant' x = x `Set.member` abundantsSet
  abundants lim = takeWhile (<= lim) abundantNumbers
  abundantsSet = Set.fromList (abundants upperLimit)

-- Without sets
problem23 :: Int -> [Int]
problem23 upperLimit = filter (not . isAbundantSum) [1..upperLimit] where
  isAbundantSum x = twiceAbundant x || any isAbundant' (remainders x)
  twiceAbundant x = x `mod` 2 == 0 && isAbundant' (x `div` 2)
  isAbundant' x = isJust $ find (== x) $ abundants x
  remainders x = (x -) <$> abundants (x - 1)
  abundants lim = takeWhile (<= lim) abundantNumbers


-- |
-- Generate the infinite list of abundant numbers.
--
-- Make use of the property that a multiple of a abundant property,
-- is itself a abundant number.
--
-- Begin explicitly with the first abundant number 12. This is neccessary to
-- prevent infinite recursion.
--
-- >>> take 4 abundantNumbers
-- [12,18,20,24]
abundantNumbers :: [Int]
abundantNumbers = 12 : filter isAbundant [13..] where
  isAbundant' x = case find (isDivisible x) $ selfLT (x `div` 2) of
    Just _  -> True
    Nothing -> isAbundant x
  isDivisible x y = x `mod` y == 0
  selfLT x = takeWhile (< x) abundantNumbers

-- |
-- Use the sigma function to calculate the altquot sum:
-- https://en.wikipedia.org/wiki/Divisor_function#Properties
--
-- >>> fastSumOfDivisors 28122
-- 29958
--
-- prop> \(Small n) -> n > 0 ==> fastSumOfDivisors n == sumOfDivisors n
fastSumOfDivisors :: Int -> Int
fastSumOfDivisors x = sigma1 - x where
  factors = group $ trialDivision x
  sigma1 = product $ (+1) . sum . raise <$> factors
  raise fs = zipWith (^) fs [1..]

-- |
-- >>> divisors 12
-- [1,2,3,4,6]
divisors :: Int -> [Int]
divisors n = [ x | x <- [1..(n-1)], n `mod` x == 0]

-- |
-- >>> sumOfDivisors 12
-- 16
sumOfDivisors :: Int -> Int
sumOfDivisors = sum . divisors

-- |
-- >>> isAbundant 24
-- True
-- >>> isAbundant 13
-- False
isAbundant :: Int -> Bool
isAbundant n = n < fastSumOfDivisors n
