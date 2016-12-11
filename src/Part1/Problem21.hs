module Problem21 where

import           Data.List (group)
import           Problem3  (trialDivision)

--
-- Problem 21: Amicable numbers
--
-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n
-- which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
-- each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
-- 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
-- 71 and 142; so d(284) = 220.
--
-- Evaluate the sum of all the amicable numbers under 10000.

problem21 :: Int
problem21 = sum $ filter isAmicable [2..9999]

isAmicable :: Int -> Bool
isAmicable n = da /= db && db == n where
  da = sumOfDivisors n
  db = sumOfDivisors da

sumOfDivisors :: Int -> Int
sumOfDivisors = sum . divisors

divisors :: Int -> [Int]
divisors x = filter (/= x) $ combis 1 factors where
  factors = group $ trialDivision x

combis :: Int -> [[Int]] -> [Int]
combis acc []          = [acc]
combis acc ([]:ys)     = combis acc ys
combis acc ((x:xs):ys) = combis (acc * product (x:xs)) ys ++ combis acc (xs:ys)
