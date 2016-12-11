module Problem34 where

import           Problem32 (digits)

--
-- Problem 34: Digit factorials
--
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

problem34 :: Int
problem34 = sum
  [ d
  | d <- [10..10^maxLen]
  , digitsFacSum d == d
  ] where maxLen = 7 -- 9.999.999 > 7*(fac 9)

digitsFacSum :: Int -> Int
digitsFacSum = sum . map fac . digits

fac :: Int -> Int
fac 0 = 1
fac 1 = 1
fac n = n * fac (n-1)
