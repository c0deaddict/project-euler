module Problem30 where

import           Data.Char (digitToInt)

--
-- Problem 30: Digit fifth powers
--
-- Surprisingly there are only three numbers that can be written as the sum of
-- fourth powers of their digits:
--
-- 1634 = 1^4 + 6^4 + 3^4 + 4^4
-- 8208 = 8^4 + 2^4 + 0^4 + 8^4
-- 9474 = 9^4 + 4^4 + 7^4 + 4^4
--
-- As 1 = 14 is not a sum it is not included.
--
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.

-- TODO: i don't understand the upper bound of 1000000
-- the numbers found are:
-- [4150,4151,54748,92727,93084,194979]
--
-- Maximum digit is: 9
-- That becomes 9^5
-- 6*9^5 = 354294 (in range of a six digit number)
-- 7*9^5 = 413343 (not in range of a seven digit number)
problem30 :: Int
problem30 = sum $ filter isFifthPower [2..1000000]

digitsFifthPower :: Int -> Int
digitsFifthPower = sum . map ((^5) . digitToInt) . show

isFifthPower :: Int -> Bool
isFifthPower x = digitsFifthPower x == x
