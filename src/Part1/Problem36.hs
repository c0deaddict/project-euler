module Problem36 where

import           Data.Char (intToDigit)
import           Numeric   (showIntAtBase)

--
-- Problem 36: Double-base palindromes
--
-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in
-- base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)

problem36 :: Int
problem36 = sum
  [ n
  | n <- [1..999999]
  , isPalindrome 10 n
  , isPalindrome 2 n
  ]

isPalindrome :: Int -> Int -> Bool
isPalindrome base n = str == reverse str where
  str = showIntAtBase base intToDigit n ""
