module Problem4 where

import Data.List (maximumBy)

--
-- Problem 4: Largest palindrome product
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

threeDigitPalindromes :: [(Int, Int)]
threeDigitPalindromes =
  [ (a, b)
  | a <- [100..999]
  , b <- [100..999]
  , isPalindrome (show (a * b))
  ]

compareProduct :: (Int, Int) -> (Int, Int) -> Ordering
compareProduct (a, b) (c, d) = compare (a * b) (c * d)

problem4 :: (Int, Int)
problem4 = maximumBy compareProduct threeDigitPalindromes
