module Problem33 where

import           Data.Ratio
import           Problem32  (digits)

--
-- Problem 33: Digit cancelling fractions
--
-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician
-- in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
-- is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less
-- than one in value, and containing two digits in the numerator and
-- denominator.
--
-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.

problem33 :: Int
problem33 = denominator . product $
  [ x
  | a <- [10..99]
  , b <- [10..99]
  , isCuriousFraction a b
  , let x = a % b
  , x < 1
  ]

isCuriousFraction :: Int -> Int -> Bool
isCuriousFraction a b =
  -- Ignore trivial
  (a /= b) && (
    -- Cancel out a2 with b1 (49/98)
    (a2 == b1 && b2 /= 0 && a1 % b2 == a % b) ||
    -- Cancel out a1 with b2 (64/16)
    (a1 == b2 && a2 % b1 == a % b)
  )
  where
    [a1, a2] = digits a
    [b1, b2] = digits b
