module Part2.Problem52 where

import           Part1.Problem32 (digits)

--
-- Problem 52: Permuted multiples
--
-- It can be seen that the number, 125874, and its double, 251748, contain
-- exactly the same digits, but in a different order.
--
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.

problem52 :: Int
problem52 = head
  [ x
  | x <- [1..]
  , sameDigits $ map (x *) [2..6]
  ]

sameDigits :: [Int] -> Bool
sameDigits [] = False
sameDigits (x:xs) = all (all (`elem` dset)) others where
  dset = digits x
  others = map digits xs
