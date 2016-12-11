module Problem26 where

import           Data.Function (on)
import           Data.List     (find, maximumBy)

--
-- Problem 26: Reciprocal cycles
--
-- A unit fraction contains 1 in the numerator. The decimal representation of
-- the unit fractions with denominators 2 to 10 are given:
--
-- 1/2	= 	0.5
-- 1/3	= 	0.(3)
-- 1/4	= 	0.25
-- 1/5	= 	0.2
-- 1/6	= 	0.1(6)
-- 1/7	= 	0.(142857)
-- 1/8	= 	0.125
-- 1/9	= 	0.(1)
-- 1/10	= 	0.1
--
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
-- seen that 1/7 has a 6-digit recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle
-- in its decimal fraction part.

problem26 :: Int
problem26 = snd $ maximumBy (compare `on` fst) withIndices where
  domain = [2..999]
  recLength = length . snd . reciprocal
  withIndices = map recLength domain `zip` domain

reciprocal :: Int -> ([Int], [Int])
reciprocal x = reciprocal' [] 1 where
  reciprocal' ds y =
    -- Check if we encountered 'y' before in 'ds'
    -- This means we have found a recurring cycle
    case find ((==) y . fst . fst) $ ds `zip` [1..] of
      Just (_,i) -> splitRecurring ds i
      Nothing ->
        case (y * 10) `divMod` x of
          (d, 0) -> splitRecurring ((y,d):ds) 0
          (d, r) -> reciprocal' ((y,d):ds) r

  splitRecurring ds i = (reverse b, reverse a) where
    (a, b) = splitAt i $ map snd ds
