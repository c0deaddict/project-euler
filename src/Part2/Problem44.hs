module Part2.Problem44 where

import           Data.List      (find)
import           Data.Maybe     (mapMaybe)
import           Part1.Problem3 (squareRoot)

--
-- Problem 44: Pentagon numbers
--
-- Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first ten
-- pentagonal numbers are:
--
-- 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
--
-- It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference,
-- 70 − 22 = 48, is not pentagonal.
--
-- Find the pair of pentagonal numbers, Pj and Pk, for which their sum and
-- difference are pentagonal and D = |Pk − Pj| is minimised; what is the value
-- of D?

problem44 :: [(Int, Int)]
problem44 = mapMaybe minPair (take 3000 pentagonals) where
  minPair p1 = (\p2 -> (p1, p2)) <$> findMinPair p1
  findMinPair p1 = find isPair domain where
    domain = reverse $ takeWhile (< p1) pentagonals
    isPair p2 = isPentagonal (p1 - p2) && isPentagonal (p1 + p2)

pentagonals :: [Int]
pentagonals = map pentagonal [1..]

pentagonal :: Int -> Int
pentagonal n = n * (3*n - 1) `div` 2

-- isPentagonal :: Int -> Bool
-- isPentagonal x = x `elem` takeWhile (<= x) pentagonals

isPentagonal :: Int -> Bool
isPentagonal x = x == pentagonal n || x == pentagonal (n + 1) where
  n = squareRoot (x*2 `div` 3)

diffs :: [(Int, Int)]
diffs = filter (isPentagonal . snd)
  $ [1..] `zip` tail (zipWith (-) pentagonals (0:pentagonals))

altProblem44 :: Maybe (Int, Int)
altProblem44 = find minPair diffs where
  minPair (n, _) = let
      p1 = pentagonal n
      p2 = pentagonal (n + 1)
    in isPentagonal $ p1 + p2