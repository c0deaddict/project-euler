module Problem39 where

import           Data.Function (on)
import           Data.List     (maximumBy)

--
-- Problem 39: Integer right triangles
--
-- If p is the perimeter of a right angle triangle with integral length sides,
-- {a,b,c}, there are exactly three solutions for p = 120.
--
-- {20,48,52}, {24,45,51}, {30,40,50}
--
-- For which value of p ≤ 1000, is the number of solutions maximised?

problem39 :: Int
problem39 = fst $ maximumBy (compare `on` snd) domain where
  domain = [(p, length $ rightTriangles p) | p <- [1..1000]]

-- |
-- All tuples (a, b, c) that form a integer right triangle (a^2 + b^2 = c^2)
-- and for which a + b + c = p
--
-- >>> rightTriangles 120
-- [(30,40,50),(24,45,51),(20,48,52)]
rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles p =
  [ (a, b, c)
  | b <- [1..2 * (p `div` 3)] -- c > a, c > b, thus a + b < 2/3 p
  , a <- [1..b - 1] -- b > a
  , let c = p - a - b
  , a^2 + b^2 == c^2
  ]
