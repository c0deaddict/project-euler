module Part1.Problem28 where

--
-- Problem 28: Number spiral diagonals
--
-- Starting with the number 1 and moving to the right in a clockwise direction
-- a 5 by 5 spiral is formed as follows:
--
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
--
-- It can be verified that the sum of the numbers on the diagonals is 101.
--
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
-- formed in the same way?

problem28 :: Int
problem28 = 1 + sum [d 2, d 4, d 6, d 8] where
  n = 1001 `div` 2
  d off = sum $ take n $ diagonalInf off
--
-- diagonal :: Int -> Int -> [Int]
-- diagonal n = diagonal' n 1 1 where
--   diagonal' n i prev acc
--     | i <= n = let
--         cur = prev + acc
--         acc' = acc + 8
--       in cur : diagonal' n (i + 1) cur acc'
--     | otherwise = []
--     -- | i < n = 8*(acc + i) - i*o : diagonal' n o (i + 1) (acc + i)

diagonalInf :: Int -> [Int]
diagonalInf = go 1 1 where
  go i prev acc =
    let cur = prev + acc
    in cur : go (i+1) cur (acc+8)
