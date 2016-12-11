module Problem31 where

import           Data.List (sortBy)

--
-- Problem 31: Coin sums
--
-- In England the currency is made up of pound, £, and pence, p, and there are
-- eight coins in general circulation:
--
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
--
-- It is possible to make £2 in the following way:
-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
--
-- How many different ways can £2 be made using any number of coins?

problem31 :: Int
problem31 = countChange coins 200

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

countChange :: [Int] -> Int -> Int
countChange _ 0 = 0
countChange initCoins initMoney = countChange' coinsDesc initMoney where
  coinsDesc = sortDesc initCoins
  sortDesc = sortBy (flip compare)
  countChange' _ 0 = 1
  countChange' [] _ = 0
  countChange' coins@(c:cs) money =
    -- Count variations: recurse without the biggest coin
    countChange' cs money +
    -- Optimal path: subtract biggest coin and recurse
    if money >= c then countChange' coins (money - c) else 0
