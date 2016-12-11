module Problem40 where

--
-- Problem 40: Champernowne's constant
--
-- An irrational decimal fraction is created by concatenating the positive
-- integers:
--
-- 0.123456789101112131415161718192021...
--
-- It can be seen that the 12th digit of the fractional part is 1.
--
-- If dn represents the nth digit of the fractional part, find the value of the
-- following expression.
--
-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

problem40 :: Int
problem40 = product $ map (champernowneDigit . (10^)) [0..6]

champernowneDigit :: Int -> Int
champernowneDigit n = go 1 (n-1) where
  -- 9, 90*2, 900*3, 9000*4, ...
  base p = p * (10^p - 10^(p-1))
  go p n | n >= base p = go (p+1) (n - base p)
         | otherwise = digit d i where
           d = 10^(p-1) + (n `div` p)
           i = p - (n `mod` p) -- i = position of digit rtl: [1..p]
  -- digit 12 1 == 2, digit 12 2 == 1
  digit n 1 = n `mod` 10
  digit n i = digit (n `div` 10) (i-1)
