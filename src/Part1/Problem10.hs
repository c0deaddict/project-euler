module Problem10 where

--
-- Problem 10: Summation of primes
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

-- https://stackoverflow.com/a/27464965/248948
squareRoot :: Integral t => t -> t
squareRoot n
   | n > 0    = babylon n
   | n == 0   = 0
   | n < 0    = error "Negative input"
   where
   babylon a   | a > b       = babylon b
               | otherwise   = a
      where b  = quot (a + quot n a) 2

altPrimeSieve :: Int -> [Int]
altPrimeSieve n = filter isPrime [2..n] where
  isPrime p = all (\x -> p `mod` x /= 0) [2..(squareRoot p)]

problem10 :: Int
problem10 = sum $ altPrimeSieve 2000000
