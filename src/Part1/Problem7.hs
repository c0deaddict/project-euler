module Problem7 where

--
-- Problem 7: 10001st prime
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6th prime is 13.
--
-- What is the 10 001st prime number?

-- https://stackoverflow.com/a/27464965/248948
squareRoot :: Integral t => t -> t
squareRoot n
   | n > 0    = babylon n
   | n == 0   = 0
   | n < 0    = error "Negative input"
   where
   babylon a   | a > b  = babylon b
               | True   = a
      where b  = quot (a + quot n a) 2

primeSieve :: Int -> [Int]
primeSieve n = filter (`notElem` sieve) [1..n] where
  sieve =
    [ i*i + j*i
    | i <- [2..squareRoot n]
    , j <- [0..((n - i*i) `div` i)]
    ]

problem7 :: Int
problem7 = (primeSieve 100000) !! 10001
