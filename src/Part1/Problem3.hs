module Part1.Problem3 where

--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

-- https://stackoverflow.com/a/27464965/248948
squareRoot :: Integral t => t -> t
squareRoot n
   | n > 0    = babylon n
   | n == 0   = 0
   | n < 0    = error "Negative input"
   where
   babylon a   | a > b     = babylon b
               | otherwise = a
      where b  = quot (a + quot n a) 2

primeSieve :: Int -> [Int]
primeSieve n = filter (`notElem` sieve) [2..n] where
  sieve =
    [ i*i + j*i
    | i <- [2..squareRoot n]
    , j <- [0..((n - i*i) `div` i)]
    ]

altPrimeSieve :: Integral t => t -> [t]
altPrimeSieve n = filter isPrime [2..n] where
  isPrime p = all (\x -> p `mod` x /= 0) [2..(squareRoot p)]

ceilSquareRoot :: Int -> Int
ceilSquareRoot n = if s2 < n then s + 1 else s where
  s = squareRoot n
  s2 = s * s

isSquare :: Int -> Bool
isSquare n = s * s == n where s = squareRoot n

fermatFactor :: Int -> Int
fermatFactor n = factor' b2 a where
  a = ceilSquareRoot n
  b2 = a * a - n
  factor' b2 a =
    if not (isSquare b2) then
      let
        a' = a + 1
        b2' = (a' * a') + n
      in factor' b2' a'
    else
      a - squareRoot b2



-- def trial_division(n):
--     """Return a list of the prime factors for a natural number."""
--     if n < 2:
--         return []
--     prime_factors = []
--     for p in prime_sieve(int(n**0.5)):
--         if p*p > n: break
--         while n % p == 0:
--             prime_factors.append(p)
--             n //= p
--     if n > 1:
--         prime_factors.append(n)
--     return prime_factors

trialDivision :: Integral t => t -> [t]
trialDivision n | n < 2 = []
trialDivision n = trialDivision' n primes where
  primes = altPrimeSieve (squareRoot n)
  trialDivision' n []
    | n > 1 = [n]
    | otherwise = []
  trialDivision' n (p:ps)
    | p*p > n = trialDivision' n []
    | otherwise =
      case factor n p of
        (x:xs) -> xs ++ trialDivision' x ps
  factor :: Integral t => t -> t -> [t]
  factor x p =
    if x `mod` p == 0 then
      factor (x `div` p) p ++ [p]
    else
      [x]

largestPrimeFactor :: Int -> Int
largestPrimeFactor p = maximum $ trialDivision p

problem3 :: Int
problem3 = largestPrimeFactor 600851475143
