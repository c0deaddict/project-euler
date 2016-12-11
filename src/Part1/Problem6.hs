module Problem6 where

problem6 :: Int
problem6 = sqrOfSum - sumOfSqrs where
  x = sum [1..100]
  sqrOfSum = x*x
  sumOfSqrs = sum [x*x | x <- [1..100]]
