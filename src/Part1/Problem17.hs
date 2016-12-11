module Problem17 where

--
-- Problem 17: Number letter counts
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
--
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
-- letters. The use of "and" when writing out numbers is in compliance with
-- British usage.

problem17 :: Int
problem17 = sum $ map (lettersLength . numberLetters) [1..1000]

lettersLength :: String -> Int
lettersLength = length . filter (`notElem` " -")

numberLetters :: Int -> String
numberLetters 0  = "zero"
numberLetters 1  = "one"
numberLetters 2  = "two"
numberLetters 3  = "three"
numberLetters 4  = "four"
numberLetters 5  = "five"
numberLetters 6  = "six"
numberLetters 7  = "seven"
numberLetters 8  = "eight"
numberLetters 9  = "nine"
numberLetters 10 = "ten"
numberLetters 11 = "eleven"
numberLetters 12 = "twelve"
numberLetters 13 = "thirteen"
numberLetters 14 = "fourteen"
numberLetters 15 = "fifteen"
numberLetters 16 = "sixteen"
numberLetters 17 = "seventeen"
numberLetters 18 = "eighteen"
numberLetters 19 = "nineteen"
numberLetters 20 = "twenty"
numberLetters 30 = "thirty"
numberLetters 40 = "forty"
numberLetters 50 = "fifty"
numberLetters 60 = "sixty"
numberLetters 70 = "seventy"
numberLetters 80 = "eighty"
numberLetters 90 = "ninety"
numberLetters n | n < 100 = base ++ "-" ++ digit where
  (b, d) = n `divMod` 10
  base = numberLetters (b * 10)
  digit = numberLetters d
numberLetters n | n < 1000 = base ++ " hundred" ++ rest where
  (b, r) = n `divMod` 100
  base = numberLetters b
  rest = if r > 0 then " and " ++ numberLetters r else ""
numberLetters n | n < 1000000 = base ++ " thousand" ++ rest where
  (b, r) = n `divMod` 1000
  base = numberLetters b
  rest = if r > 0 then " and " ++ numberLetters r else ""
