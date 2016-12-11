module Problem19 where

--
-- Problem 19: Counting Sundays
--
-- You are given the following information, but you may prefer to do some
-- research for yourself.
--
-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century
-- unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century
-- (1 Jan 1901 to 31 Dec 2000)?

problem19 :: Int
problem19 = sum $ map (boolToInt . isFirstSunday) weekDays where
  givenMonday = Date 1900 1 1
  startDate = Date 1901 1 1
  endDate = Date 2000 12 31
  fromStart = dropWhile (\x -> fst x < startDate) $ enumWeekDays (givenMonday, 0)
  weekDays = takeWhile (\x -> fst x <= endDate) fromStart
  isFirstSunday (date, weekDay) = weekDay == 6 && dayOfMonth date == 1
  boolToInt True  = 1
  boolToInt False = 0


data Date = Date
  { year       :: Int
  , month      :: Int
  , dayOfMonth :: Int
  } deriving Show

instance Eq Date where
  (Date y1 m1 d1) == (Date y2 m2 d2) =
    y1 == y2 && m1 == m2 && d1 == d2

instance Ord Date where
  (Date y1 m1 d1) <= (Date y2 m2 d2)
    | y1 < y2 = True
    | y1 > y2 = False
    | m1 < m2 = True
    | m1 > m2 = False
    | d1 < d2 = True
    | d1 > d2 = False
    | otherwise = True

-- |
-- Test if a year is a leap year
--
-- >>> isLeapYear 2015
-- False
-- >>> isLeapYear 2016
-- True
-- >>> isLeapYear 1900
-- False
-- >>> isLeapYear 2000
-- True
isLeapYear :: Int -> Bool
isLeapYear year = year `mod` 4 == 0 &&
  (year `mod` 100 /= 0 || year `mod` 400 == 0)

-- |
-- Number of days in a month
--
-- >>> daysInMonth 2016 1
-- 31
-- >>> daysInMonth 2016 2
-- 29
-- >>> daysInMonth 2017 2
-- 28
daysInMonth :: Int -> Int -> Int
daysInMonth _ 1    = 31
daysInMonth _ 3    = 31
daysInMonth _ 4    = 30
daysInMonth _ 5    = 31
daysInMonth _ 6    = 30
daysInMonth _ 7    = 31
daysInMonth _ 8    = 31
daysInMonth _ 9    = 30
daysInMonth _ 10   = 31
daysInMonth _ 11   = 30
daysInMonth _ 12   = 31
daysInMonth year 2 = if isLeapYear year then 29 else 28


nextDay :: Date -> Date
nextDay date = let nextDayOfMonth = dayOfMonth date + 1 in
  if nextDayOfMonth > daysInMonth (year date) (month date) then
    if month date + 1 > 12 then
      Date (year date + 1) 1 1
    else
      Date (year date) (month date + 1) 1
  else
    Date (year date) (month date) nextDayOfMonth


enumDays :: Date -> [Date]
enumDays date = date : enumDays (nextDay date)

enumWeekDays :: (Date, Int) -> [(Date, Int)]
enumWeekDays (date, weekDay) =
  (date, weekDay) : enumWeekDays (nextDay date, nextWeekDay) where
    nextWeekDay = (weekDay + 1) `mod` 7
