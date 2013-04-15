import Control.Arrow

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving (Show, Enum, Bounded)

data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday
         deriving (Show, Enum, Bounded)

monthDays January   _     = 31
monthDays February  True  = 29
monthDays February  False = 28
monthDays March     _     = 31
monthDays April     _     = 30
monthDays May       _     = 31
monthDays June      _     = 30
monthDays July      _     = 31
monthDays August    _     = 31
monthDays September _     = 30
monthDays October   _     = 31
monthDays November  _     = 30
monthDays December  _     = 31

main = print $ length $ filter valid dayItems

valid (Sunday, 1) = True
valid _           = False

dayItems = zip (cycle days) monthItems

days = enumFrom minBound

monthItems = concat [ take (monthDays m l) [1..] | l <- leapYears, m <- months ]

leapYears = map isLeapYear years

months = enumFrom minBound

years = [1900..2000]

a `divides` b = b `mod` a == 0

isLeapYear n | 400 `divides` n = True
             | 100 `divides` n = False
             | 4   `divides` n = True
             | otherwise       = False
