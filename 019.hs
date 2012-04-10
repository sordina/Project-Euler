main = print solve

years = [1900..]

months = cycle $ enumFrom January

solve = length [ day | (day,weekday) <- days
                     , weekday == Sunday
                     , day >= (fromDate 1 1 1)
                     , day <= (fromDate 2000 12 31)
                     ]

fromDate :: Integer -> Integer -> Integer -> Integer
fromDate year month day = head [ day | day <- days, day == (year, month, day) ]

is_leap year
  | year `mod` 400 == 0 = False
  | year `mod` 4   == 0 = True
  | otherwise           = False

month_lengths :: Bool -> [(Month,Integer)]
month_lengths leap = [
  (January, 31)
  (Febuary, febuary_length leap)
  (March, 31)
  (April, 30)
  (May, 31)
  (June, 30)
  (July, 31)
  (August, 31)
  (September, 30)
  (October, 31)
  (November, 30)
  (December, 31)
  ]

febuary_length True = 29
febuary_length False = 28

weekdays = repeat $ enumFrom Monday

days :: [(Integer, Day)]
days = undefined

data Day =
    Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Enum, Show, Eq)

data Month =
  January
  | Febuary
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
  deriving (Enum, Show, Eq)

