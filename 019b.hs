import Data.List

answer :: Integer
answer = fromIntegral $ length valid_days

valid_days :: [Day]
valid_days = [
  day | day <- twentieth_centuary,
    is_sunday day,
    is_first_day_of_month day
  ]

days :: [Day]
days = iterate next_day (1900, January, 1, Monday)

twentieth_centuary :: [Day]
twentieth_centuary = takeWhile before_twentieth_end $ dropWhile before_twentieth_start days
  where
    before_twentieth_start = ( < twentieth_centuary_start) . weekdayless
    before_twentieth_end   = ( <= twentieth_centuary_end) . weekdayless

twentieth_centuary_start = (1901, January, 1) -- 1 Jan 1901 to 31 Dec 2000
twentieth_centuary_end = (2000, December, 31)

-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
is_leap_year :: Year -> Bool
is_leap_year year
  | 400 `divides` year = True
  | 100 `divides` year = False
  | 4   `divides` year = True
  | otherwise          = False

divides :: Integer -> Integer -> Bool
denominator `divides` numerator = numerator `mod` denominator == 0

type Weekdayless = (Year, Month, Day_of_month)

weekdayless :: Day -> Weekdayless
weekdayless (year, month, day, _) = (year, month, day)

next_day :: Day -> Day

next_day (year, Febuary, 28, week_day)
  | is_leap_year year = (year, Febuary, 29, next_week_day week_day)
  | otherwise         = (year, March,   01, next_week_day week_day)

next_day (year, December, day, week_day)
  | day == month_days (is_leap_year year) December = (year + 1, January,  01,      next_week_day week_day)
  | otherwise                                      = (year,     December, day + 1, next_week_day week_day)

next_day (year, month, day, week_day)
  | day == month_days (is_leap_year year) month = (year, next_month month, 01,      next_week_day week_day)
  | otherwise                                   = (year, month,            day + 1, next_week_day week_day)

type Day = (Year, Month, Day_of_month, Weekday_name)

type Year = Integer

data Month = January | Febuary | March | April | May | June | July | August | September | October | November | December
  deriving (Eq, Ord, Show)

next_month :: Month -> Month
next_month January   = Febuary
next_month Febuary   = March
next_month March     = April
next_month April     = May
next_month May       = June
next_month June      = July
next_month July      = August
next_month August    = September
next_month September = October
next_month October   = November
next_month November  = December
next_month December  = January

is_first_day_of_month (_, _, 1, _) = True
is_first_day_of_month _            = False

type Day_of_month = Integer

data Weekday_name = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Show)

next_week_day :: Weekday_name -> Weekday_name
next_week_day Monday    = Tuesday
next_week_day Tuesday   = Wednesday
next_week_day Wednesday = Thursday
next_week_day Thursday  = Friday
next_week_day Friday    = Saturday
next_week_day Saturday  = Sunday
next_week_day Sunday    = Monday

is_sunday :: Day -> Bool
is_sunday (_, _, _, Sunday) = True
is_sunday _                 = False

month_days :: LeapYear -> Month -> Days
month_days _     January   = 31
month_days True  Febuary   = 29
month_days False Febuary   = 28
month_days _     March     = 31
month_days _     April     = 30
month_days _     May       = 31
month_days _     June      = 30
month_days _     July      = 31
month_days _     August    = 31
month_days _     September = 31
month_days _     October   = 31
month_days _     November  = 30
month_days _     December  = 31

type Days = Integer
type LeapYear = Bool
