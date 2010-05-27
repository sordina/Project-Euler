days :: Bool -> Integer
days leap = sum
  [
    {- January   -} 31,
    {- February  -} febuaryDays leap,
    {- March     -} 31,
    {- April     -} 30,
    {- May       -} 31,
    {- June      -} 30,
    {- July      -} 31,
    {- August    -} 31,
    {- September -} 30,
    {- October   -} 31,
    {- November  -} 30,
    {- December  -} 31]

febuaryDays :: Bool -> Integer
febuaryDays True = 29
febuaryDays _    = 28
