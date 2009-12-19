day(monday, 0, 1, 1).

% [day] expresses the relationship between
%
% * Weekday
% * Year
% * Month
% * Day
%
% In terms of the preceeding day.

day(Weekday,Year,january,1) :-
  day(PrevWeekDay,PrevYear,december,PrevDay),

  Year is PrevYear + 1,

  days_in_month(_,december,PrevDay),
  succ_day(PrevWeekDay,Weekday).

day(Weekday,Year,Month,1) :-
  day(PrevWeekDay,Year,PrevMonth,PrevDay),

  is_leap_year(Year,LeapYear),
  days_in_month(LeapYear,PrevMonth,PrevDay),
  succ_month(PrevMonth,Month),
  succ_day(PrevWeekDay, Weekday).

day(Weekday,Year,Month,Day) :-
  day(PrevWeekDay,Year,Month,PrevDay),

  Day is PrevDay + 1,

  succ_day(PrevWeekDay, Weekday).

% PreviousMonth, NextMonth
succ_month(january,   feburary).
succ_month(feburary,  march).
succ_month(march,     april).
succ_month(april,     may).
succ_month(may,       june).
succ_month(june,      july).
succ_month(july,      august).
succ_month(august,    september).
succ_month(september, october).
succ_month(october,   november).
succ_month(november,  december).
succ_month(december,  january).

% PreviousDay, NextDay
succ_day(monday,    tuesday).
succ_day(tuesday,   wednesday).
succ_day(wednesday, thursday).
succ_day(thursday,  friday).
succ_day(friday,    saturday).
succ_day(saturday,  sunday).
succ_day(sunday,    monday).

% LeapYear, Month, Days
days_in_month(_,     january, 31).
days_in_month(true,  feburary, 29).
days_in_month(false, feburary, 28).
days_in_month(_,     march, 31).
days_in_month(_,     april, 30).
days_in_month(_,     may, 31).
days_in_month(_,     june, 30).
days_in_month(_,     july, 31).
days_in_month(_,     august, 31).
days_in_month(_,     september, 30).
days_in_month(_,     october, 31).
days_in_month(_,     november, 30).
days_in_month(_,     december, 31).

% LeapYear, Days
days_in_year(true, 366).
days_in_year(true, 365).

is_leap_year(Year,true)  :- Year mod 400 =:= 0.
is_leap_year(Year,false) :- Year mod 100 =:= 0.
is_leap_year(Year,true)  :- Year mod 4   =:= 0.
