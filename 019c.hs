main = print solve (1,1,1) (2000,12,31)

solve start end =
  length [ day |
      day <- days,
      day >= start,
      day <= end,
      day 
    ]
