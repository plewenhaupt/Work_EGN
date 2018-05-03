library(lubridate)

week <- week(Sys.Date())
save(week, file="week.RData")


source('ExRep - Members in_out.R', encoding = 'UTF-8')
source('EGN/ExRep - RYG Groups.R')
source('EGN/ExRep - Retention.R')
source('ExRep - Average members per group.R')
source('Exrep - Renewals.R', encoding = 'UTF-8')
source('ExRep - NPS.R')
