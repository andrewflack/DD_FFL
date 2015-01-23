# 2010 week 1
# http://games.espn.go.com/ffl/boxscorequick?leagueId=341583&teamId=1&scoringPeriodId=1&seasonId=2010&view=scoringperiod&version=quick

url1 <- 'http://games.espn.go.com/ffl/boxscorequick?leagueId=341583&teamId='
url2 <- '&scoringPeriodId='
url3 <- '&seasonId='

y <- rep(c(2010,2011,2012,2013,2014),each=104)
w <- rep(1:13,each=8,times=5)
t <- rep(1:8,times=65)
dat <- data.frame(y,w,t)

scores.raw <- ldply(mapply(getScoresTable,dat$y,dat$w,dat$t), data.frame)