library(tidyverse)
library(rvest)

getScore <- function(year, week, teamID){
  
  # 2010 week 1
  # http://games.espn.go.com/ffl/boxscorequick?leagueId=341583&teamId=1&scoringPeriodId=1&seasonId=2010&view=scoringperiod&version=quick
  
  url1 <- 'http://games.espn.com/ffl/boxscorequick?leagueId=341583&teamId='
  url2 <- '&scoringPeriodId='
  url3 <- '&seasonId='
  url4 <- '&view=scoringperiod&version=quick'
  
  box <- paste0(url1, teamID, url2, week, url3, year, url4)
  
  if(year == 2016) {
    score <- read_html(box) %>% 
      html_node("div:nth-child(8) .totalScore") %>% 
      html_text() %>% 
      as.numeric()
  } else if (year == 2017) {
    score <- read_html(box) %>% 
      html_node("div:nth-child(6) .totalScore") %>% 
      html_text() %>% 
      as.numeric()    
  } else {
    score <- read_html(box) %>% 
      html_node("#playertable_0 .playerTableBgRowTotals .appliedPoints") %>% 
      html_text() %>% 
      as.numeric()
  }

  return(score)

}

y <- rep(c(2010:2017), each = 104)
w <- rep(1:13, each = 8, times = 8)
t <- rep(1:8, times = 104)
dat <- data.frame(y, w, t)

# scores_raw <- dat %>%
#   rowwise() %>%
#   mutate(pts = getScore(y, w, t))
# 
# write.csv(scores_raw, "data/scores_raw.csv", row.names = FALSE)
scores_raw <- read.csv("data/scores_raw.csv", stringsAsFactors = FALSE)

# join teams and owners and clean up ####################################
teams_and_owners <- read.csv("data/ddffl_teamsandowners.csv")

scores_clean <- scores_raw %>% 
  left_join(teams_and_owners, by = c("y" = "year", "t" = "teamID")) %>% 
  rename(year = y, week = w, teamID = t)

# prep results ###############################
weekly_matchups <- read.csv("data/ddffl_weeklymatchups.csv")

results <- scores_clean %>% 
  left_join(weekly_matchups, by = c("year", "week", "teamID")) %>% 
  left_join(teams_and_owners, by = c("year", "opp_teamID" = "teamID"))

results <- results %>% 
  left_join(results[, c("year", "week", "teamID", "pts")], by = c("year", "week", "opp_teamID" = "teamID")) %>% 
  select(year, week, owner = owner.x, week_total = pts.x, opp_owner = owner.y, opp_week_total = pts.y)

# add column for won_matchup
results <- results %>% 
  mutate(won_matchup = ifelse(week_total > opp_week_total, 1, ifelse(week_total == opp_week_total, .5, 0)))

# manually fix ties
results[which(results$year == 2014 & results$week == 10 & results$owner == "Nathan S"), "won_matchup"] <- 1
results[which(results$year == 2014 & results$week == 10 & results$owner == "Craig B"), "won_matchup"] <- 0

results[which(results$year == 2015 & results$week == 8 & results$owner == "John L"), "won_matchup"] <- 1
results[which(results$year == 2015 & results$week == 8 & results$owner == "Andrew F"), "won_matchup"] <- 0

results[which(results$year == 2015 & results$week == 10 & results$owner == "John N"), "won_matchup"] <- 1
results[which(results$year == 2015 & results$week == 10 & results$owner == "Steve F"), "won_matchup"] <- 0

results[which(results$year == 2016 & results$week == 10 & results$owner == "Jeff M"), "won_matchup"] <- 1
results[which(results$year == 2016 & results$week == 10 & results$owner == "John L"), "won_matchup"] <- 0

# add elo ratings #########################

# is new season
results_w_elo <- results %>% 
  mutate(new_season = ifelse(year != 2010 & week == 1, 1, 0)) %>%
  arrange(year, week)

# is copy
results_w_elo.sort <- t(apply(results_w_elo[,c("year", "week", "owner", "opp_owner")], 1, sort))
results_w_elo$is_copy <- ifelse(duplicated(results_w_elo.sort) == TRUE, 1, 0)

# is new owner
# http://stackoverflow.com/questions/30159041/creating-a-running-counting-variable-in-r
results_w_elo$owner_n <- sapply(1:nrow(results_w_elo), function(i) sum(results_w_elo$owner[i] == unlist(results_w_elo[1:i, c("owner")])))

results_w_elo <- results_w_elo %>%
  arrange(year, week) %>%
  # new team if owner has only played on game (owner_n = 1) and it's the start of a new season
  mutate(new_owner = ifelse((owner_n == 1 & new_season == 1) | (owner_n == 1 & year == 2010), 1, 0))

# add "period" to count consecutive weeks across full history
results_w_elo$period <- rep(1:((length(unique(results_w_elo$year)))*13), each = 8)

# remove rows where both scores are 0 (games haven't been played yet)
results_w_elo <- results_w_elo %>% filter(week_total != 0 & opp_week_total != 0)

# add margin of victory (mov)
results_w_elo <- results_w_elo %>% 
  mutate(mov = week_total - opp_week_total)

init <- 1500
k <- 10
revert <- 1/4 # between seasons, a team retains 3/4 of their rating

# initialize 3D array to track elo_i and elo_n rating by owner and period
elohist_i <- array(init, dim = c(length(sort(unique(c(results_w_elo$owner, results_w_elo$opp_owner)))), max(results_w_elo$period) + 1), dimnames = list(sort(unique(results_w_elo$owner)), seq(1:(max(results_w_elo$period) + 1))))
elohist_n <- array(NA, dim = c(length(sort(unique(c(results_w_elo$owner, results_w_elo$opp_owner)))), length(unique(results$year))*13), dimnames = list(sort(unique(results_w_elo$owner)), seq(1:(length(unique(results$year))*13))))

results_w_elo$elo_i <- NA
results_w_elo$elo_n <- NA
results_w_elo$opp_elo_i <- NA
results_w_elo$elo_diff <- NA
results_w_elo$MOVM <- NA
results_w_elo$exp <- NA

for (i in 1:nrow(results_w_elo)) {
  if (results_w_elo$new_owner[i] == 1) {
    results_w_elo$elo_i[i] <- init
  } else {
    results_w_elo$elo_i[i] <- elohist_i[results_w_elo$owner[i], results_w_elo$period[i]]
  }
  
  if (results_w_elo$new_season[i] == 1) {
    results_w_elo$elo_i[i] <- init*revert + results_w_elo$elo_i[i]*(1-revert)
  }
  
  results_w_elo$opp_elo_i[i] <- elohist_i[results_w_elo$opp_owner[i], results_w_elo$period[i]]
  
  results_w_elo$elo_diff[i] <- results_w_elo$elo_i[i] - results_w_elo$opp_elo_i[i]
  print(paste0("Owner elo_i: ", results_w_elo$elo_i[i], " opp_owner elo_i: ", elohist_i[results_w_elo$opp_owner[i], results_w_elo$period[i]], " elo_diff: ", results_w_elo$elo_diff[i]))
  
  results_w_elo$exp[i] <- 1 - 1/((10^(results_w_elo$elo_diff[i]/400)) + 1)
  
  # calculate MOVM
  if (results_w_elo$won_matchup[i] == 1) {
    results_w_elo$MOVM[i] <- log(abs(results_w_elo$mov[i]) + 1)*(2.2/((results_w_elo$elo_i[i] - results_w_elo$opp_elo_i[i]) * .001 + 2.2))
  } else {
    results_w_elo$MOVM[i] <- log(abs(results_w_elo$mov[i]) + 1)*(2.2/((results_w_elo$opp_elo_i[i] - results_w_elo$elo_i[i]) * .001 + 2.2))
  }
  
  # calculate new rating (elo_n)
  results_w_elo$elo_n[i] <- results_w_elo$elo_i[i] + results_w_elo$MOVM[i]*k*(results_w_elo$won_matchup[i]-results_w_elo$exp[i])
  
  # update recordkeeping
  elohist_n[results_w_elo$owner[i], results_w_elo$period[i]] <- results_w_elo$elo_n[i]
  elohist_i[results_w_elo$owner[i], results_w_elo$period[i] + 1] <- results_w_elo$elo_n[i]
}

# write.csv(results_w_elo, "data/results_w_elo.csv", row.names = FALSE)

## export for calibration check
# results_w_elo %>% 
#   filter(is_copy == 0) %>% 
#   write.csv(paste0("data/results_w_elo_", k, "_", revert, ".csv"), row.names = FALSE)

results_w_elo %>% 
  ggplot(aes(x = period, y = elo_n, color = owner)) + 
  facet_wrap(~owner) + 
  geom_line(size = 1) + 
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none") + 
  scale_x_continuous(breaks = c(0, 13, 26, 39, 52, 65, 78, 91), 
                     label = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")) + 
  labs(x = NULL, y = NULL, title = "DDFFL Elo Rating History")

ggsave("graphs/elo_history.png")


previous_week <- results_w_elo %>% 
  arrange(desc(year), desc(week), desc(elo_n)) %>% 
  slice(9:16) %>% 
  select(owner, prior_rating = elo_n)

current_ratings <- results_w_elo %>% 
  arrange(desc(year), desc(week), desc(elo_n)) %>% 
  head(8) %>% 
  select(year, week, owner, rating = elo_n) %>% 
  left_join(previous_week, by = c("owner")) %>% 
  mutate(one_week_change = round(rating - prior_rating)) %>% 
  select(owner, rating, one_week_change) %>% 
  mutate(rating = round(rating, 0))

current_ratings
