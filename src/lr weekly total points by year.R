library(tidyverse)
library(modelr)

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

# remove rows where both scores are 0 (games haven't been played yet)
results <- results %>% filter(week_total != 0 & opp_week_total != 0)

######################

df <- results %>% select(year, week_total, won_matchup)

# nest by year
by_year <- df %>% 
  group_by(year) %>% 
  nest()

# define model
yr_model <- function(tab) {
  glm(won_matchup ~ week_total, data = tab, family = "binomial")
}

# fit a model and make predictions for each year
by_year <- by_year %>% 
  mutate(model = map(data, yr_model),
         pred = map2(model, data, ~ predict.glm(.x, type = "response")))

# plot
by_year %>% 
  unnest(data, pred) %>% 
  ggplot() + 
  geom_point(aes(x = week_total, y = won_matchup, colour = as.factor(year)), alpha = .2, size = 2) + 
  geom_line(aes(x = week_total, y = pred, colour = as.factor(year)), size = 1.5) +
  theme_minimal() +
  labs(x = "Total Points Scored",
       y = "P(Win Matchup)",
       title = "Probability of Winning Matchup Given Total Points Scored",
       subtitle = "By Season, 2010 - 2017") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent)

