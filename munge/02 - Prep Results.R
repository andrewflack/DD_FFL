# prep "Results" by joining with weekly matchups and denoting winner

weekly_matchups <- read_csv("data/ddffl_weeklymatchups.csv")

results <- scores_clean %>% 
  left_join(weekly_matchups, by = c("year", "week", "teamID")) %>% 
  left_join(teams_and_owners, by = c("year", "opp_teamID" = "teamID"))

results <- results %>% 
  left_join(results[, c("year", "week", "teamID", "pts")], by = c("year", "week", "opp_teamID" = "teamID")) %>% 
  select(year, week, owner = owner.x, week_total = pts.x, opp_owner = owner.y, opp_week_total = pts.y)

# add column for won_matchup
results <- results %>% 
  mutate(won_matchup = ifelse(week_total > opp_week_total, 1, ifelse(week_total == opp_week_total, .5, 0)))

# remove rows where both scores are 0 (games haven't been played yet)
results <- results %>% filter(week_total != 0 & opp_week_total != 0)
