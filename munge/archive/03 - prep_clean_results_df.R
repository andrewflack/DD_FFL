# Prep clean results

results <- scores.clean %>%
  select(year, teamID, week, name, owner, FFLpts, pos, player) %>%
  spread(pos, FFLpts) %>%
  select(year, teamID, owner, week, QB, RB, WR, TE, K, DST) %>% 
  group_by(year, teamID, owner, week) %>% 
  
  # sum points per week by position
  summarise_each(funs(sum(., na.rm = TRUE)))  %>% 
  mutate(week_total = K + QB + RB + TE + WR + DST) %>%
  select(year, week, teamID, owner, week_total) %>% 
  
  # add opponent's teamID
  left_join(ddffl.weeklymatchups, by = c("year","week","teamID")) %>%
  
  # add opp_owner based on opp_teamID
  left_join(ddffl.teamsandowners[,c("year","teamID","owner")], by = c("year", "opp_teamID" = "teamID"))

# clean up column names before continuing
colnames(results) <- c("year", "week", "teamID", "owner", "week_total", "opp_teamID", "opp_owner")

# add the opp_week_score and clean up column names again
results <- results %>% 
  left_join(results[,c("year","week","teamID","week_total")], by = c("year", "week", "opp_teamID" = "teamID"))

colnames(results) <- c("year", "week", "teamID", "owner", "week_total", "opp_teamID", "opp_owner", "opp_week_total")

# add column for won_matchup
results <- results %>%
  mutate(won_matchup = ifelse(week_total > opp_week_total, 1, 0))

# drop teamID and opp_teamID
results <- results %>%
  ungroup() %>%
  select(-teamID, -opp_teamID)


  