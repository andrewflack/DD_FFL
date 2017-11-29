# # Add playoff results
# 
# # - we need to create matchups week by week since they are not know before the season begins
# # - weekly matchups depend on seeding and the results of weeks 13-15
# 
# # calculate seeds based on week 13 results
# playoff_seeds <- results %>%
#   ungroup() %>%
#   group_by(year, owner) %>%
#   mutate(wins = sum(won_matchup),
#             pts_for = sum(week_total)) %>%
#   filter(week == 1) %>% 
#   select(year, owner, division, wins, pts_for) %>% 
#   arrange(year, desc(wins), desc(pts_for), division) %>%
#   mutate(seed = CountDistinctAlong(pts_for)) %>%
#   select(-wins, -pts_for, -division)
# 
# # convert seeds into week 14 matchups
# playoff_matchups_14 <- playoff_seeds %>%
#   left_join(teams_and_owners, by = c("year", "owner")) %>%
#   select(year, owner, seed, teamID) %>%
#   group_by(year) %>%
#   mutate(opp_seed = rev(seed), bracket_section = ifelse(seed %in% c(1, 8, 4, 5), "top", "bottom")) %>%
#   left_join(playoff_seeds, by = c("opp_seed" = "seed", "year")) %>%
#   select(-seed, -opp_seed) %>%
#   left_join(teams_and_owners, by = c("owner.y" = "owner", "year")) %>%
#   select(year, teamID.x, teamID.y, bracket_section) %>%
#   rename(teamID = teamID.x, opp_teamID = teamID.y) %>%
#   mutate(week = 14)
# 
# results_14 <- scores_clean %>%
#   filter(week == 14) %>%
#   left_join(playoff_matchups_14, by = c("year", "week", "teamID")) %>%
#   left_join(teams_and_owners, by = c("year", "opp_teamID" = "teamID"))
# 
# results_14 <- results_14 %>%
#   left_join(results_14[, c("year", "week", "teamID", "pts")], by = c("year", "week", "opp_teamID" = "teamID")) %>%
#   select(year, week, owner = owner.x, week_total = pts.x, opp_owner = owner.y, opp_week_total = pts.y, bracket_section) %>%
#   mutate(won_matchup = ifelse(week_total > opp_week_total, 1, ifelse(week_total == opp_week_total, .5, 0))) %>%
#   filter(week_total != 0 & opp_week_total != 0)
# 
# # week 15 matchups (based on results of week 14 matchups)
# playoff_matchups_15 <- results_14 %>%
#   select(-week_total, -opp_owner, -opp_week_total) %>%
#   left_join(teams_and_owners, by = c("year", "owner")) %>%
#   left_join(playoff_seeds, by = c("year", "owner")) %>%
#   ungroup() %>%
#   arrange(year, desc(won_matchup), seed) %>%
#   group_by(bracket_section) %>%
#   mutate(bracket_section_15 = ifelse(won_matchup, paste0("winners_", bracket_section), paste0("losers_", bracket_section))) %>%
#   ungroup() %>%
#   group_by(year, bracket_section_15) %>%
#   mutate(opp_seed = rev(seed)) %>%
#   ungroup() %>%
#   select(year, teamID, opp_seed, bracket_section_15) %>%
#   left_join(playoff_seeds, by = c("year", "opp_seed" = "seed")) %>%
#   left_join(teams_and_owners, by = c("year", "owner")) %>%
#   select(-owner, -name, -opp_seed) %>%
#   rename(teamID = teamID.x, opp_teamID = teamID.y) %>%
#   mutate(week = 15)
# 
# # week 15 results
# results_15 <- scores_clean %>%
#   filter(week == 15) %>%
#   left_join(playoff_matchups_15, by = c("year", "week", "teamID")) %>%
#   left_join(teams_and_owners, by = c("year", "opp_teamID" = "teamID"))
# 
# results_15 <- results_15 %>%
#   left_join(results_15[, c("year", "week", "teamID", "pts")], by = c("year", "week", "opp_teamID" = "teamID")) %>%
#   select(year, week, owner = owner.x, week_total = pts.x, opp_owner = owner.y, opp_week_total = pts.y, bracket_section_15) %>%
#   mutate(won_matchup = ifelse(week_total > opp_week_total, 1, ifelse(week_total == opp_week_total, .5, 0))) %>%
#   filter(week_total != 0 & opp_week_total != 0)
# 
# # week 16 matchups (based on results of week 15 matchups)
# playoff_matchups_16 <- results_15 %>%
#   select(-week_total, -opp_owner, -opp_week_total) %>%
#   left_join(teams_and_owners, by = c("year", "owner")) %>%
#   left_join(playoff_seeds, by = c("year", "owner")) %>%
#   ungroup() %>%
#   arrange(year, desc(won_matchup), seed) %>%
#   group_by(bracket_section_15) %>%
#   mutate(bracket_section_16 = ifelse(str_detect(bracket_section_15, "winners"), ifelse(won_matchup, "championship", "consolation"), ifelse(won_matchup, "losers_top", "losers_bottom"))) %>%
#   ungroup() %>%
#   group_by(year, bracket_section_16) %>%
#   mutate(opp_seed = rev(seed)) %>%
#   ungroup() %>%
#   select(year, teamID, opp_seed, bracket_section_16) %>%
#   left_join(playoff_seeds, by = c("year", "opp_seed" = "seed")) %>%
#   left_join(teams_and_owners, by = c("year", "owner")) %>%
#   select(-owner, -name, -opp_seed) %>%
#   rename(teamID = teamID.x, opp_teamID = teamID.y) %>%
#   mutate(week = 16)
# 
# # week 16 results
# results_16 <- scores_clean %>%
#   filter(week == 16) %>%
#   left_join(playoff_matchups_16, by = c("year", "week", "teamID")) %>%
#   left_join(teams_and_owners, by = c("year", "opp_teamID" = "teamID"))
# 
# results_16 <- results_16 %>%
#   left_join(results_16[, c("year", "week", "teamID", "pts")], by = c("year", "week", "opp_teamID" = "teamID")) %>%
#   select(year, week, owner = owner.x, week_total = pts.x, opp_owner = owner.y, opp_week_total = pts.y, bracket_section_16) %>%
#   mutate(won_matchup = ifelse(week_total > opp_week_total, 1, ifelse(week_total == opp_week_total, .5, 0))) %>%
#   filter(week_total != 0 & opp_week_total != 0)
# 
# # bind weeks 14, 15 and 16 results together with original results df and sort by year, week
# results_14 <- results_14 %>% select(-bracket_section)
# results_15 <- results_15 %>% select(-bracket_section_15)
# results_16 <- results_16 %>% select(-bracket_section_16)
# 
# results <- bind_rows(results, results_14, results_15, results_16) %>% arrange(year, week)
# 
# remove rows where both scores are 0 (games haven't been played yet)
results <- results %>% filter(week_total != 0 & opp_week_total != 0)
