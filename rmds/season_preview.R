# External R script to generate the in-season weekly updates

## ---- current_ratings
current_ratings <- results_w_elo %>% 
  filter(year == params$current_season - 1) %>% 
  filter(week == max(week)) %>% 
  mutate(new_rating = params$init*params$revert + elo_n*(1-params$revert)) %>%
  select(owner, rating = new_rating) %>% 
  mutate_if(is.numeric, funs(round(., 0))) %>% 
  arrange(desc(rating))
## ---- end current_ratings

## ---- wins_ytd
wins_ytd <- results_w_elo %>% 
  filter(year == params$current_season - 1) %>% 
  filter(week == 13) %>% 
  mutate(wins = 0) %>% 
  select(owner, wins)
## ---- end wins_ytd

## ---- simulate_season
sim_results_tall <- simulateRestOfSeason(params$n_sims, params$current_season, params$weeks_played)

filename <- paste0("data/sim_results_", params$current_season, "_week_", params$weeks_played, ".csv")
write.csv(sim_results_tall, filename, row.names = FALSE)
## ---- end simulate_season

## ---- sim_dashboard
sim_stats <- sim_results_tall %>% 
  group_by(owner) %>% 
  summarize(sim_wins = round(mean(wins), 2),
            p_1st = sum(place == 1)/params$n_sims) %>% 
  arrange(desc(sim_wins)) %>% 
  left_join(wins_ytd, by = "owner") %>% 
  select(owner, ytd_wins = wins, sim_wins, p_1st)

sim_dashboard <- current_ratings %>% 
  left_join(sim_stats, by = "owner")

sim_dashboard$current_season <- params$current_season
sim_dashboard$weeks_played <- params$weeks_played

filename <- paste0("data/sim_dashboard_", params$current_season, "_week_", params$weeks_played, ".csv")
write.csv(sim_dashboard, filename, row.names = FALSE)
## ---- end sim_dashboard

## ---- predicted_wins_graphic
owner_order <- sim_results_tall %>%
  group_by(owner) %>%
  summarize(avg_wins = mean(wins)) %>%
  arrange(desc(avg_wins)) %>%
  select(owner) %>%
  collect() %>%
  .[["owner"]]

sim_results_tall$owner_f <- ordered(sim_results_tall$owner, owner_order)

sim_results_tall %>%
  group_by(owner_f, wins) %>%
  count() %>%
  mutate(freq = n/params$n_sims) %>%
  ggplot(aes(x = as.factor(wins), y = freq, fill = owner_f)) +
  geom_bar(stat = "identity") +
  facet_grid(owner_f ~ ., switch = "y") +
  scale_y_continuous(position = "right") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.y = element_text(angle = 180)) +
  labs(x = "Wins", y = NULL, title = NULL)
## ---- end predicted_wins_graphic

## ---- next_week_matchups
next_week_matchups <- weekly_matchups %>% 
  filter(year == params$current_season & week == params$weeks_played + 1) %>% 
  slice(seq(1, 7, by = 2)) %>% 
  left_join(teams_and_owners, by = c("teamID", "year")) %>% 
  left_join(teams_and_owners, by = c("opp_teamID" = "teamID", "year")) %>% 
  select(-name.x, -name.y) %>% 
  rename(owner = owner.x, opp_owner = owner.y) %>% 
  left_join(current_ratings[, c("owner", "rating")], by = "owner") %>% 
  left_join(current_ratings[, c("owner", "rating")], by = c("opp_owner" = "owner")) %>% 
  rename(rating = rating.x, opp_rating = rating.y) %>% 
  mutate(w_exp = round(1 - 1/((10^((rating - opp_rating)/400)) + 1), 2)) %>% 
  select(owner, opp_owner, w_exp) %>% 
  transmute(matchup = ifelse(w_exp >= .5, paste0(owner, " over ", opp_owner), 
                             paste0(opp_owner, " over ", owner)), 
            w_exp = ifelse(w_exp >= .5, w_exp, 1-w_exp)) %>% 
  arrange(desc(w_exp))
## ---- end next_week_matchups

