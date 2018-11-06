# External R script to generate the in-season weekly updates

## ---- current_ratings
current_ratings <- results_w_elo %>% 
  filter(year == params$current_season) %>% 
  filter(week == params$weeks_played) %>% 
  mutate(one_week_change = elo_n - elo_i) %>% 
  select(owner, rating = elo_n, one_week_change) %>% 
  mutate_if(is.numeric, funs(round(., 0))) %>% 
  arrange(desc(rating))
## ---- end current_ratings

## ---- wins_ytd
wins_ytd <- results_w_elo %>% 
  filter(year == params$current_season) %>% 
  filter(week <= params$weeks_played) %>% 
  group_by(owner) %>% 
  summarize(wins = sum(won_matchup)) %>% 
  arrange(desc(wins))
## ---- end wins_ytd

## ---- simulate_season
sim_results_tall <- simulateRestOfSeason(params$n_sims, params$current_season, params$weeks_played)
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

## ---- win_probs_through_season
sim_file_names <- list.files("data/", pattern = "sim_dashboard")
sim_file_names <- sapply(sim_file_names, function(x) paste0("data/", x), USE.NAMES = FALSE)
all_sim_results <- do.call(rbind, lapply(sim_file_names, read_csv))

all_sim_results %>% 
  filter(current_season == params$current_season & weeks_played <= params$weeks_played) %>% 
  select(weeks_played, owner, p_1st) %>% 
  gather(key = variable, value = value, owner) %>% 
  ggplot(aes(x = weeks_played, y = p_1st, colour = value, group = value)) + 
  geom_line(size = 2, alpha = .75) +
  theme_minimal() +
  labs(x = "Week", 
       y = "Probability", 
       title = "Probability of Winning Regular Season Championship By Week") +
  guides(colour = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = c(0:13), limits = c(0, 13)) +
  ylim(0, 1)
## ---- end win_probs_through_season
