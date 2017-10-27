


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

source("src/rest_of_season_sim.R")