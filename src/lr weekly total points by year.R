library(ProjectTemplate)
load.project()

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

