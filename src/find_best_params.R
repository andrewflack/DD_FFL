# elo calibration

library(ProjectTemplate)
load.project()

parameter_space <- data.frame(k = seq(5, 25, 2.5),
                              revert = seq(.10, .90, .10),
                              playoff_revert = seq(.10, .9, .10))

parameter_space <- parameter_space %>% complete(k, revert, playoff_revert)

CalculateBrierScore <- function(df, k, revert, playoff_revert){
  
  results_w_elo_tmp <- AddEloRatings(df, k, revert, playoff_revert, keep_all = TRUE)[[1]]
  
  results_w_elo_tmp <- results_w_elo_tmp %>% 
    filter(is_copy == 0) %>% 
    select(won_matchup, exp) %>% 
    mutate(brier = (exp - won_matchup)^2)
    
  score <- sum(results_w_elo_tmp$brier)
  
  return(score)
}

parameter_space <- parameter_space %>% 
  rowwise() %>% 
  mutate(score = CalculateBrierScore(df = results, 
                                     k = k, 
                                     revert = revert, 
                                     playoff_revert = playoff_revert))

write.csv(parameter_space, file = "data/parameters_w_brier.csv", row.names = FALSE)

parameter_space %>%
  ggplot(aes(x = as.factor(k), y = score, colour = as.factor(revert))) + 
  geom_line(aes(group = revert)) + 
  facet_wrap(~playoff_revert) +
  theme_minimal() +
  labs(x = "k",
       y = "Brier Score",
       title = "Historical Prediction Error for Varying Parameters",
       subtitle = "Panels: Reversion Entering Playoffs") +
  guides(colour = guide_legend(title = "Reversion Between\nSeasons"))

parameter_space[which.min(parameter_space$score),]
