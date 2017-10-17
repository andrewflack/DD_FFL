# simulate rest of season

current_season <- max(results_w_elo$year)
weeks_played <- max(results_w_elo[which(results_w_elo$year == current_season), "week"])

wins_ytd <- results_w_elo %>% 
  filter(year == current_season) %>% 
  group_by(owner) %>% 
  summarize(wins = sum(won_matchup)) %>% 
  arrange(desc(wins))

n_sims <- 1000
sim_results <- list()

for (sim in 1:n_sims) {
  
  sim_wins <- wins_ytd
  sim_elo_n <- elohist_n
  
  for (i in (weeks_played + 1):13) {
    # print(i)
    
    # pull the matchups for one week at a time
    week_matchups <- weekly_matchups %>% 
      filter(year == current_season & week == i) %>% 
      slice(seq(1, 7, by = 2))
    
    # now loop through each matchup
    for (j in 1:nrow(week_matchups)) {
      # print(j)
      
      t1 <- teams_and_owners[which(teams_and_owners$year == current_season & teams_and_owners$teamID == week_matchups[j, "teamID"]), "owner"]
      t2 <- teams_and_owners[which(teams_and_owners$year == current_season & teams_and_owners$teamID == week_matchups[j, "opp_teamID"]), "owner"]
      
      pd <- ((current_season-2010)*13) + i-1
      # print(pd)
      
      elo_1 <- sim_elo_n[t1, pd]
      elo_2 <- sim_elo_n[t2, pd]
      
      t1_w_exp <- 1 - 1/((10^((elo_1 - elo_2)/400)) + 1)
      
      # simulate a single game
      t1_won_matchup <- rbinom(1, 1, t1_w_exp)
      t2_won_matchup <- 1 - t1_won_matchup
      
      # update ytd wins
      sim_wins[which(sim_wins$owner == t1), "wins"] <- as.numeric(sim_wins[which(sim_wins$owner == t1), "wins"]) + t1_won_matchup
      sim_wins[which(sim_wins$owner == t2), "wins"] <- as.numeric(sim_wins[which(sim_wins$owner == t2), "wins"]) + t2_won_matchup
      
      # update elo ratings
      t1_elo_n <- elo_1 + k*(t1_won_matchup-t1_w_exp)
      t2_elo_n <- elo_2 + k*(t2_won_matchup-(1-t1_w_exp))
      
      sim_elo_n[t1, pd + 1] <- t1_elo_n
      sim_elo_n[t2, pd + 1] <- t2_elo_n
      
    }
    
  }
  
  sim_wins <- sim_wins %>% arrange(desc(wins)) %>% mutate(place = seq_along(wins))
  sim_results[[sim]] <- sim_wins
}

sim_results_tall <- ldply(sim_results, data.frame)

sim_stats <- sim_results_tall %>% 
  group_by(owner) %>% 
  summarize(sim_wins = round(mean(wins), 2),
            p_1st = sum(place == 1)/n_sims) %>% 
  arrange(desc(sim_wins)) %>% 
  left_join(wins_ytd, by = "owner") %>% 
  select(owner, ytd_wins = wins, sim_wins, p_1st)

current_ratings %>% left_join(sim_stats, by = "owner")

#### Plots ####
sim_results_tall %>% 
  ggplot(aes(x = reorder(owner, wins, FUN = median), y = wins)) + 
  geom_boxplot() + 
  coord_flip()

sim_results_tall %>% 
  group_by(wins) %>% 
  count() %>% 
  mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = as.factor(wins), y = freq)) + 
  geom_bar(stat = "identity")

sim_results_tall %>% 
  group_by(wins, place) %>% 
  count() %>% 
  arrange(place, wins) %>% 
  mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = as.factor(wins), y = freq, fill = as.factor(place))) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~place, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Win Total", y = "P(Finish in Place | Win Total)", 
       title = "Probability of Finishing in 1st - 8th Place",
       subtitle = "Conditional on Win Total")

sim_results_tall %>% 
  group_by(wins, place) %>% 
  count() %>% 
  arrange(place, wins) %>% 
  mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = as.factor(wins), y = freq, fill = as.factor(place))) + 
  geom_bar(stat = "identity") + 
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Win Total", y = "P(Finish in Place | Win Total)", 
       title = "Probability of Finishing in 1st - 8th Place",
       subtitle = "Conditional on Win Total")