simulateRestOfSeason <- function(n_sims, current_season, weeks_played, .elohist_n = elohist_n, .wins_ytd = wins_ytd, .current_ratings = current_ratings) {
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
        
        t1 <- teams_and_owners[which(teams_and_owners$year == current_season & teams_and_owners$teamID == as.numeric(week_matchups[j, "teamID"])), "owner"]
        t2 <- teams_and_owners[which(teams_and_owners$year == current_season & teams_and_owners$teamID == as.numeric(week_matchups[j, "opp_teamID"])), "owner"]
        
        pd <- ((current_season-2010)*13) + i-1
        # print(pd)
        
        if (i == 1){ # 0 weeks played
          elo_1 <- as.numeric(current_ratings[current_ratings$owner == as.character(t1), "rating"])
          elo_2 <- as.numeric(current_ratings[current_ratings$owner == as.character(t2), "rating"])
        } else {
          elo_1 <- sim_elo_n[as.character(t1), pd]
          elo_2 <- sim_elo_n[as.character(t2), pd] 
        }
        
        t1_w_exp <- 1 - 1/((10^((elo_1 - elo_2)/400)) + 1)
        
        # simulate a single game
        t1_won_matchup <- rbinom(1, 1, t1_w_exp)
        t2_won_matchup <- 1 - t1_won_matchup
        
        # update ytd wins
        sim_wins[which(sim_wins$owner == as.character(t1)), "wins"] <- as.numeric(sim_wins[which(sim_wins$owner == as.character(t1)), "wins"]) + t1_won_matchup
        sim_wins[which(sim_wins$owner == as.character(t2)), "wins"] <- as.numeric(sim_wins[which(sim_wins$owner == as.character(t2)), "wins"]) + t2_won_matchup
        
        # update elo ratings
        t1_elo_n <- elo_1 + k*(t1_won_matchup-t1_w_exp)
        t2_elo_n <- elo_2 + k*(t2_won_matchup-(1-t1_w_exp))
        
        sim_elo_n[as.character(t1), pd + 1] <- t1_elo_n
        sim_elo_n[as.character(t2), pd + 1] <- t2_elo_n
        
      }
      
    }
    
    final_ratings <- sim_elo_n[, pd] %>% as.data.frame() %>% rownames_to_column()
    colnames(final_ratings) <- c("owner", "rating")
    
    # assign finish place based on simulated wins and rating (tiebreaker)
    # sim_wins <- sim_wins %>% 
    #   left_join(final_ratings, by = c("owner")) %>% 
    #   arrange(desc(wins), desc(rating)) %>%
    #   mutate(place = seq_along(wins))
    sim_wins <- sim_wins %>%
      left_join(final_ratings, by = c("owner")) %>%
      group_by(wins) %>%
      sample_frac(weight = rating) %>%
      arrange(desc(wins)) %>%
      ungroup() %>%
      mutate(place = seq_along(wins))
    
    sim_results[[sim]] <- sim_wins # regular season wins
    
    #### end of regular season, now simulate playoffs ###
    
    # Assign playoff seeds
    
  }
  
  sim_results_tall <- plyr::ldply(sim_results, data.frame)
  
  return(sim_results_tall)
}
