# add elo ratings #########################

# is new season
results_w_elo <- results %>% 
  mutate(new_season = ifelse(year != 2010 & week == 1, 1, 0)) %>%
  arrange(year, week)

# is copy
results_w_elo.sort <- t(apply(results_w_elo[,c("year", "week", "owner", "opp_owner")], 1, sort))
results_w_elo$is_copy <- ifelse(duplicated(results_w_elo.sort) == TRUE, 1, 0)

# is new owner
# http://stackoverflow.com/questions/30159041/creating-a-running-counting-variable-in-r
results_w_elo$owner_n <- sapply(1:nrow(results_w_elo), function(i) sum(results_w_elo$owner[i] == unlist(results_w_elo[1:i, c("owner")])))

results_w_elo <- results_w_elo %>%
  arrange(year, week) %>%
  # new team if owner has only played on game (owner_n = 1) and it's the start of a new season
  mutate(new_owner = ifelse((owner_n == 1 & new_season == 1) | (owner_n == 1 & year == 2010), 1, 0))

# add "period" to count consecutive weeks across full history
# results_w_elo$period <- rep(1:((length(unique(results_w_elo$year)))*13), each = 8)
results_w_elo <- results_w_elo %>% 
  mutate(yrwk = paste0(year, week)) %>% 
  mutate(period = CountDistinctAlong(yrwk)) %>% 
  select(-yrwk)

# add margin of victory (mov)
results_w_elo <- results_w_elo %>% 
  mutate(mov = week_total - opp_week_total)

init <- 1500
k <- 10
revert <- 3/4 # between seasons, a team retains 1/4 of their rating

# initialize 3D array to track elo_i and elo_n rating by owner and period
elohist_i <- array(init, dim = c(length(sort(unique(c(results_w_elo$owner, results_w_elo$opp_owner)))), max(results_w_elo$period) + 1), dimnames = list(sort(unique(results_w_elo$owner)), seq(1:(max(results_w_elo$period) + 1))))
elohist_n <- array(NA, dim = c(length(sort(unique(c(results_w_elo$owner, results_w_elo$opp_owner)))), length(unique(results$year))*13), dimnames = list(sort(unique(results_w_elo$owner)), seq(1:(length(unique(results$year))*13))))

results_w_elo$elo_i <- NA
results_w_elo$elo_n <- NA
results_w_elo$opp_elo_i <- NA
results_w_elo$elo_diff <- NA
results_w_elo$MOVM <- NA
results_w_elo$exp <- NA

for (i in 1:nrow(results_w_elo)) {
  if (results_w_elo$new_owner[i] == 1) {
    results_w_elo$elo_i[i] <- init
  } else {
    results_w_elo$elo_i[i] <- elohist_i[results_w_elo$owner[i], results_w_elo$period[i]]
  }
  
  if (results_w_elo$new_season[i] == 1) {
    results_w_elo$elo_i[i] <- init*revert + results_w_elo$elo_i[i]*(1-revert)
  }
  
  results_w_elo$opp_elo_i[i] <- elohist_i[results_w_elo$opp_owner[i], results_w_elo$period[i]]
  
  results_w_elo$elo_diff[i] <- results_w_elo$elo_i[i] - results_w_elo$opp_elo_i[i]
  print(paste0("Owner elo_i: ", results_w_elo$elo_i[i], " opp_owner elo_i: ", elohist_i[results_w_elo$opp_owner[i], results_w_elo$period[i]], " elo_diff: ", results_w_elo$elo_diff[i]))
  
  results_w_elo$exp[i] <- 1 - 1/((10^(results_w_elo$elo_diff[i]/400)) + 1)
  
  # calculate MOVM
  if (results_w_elo$won_matchup[i] == 1) {
    results_w_elo$MOVM[i] <- log(abs(results_w_elo$mov[i]) + 1)*(2.2/((results_w_elo$elo_i[i] - results_w_elo$opp_elo_i[i]) * .001 + 2.2))
  } else {
    results_w_elo$MOVM[i] <- log(abs(results_w_elo$mov[i]) + 1)*(2.2/((results_w_elo$opp_elo_i[i] - results_w_elo$elo_i[i]) * .001 + 2.2))
  }
  
  # calculate new rating (elo_n)
  results_w_elo$elo_n[i] <- results_w_elo$elo_i[i] + results_w_elo$MOVM[i]*k*(results_w_elo$won_matchup[i]-results_w_elo$exp[i])
  
  # update recordkeeping
  elohist_n[results_w_elo$owner[i], results_w_elo$period[i]] <- results_w_elo$elo_n[i]
  elohist_i[results_w_elo$owner[i], results_w_elo$period[i] + 1] <- results_w_elo$elo_n[i]
}

results_w_elo <- 
  results_w_elo %>% 
  select(year, week, owner, week_total, opp_owner, opp_week_total, won_matchup, elo_i, elo_n)

write.csv(results_w_elo, "data/results_w_elo.csv", row.names = FALSE)

# # export for calibration check
# results_w_elo %>%
#   filter(is_copy == 0) %>%
#   write.csv(paste0("data/results_w_elo_", k, "_", revert, ".csv"), row.names = FALSE)