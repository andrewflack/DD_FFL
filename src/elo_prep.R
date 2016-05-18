
# is new season
results <- results %>% 
  mutate(new_season = ifelse(year != 2010 & week == 1, 1, 0)) %>%
  arrange(year, week)

# is copy
results.sort <- t(apply(results[,c("year", "week", "owner", "opp_owner")], 1, sort))
results$is_copy <- ifelse(duplicated(results.sort) == TRUE, 1, 0)

# is new owner
# http://stackoverflow.com/questions/30159041/creating-a-running-counting-variable-in-r
results$owner_n <- sapply(1:nrow(results), function(i) sum(results$owner[i] == unlist(results[1:i, c("owner")])))

results <- results %>%
  arrange(year, week) %>%
  # new team if owner has only played on game (owner_n = 1) and it's the start of a new season
  mutate(new_owner = ifelse((owner_n == 1 & new_season == 1) | (owner_n == 1 & year == 2010), 1, 0))

# add "period" to count consecutive weeks across full history
results$period <- rep(1:((length(unique(results$year)))*13), each = 8)

# add margin of victory (mov)
results <- results %>% 
  mutate(mov = week_total - opp_week_total)

init <- 1500
k <- 20

# initialize 3D array to track elo_i and elo_n rating by owner and period
elohist_i <- array(init, dim = c(length(sort(unique(c(results$owner, results$opp_owner)))), max(results$period) + 1), dimnames = list(sort(unique(results$owner)), seq(1:(max(results$period) + 1))))
elohist_n <- array(init, dim = c(length(sort(unique(c(results$owner, results$opp_owner)))), max(results$period)), dimnames = list(sort(unique(results$owner)), seq(1:max(results$period))))

results$elo_i <- NA
results$elo_n <- NA
results$opp_elo_i <- NA
results$elo_diff <- NA
results$MOVM <- NA
results$exp <- NA

for (i in 1:nrow(results)) {
  if (results$new_owner[i] == 1) {
    results$elo_i[i] <- init
  } else {
    results$elo_i[i] <- elohist_i[results$owner[i], results$period[i]]
  }
  results$opp_elo_i[i] <- elohist_i[results$opp_owner[i], results$period[i]]
  results$elo_diff[i] <- results$elo_i[i] - results$opp_elo_i[i]
  print(paste0("Owner elo_i: ", results$elo_i[i], " opp_owner elo_i: ", elohist_i[results$opp_owner[i], results$period[i]], " elo_diff: ", results$elo_diff[i]))
  results$exp[i] <- 1 - 1/((10^(results$elo_diff[i]/400)) + 1)
  if (results$won_matchup[i] == 1) {
    results$MOVM[i] <- log(abs(results$mov[i]) + 1)*(2.2/((results$elo_i[i] - results$opp_elo_i[i]) * .001 + 2.2))
  } else {
    results$MOVM[i] <- log(abs(results$mov[i]) + 1)*(2.2/((results$opp_elo_i[i] - results$elo_i[i]) * .001 + 2.2))
  }
  results$elo_n[i] <- results$elo_i[i] + (results$MOVM[i]*k*((abs(results$mov[i])^.8)/(7.5 + .006*(abs(results$elo_diff[i])))))*(results$won_matchup[i]-results$exp[i])
  elohist_n[results$owner[i], results$period[i]] <- results$elo_n[i]
  elohist_i[results$owner[i], results$period[i] + 1] <- results$elo_n[i]
}

results %>% 
  ggplot(aes(x = period, y = elo_n, color = owner)) + 
  facet_wrap(~owner) + 
  geom_line(color = "red") + 
  geom_vline(xintercept = c(13, 26, 39, 52, 65), color = "grey50") + 
  theme_bw()
