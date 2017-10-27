# manually fix ties
results[which(results$year == 2014 & results$week == 10 & results$owner == "Nathan S"), "won_matchup"] <- 1
results[which(results$year == 2014 & results$week == 10 & results$owner == "Craig B"), "won_matchup"] <- 0

results[which(results$year == 2015 & results$week == 8 & results$owner == "John L"), "won_matchup"] <- 1
results[which(results$year == 2015 & results$week == 8 & results$owner == "Andrew F"), "won_matchup"] <- 0

results[which(results$year == 2015 & results$week == 10 & results$owner == "John N"), "won_matchup"] <- 1
results[which(results$year == 2015 & results$week == 10 & results$owner == "Steve F"), "won_matchup"] <- 0

results[which(results$year == 2016 & results$week == 10 & results$owner == "Jeff M"), "won_matchup"] <- 1
results[which(results$year == 2016 & results$week == 10 & results$owner == "John L"), "won_matchup"] <- 0
