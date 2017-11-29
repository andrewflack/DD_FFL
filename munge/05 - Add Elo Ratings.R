# add elo ratings #########################

AddEloRatings <- function(df, k = 10, revert = 3/4, playoff_revert = 2/3, init = 1500, keep_all = FALSE, verbose = FALSE) {
  
  # is new season
  df_w_elo <- df %>% 
    mutate(new_season = ifelse(year != min(years) & week == 1, 1, 0)) %>%
    arrange(year, week)
  
  # is copy
  df_w_elo.sort <- t(apply(df_w_elo[,c("year", "week", "owner", "opp_owner")], 1, sort))
  df_w_elo$is_copy <- ifelse(duplicated(df_w_elo.sort) == TRUE, 1, 0)
  
  # is new owner
  # http://stackoverflow.com/questions/30159041/creating-a-running-counting-variable-in-r
  df_w_elo$owner_n <- sapply(1:nrow(df_w_elo), function(i) sum(df_w_elo$owner[i] == unlist(df_w_elo[1:i, c("owner")])))
  
  df_w_elo <- df_w_elo %>%
    arrange(year, week) %>%
    # new team if owner has only played on game (owner_n = 1) and it's the start of a new season
    mutate(new_owner = ifelse((owner_n == 1 & new_season == 1) | (owner_n == 1 & year == min(years)), 1, 0))
  
  # add "period" to count consecutive weeks across full history
  # df_w_elo$period <- rep(1:((length(unique(df_w_elo$year)))*13), each = 8)
  df_w_elo <- df_w_elo %>% 
    mutate(yrwk = paste0(year, week)) %>% 
    mutate(period = CountDistinctAlong(yrwk)) %>% 
    select(-yrwk)
  
  # add margin of victory (mov)
  df_w_elo <- df_w_elo %>% 
    mutate(mov = week_total - opp_week_total)
  
  # init <- 1500
  # k <- 10
  # revert <- 3/4 # between seasons, a team retains 1/4 of their rating
  # 
  # # revert_playoffs
  # playoff_revert <- 2/3
  
  # initialize 3D array to track elo_i and elo_n rating by owner and period
  elohist_i <- array(init, dim = c(length(sort(unique(c(df_w_elo$owner, df_w_elo$opp_owner)))), max(df_w_elo$period) + 1), dimnames = list(sort(unique(df_w_elo$owner)), seq(1:(max(df_w_elo$period) + 1))))
  elohist_n <- array(NA, dim = c(length(sort(unique(c(df_w_elo$owner, df_w_elo$opp_owner)))), length(unique(df$year))*weeks), dimnames = list(sort(unique(df_w_elo$owner)), seq(1:(length(unique(df$year))*weeks))))
  
  df_w_elo$elo_i <- NA
  df_w_elo$elo_n <- NA
  df_w_elo$opp_elo_i <- NA
  df_w_elo$elo_diff <- NA
  df_w_elo$MOVM <- NA
  df_w_elo$exp <- NA
  
  for (i in 1:nrow(df_w_elo)) {
    if (df_w_elo$new_owner[i] == 1) {
      df_w_elo$elo_i[i] <- init
    } else {
      df_w_elo$elo_i[i] <- elohist_i[df_w_elo$owner[i], df_w_elo$period[i]]
    }
    
    if (df_w_elo$new_season[i] == 1) {
      df_w_elo$elo_i[i] <- init*revert + df_w_elo$elo_i[i]*(1-revert)
    }
    
    if (df_w_elo$week[i] == 14) {
      df_w_elo$elo_i[i] <- init*playoff_revert + df_w_elo$elo_i[i]*(1-playoff_revert)
    }
    
    df_w_elo$opp_elo_i[i] <- elohist_i[df_w_elo$opp_owner[i], df_w_elo$period[i]]
    
    df_w_elo$elo_diff[i] <- df_w_elo$elo_i[i] - df_w_elo$opp_elo_i[i]
    
    if(verbose){
      print(i)
      print(paste0("Owner elo_i: ", round(df_w_elo$elo_i[i]), " opp_owner elo_i: ", round(elohist_i[df_w_elo$opp_owner[i], df_w_elo$period[i]]), " elo_diff: ", round(df_w_elo$elo_diff[i])))
    }
    
    df_w_elo$exp[i] <- 1 - 1/((10^(df_w_elo$elo_diff[i]/400)) + 1)
    
    # calculate MOVM
    if (df_w_elo$won_matchup[i] == 1) {
      df_w_elo$MOVM[i] <- log(abs(df_w_elo$mov[i]) + 1)*(2.2/((df_w_elo$elo_i[i] - df_w_elo$opp_elo_i[i]) * .001 + 2.2))
    } else {
      df_w_elo$MOVM[i] <- log(abs(df_w_elo$mov[i]) + 1)*(2.2/((df_w_elo$opp_elo_i[i] - df_w_elo$elo_i[i]) * .001 + 2.2))
    }
    
    # calculate new rating (elo_n)
    df_w_elo$elo_n[i] <- df_w_elo$elo_i[i] + df_w_elo$MOVM[i]*k*(df_w_elo$won_matchup[i]-df_w_elo$exp[i])
    
    # update recordkeeping
    elohist_n[df_w_elo$owner[i], df_w_elo$period[i]] <- df_w_elo$elo_n[i]
    elohist_i[df_w_elo$owner[i], df_w_elo$period[i] + 1] <- df_w_elo$elo_n[i]
  }
  
  if(!keep_all) {
    df_w_elo <- 
      df_w_elo %>% 
      select(year, week, owner, week_total, opp_owner, opp_week_total, won_matchup, elo_i, elo_n) 
  }
  
  return(list(df_w_elo, elohist_n, k))
}
# debug(AddEloRatings)
add_elo_output <- AddEloRatings(df = results)
results_w_elo <- add_elo_output[[1]]
elohist_n <- add_elo_output[[2]]
k <- add_elo_output[[3]]

write.csv(results_w_elo, "data/results_w_elo.csv", row.names = FALSE)

# # export for calibration check
# df_w_elo %>%
#   filter(is_copy == 0) %>%
#   write.csv(paste0("data/df_w_elo_", k, "_", revert, ".csv"), row.names = FALSE)