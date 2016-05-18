# Calculate elo ratings for fantasy football league (DDFFL)

# Input is a dataframe containing seven variables: 
# (1) a numeric vector denoting the time period in which the matchup took place 
# (2) character identifier for owner one 
# (3) a character identifier for owner two 
# (4) the result of the matchup expressed as a number (1 for owner win, 0 for owner loss, .5 for tie) 
# (5) margin of victory (owner - opp_owner) 
# (6) binary flag if "owner" is a new owner 
# (7) binary flag if period is the start of a new season

calculateDDFFLelo <- function(x, init=1500, k=20) {
  names(x) <- c("year", "week", "owner", "week_total", "opp_owner", "opp_week_total", "won_matchup", "new_season", "is_copy", "owner_n", "new_owner", "period", "mov")
  
  owner_list <- sort(unique(c(x$owner, x$opp_owner)))
  year_list <- sort(unique(x$year))
  week_list <- sort(unique(x$week))
  
  n_owners <- length(owner_list)
  n_years <- length(year_list)
  n_weeks <- length(week_list)
  
  # initialize array for initial ratings
  elo_i <- array(NA, dim = c(n_owners, n_years, n_weeks), dimnames = list(NULL, year_list, NULL))
  
  if(x$new_owner == 1) {
    elo_i[x$owner, x$year, x$week] <- init
  }
}