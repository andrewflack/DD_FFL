# Function to pull scoring leaders tables from ESPN site

getScoringLeadersTable <- function(year,week,n_players,start_index){
  url <- paste0(url1, leagueId, url2, week, url3, year, url4, start_index)
  parsedurl <- htmlParse(url)
  tableNodes <- getNodeSet(parsedurl,"//table")
  tab <- readHTMLTable(tableNodes[[2]], header = TRUE, skip.rows = c(1,2,12), stringsAsFactors = FALSE)
  tab$year <- year
  tab$week <- week
  tab$teamID <- teamID
  tab
}