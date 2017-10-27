# Function to pull scores tables from ESPN site

getScoresTable <- function(year,week,teamID){
  url <- paste(url1, teamID, url2, week, url3, year, sep = "")
  parsedurl <- htmlParse(url)
  tableNodes <- getNodeSet(parsedurl,"//table")
  tab <- readHTMLTable(tableNodes[[2]], header = TRUE, skip.rows = c(1,2,12), stringsAsFactors = FALSE)
  tab$year <- year
  tab$week <- week
  tab$teamID <- teamID
  tab
}