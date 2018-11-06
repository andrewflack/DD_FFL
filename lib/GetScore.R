# Function to scrape the score for a given year/week/teamID combination

getScore <- function(year, week, teamID){
  
  # 2010 week 1
  # http://games.espn.go.com/ffl/boxscorequick?leagueId=341583&teamId=1&scoringPeriodId=1&seasonId=2010&view=scoringperiod&version=quick
  
  url1 <- 'http://games.espn.com/ffl/boxscorequick?leagueId=341583&teamId='
  url2 <- '&scoringPeriodId='
  url3 <- '&seasonId='
  url4 <- '&view=scoringperiod&version=quick'
  
  box <- paste0(url1, teamID, url2, week, url3, year, url4)
  
  if(year == 2016) {
    score <- read_html(box) %>% 
      # html_node("div:nth-child(8) .totalScore") %>% 
      html_node("#playertable_0 .playerTableBgRowTotals .appliedPoints") %>% 
      html_text() %>% 
      as.numeric()
  } else if (year == 2017) {
    score <- read_html(box) %>% 
      # html_node("div:nth-child(6) .totalScore") %>% 
      html_node("div:nth-child(8) .totalScore") %>% 
      html_text() %>% 
      as.numeric()    
  } else if (year == 2018) {
    score <- read_html(box) %>% 
      html_node("div:nth-child(6) .totalScore") %>% 
      html_text() %>% 
      as.numeric()    
  } else {
    score <- read_html(box) %>% 
      html_node("#playertable_0 .playerTableBgRowTotals .appliedPoints") %>% 
      html_text() %>% 
      as.numeric()
  }
  
  return(score)
  
}