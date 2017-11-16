# check to see if new data exists and scrape if necessary

years <- 2010:2017
weeks <- 16
teams <- 8

y <- rep(years, each = length(years)*weeks)
w <- rep(1:weeks, each = teams, times = length(years))
t <- rep(1:length(years), times = length(years)*weeks)
dat <- data.frame(y, w, t)

if (file.exists("data/scores_raw.csv")) {
  
  # if scores_raw.csv exists, load it
  print("Loading scores_raw.csv")
  scores_raw <- read_csv("data/scores_raw.csv")
  
  # find out the year and week of the most recent data in the file
  last_year <- max(scores_raw$y)
  
  last_week <- scores_raw %>% 
    filter(y == last_year) %>% 
    filter(pts != 0 & !is.na(pts)) %>% 
    select(w) %>% 
    collect() %>% 
    .[["w"]] %>% 
    max()
  
  # print(paste0("Last Year: ", last_year, ", Last Week: ", last_week))
  
  # check if new data exists by pulling one score
  last_score <- getScore(last_year, last_week + 1, 1)
  new_data <- !(is.na(last_score) | last_score == 0)
  
  if (new_data) {
    print("New Data Exists: Scraping...")
    
    new_dat <- dat %>% 
      filter(y >= last_year & w > last_week)
    
    scores_raw_new <- new_dat %>% 
      rowwise() %>%
      mutate(pts = getScore(y, w, t))
    
    scores_raw <- rbind(scores_raw, scores_raw_new)
    write.csv(scores_raw, "data/scores_raw.csv", row.names = FALSE)
    
  } else {
    print("No New Data - Up to Date")
  }
    
} else {
  
  # file doesn't exist, so scrape the whole dang thing
  scores_raw <- dat %>%
    rowwise() %>%
    mutate(pts = getScore(y, w, t))
  
  write.csv(scores_raw, "data/scores_raw.csv", row.names = FALSE)
}

# join teams and owners and clean up #
teams_and_owners <- read_csv("data/ddffl_teamsandowners.csv")

scores_clean <- scores_raw %>% 
  left_join(teams_and_owners, by = c("y" = "year", "t" = "teamID")) %>% 
  rename(year = y, week = w, teamID = t)

write.csv(scores_clean, "data/scores_clean.csv", row.names = FALSE)

