### Who was most unlucky during the 2014 season?

library(ProjectTemplate)
load.project()

weekly2014 <- scores.clean %>%
  select(year, teamID, week, name, owner, FFLpts, pos, player) %>%
  filter(year == 2014) %>%
  spread(pos, FFLpts) %>%
  select(year, teamID, owner, week, QB, RB, WR, TE, K, DST) %>% 
  group_by(year, teamID, owner, week) %>% 
  summarise_each(funs(sum(., na.rm = TRUE)))  %>% # this sums points per week by position (total of all 3 RBs started, for example)
  mutate(week_total = K + QB + RB + TE + WR + DST)

### Determine whether each row is a matchup win or loss
ddffl.weeklymatchups$year <- factor(ddffl.weeklymatchups$year)
ddffl.weeklymatchups$week <- factor(ddffl.weeklymatchups$week)
ddffl.weeklymatchups$teamID <- factor(ddffl.weeklymatchups$teamID)
weekly2014$year <- factor(weekly2014$year)
weekly2014$week <- factor(weekly2014$week)
weekly2014$teamID <- factor(weekly2014$teamID)
weekly2014 <- left_join(weekly2014, ddffl.weeklymatchups, by = c("year", "week", "teamID"))

getFFLoppScore <- function(the_year, the_week, the_opp_teamID) { 
  FFLoppscore <- weekly2014$week_total[weekly2014$year == the_year & weekly2014$week == the_week & weekly2014$teamID == the_opp_teamID]
  FFLoppscore
}

weekly2014$opp_week_total <- mapply(getFFLoppScore, weekly2014$year, weekly2014$week, weekly2014$opp_teamID)

weekly2014 <- weekly2014 %>%
  mutate(won_matchup = ifelse(week_total > opp_week_total, 1, 0))

# generate logistic regression
luck_logit <- glm(won_matchup ~ QB + RB + WR + TE + K + DST, data = weekly2014, family = "binomial") # use full weekly data to create model
summary(luck_logit)

luck_logit2 <- glm(won_matchup ~ QB + RB + WR + TE + DST, data = weekly2014, family = "binomial") # summary shows that K is not significant
summary(luck_logit2)

luck_logit3 <- glm(won_matchup ~ week_total, data = weekly2014, family = "binomial") # use only week_total to create model
summary(luck_logit3)

# use logit to produce win probabilities
weekly2014$win_prob <- round(predict(luck_logit3, newdata = weekly2014[,c("week_total")], type = "response"),2)

nreps <- 10000
season_sim <- matrix(NA, nrow = nreps, ncol = 8)
colnames(season_sim) <- c("Jeff M", "Nathan S", "Steve C", "John N", "John L", "Craig B", "Andrew F", "Steve F")

set.seed(1234)
for (i in 1:nreps) {
  y <- rbinom(n=nrow(weekly2014), size=1, prob=weekly2014$win_prob)
  df <- data.frame(weekly2014$owner, y)
  season_sim[i,1] <- sum(df$y[which(df$weekly2014.owner=="Jeff M")])
  season_sim[i,2] <- sum(df$y[which(df$weekly2014.owner=="Nathan S")])
  season_sim[i,3] <- sum(df$y[which(df$weekly2014.owner=="Steve C")])
  season_sim[i,4] <- sum(df$y[which(df$weekly2014.owner=="John N")])
  season_sim[i,5] <- sum(df$y[which(df$weekly2014.owner=="John L")])
  season_sim[i,6] <- sum(df$y[which(df$weekly2014.owner=="Craig B")])
  season_sim[i,7] <- sum(df$y[which(df$weekly2014.owner=="Andrew F")])
  season_sim[i,8] <- sum(df$y[which(df$weekly2014.owner=="Steve F")])
}

weekly2014 %>% 
  group_by(owner) %>% 
  summarize(n_wins = sum(won_matchup)) %>%
  merge(data.frame(owner = c("Jeff M", "Nathan S", "Steve C", "John N", "John L", "Craig B", "Andrew F", "Steve F"), pred_wins = round(unname(colMeans(season_sim)),1)), by = "owner") %>%
  mutate(delta = n_wins - pred_wins) %>%
  arrange(desc(pred_wins))

as.data.frame(season_sim) %>% 
  gather(owner, wins) %>% 
  ggplot(aes(x = owner, y = wins)) + 
  geom_boxplot()

# 5 most unlucky losses
weekly2014 %>% 
  ungroup() %>% 
  select(owner, week_total, win_prob, opp_week_total, won_matchup) %>% 
  filter(won_matchup == 0) %>% 
  arrange(desc(win_prob)) %>%
  top_n(5, win_prob)

# 5 luckiest wins
weekly2014 %>% 
  ungroup() %>% 
  select(owner, week_total, win_prob, opp_week_total, won_matchup) %>% 
  filter(won_matchup == 1) %>% 
  arrange(desc(win_prob)) %>%
  tail(5) %>%
  arrange(win_prob)