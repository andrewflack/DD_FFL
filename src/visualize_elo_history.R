#### Visualize FFL Elo History ####

ffl.elo$year <- as.factor(ffl.elo$year)

ffl.elo %>% 
  arrange(year, week) %>% 
  select(game_order, year, week, owner, elo_n) %>% 
  ggplot(aes(x = game_order, y = elo_n, color = owner)) + 
  geom_line(group = 1, size = 1.5)

ffl.elo %>% 
  arrange(year, week) %>% 
  select(year, week, owner, elo_n) %>% 
  mutate(year_week = as.factor(paste(year, week, sep = "-"))) %>% 
  ggplot(aes(x = reorder(year_week, order(year, week)), y = elo_n, color = owner)) + 
  geom_line(aes(group = owner), size = 1.5) + 
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90), axis.title.x = element_blank()) +
  labs(y = "Elo Rating", title = "DDFFL Elo Rating History")
