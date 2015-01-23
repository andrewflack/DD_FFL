### 
# Look at density curves for scoring by position for owners who have been in the league at least 3 seasons
scores.clean %>% 
  group_by(owner) %>% 
  mutate(seasons=n_distinct(year)) %>% 
  filter(seasons>2) %>% 
  ggplot(aes(x=FFLpts,colour=pos)) + 
  geom_density()

###
# Look at density curve for weekly totals
scores.clean %>% 
  group_by(owner,year,week) %>% 
  summarize(weeklytotal=sum(FFLpts)) %>% 
  ggplot(aes(x=weeklytotal)) + 
  geom_density()

# ...and weekly totals by owner
scores.clean %>% 
  group_by(owner,year,week) %>% 
  summarize(weeklytotal=sum(FFLpts)) %>% 
  ggplot(aes(x=weeklytotal,colour=owner)) + 
  geom_density()

### 
# Look at distribution of scoring by position for each owner who has been in the league at least 3 seasons

scores.clean %>%
  group_by(owner) %>%
  mutate(seasons=n_distinct(year)) %>%
  filter(seasons > 2) %>%
  ggplot(aes(x=reorder(owner,FFLpts,median),y=FFLpts)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ pos) +
  ggtitle("Point Distributions by Position") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

# associated tables...
# QB
scores.clean %>%
  group_by(owner) %>%
  mutate(seasons=n_distinct(year)) %>%
  filter(seasons > 2) %>%
  filter(pos=="QB") %>%
  summarize(avgQBpts=round(mean(FFLpts),1),medQBpts=round(median(FFLpts),1)) %>%
  arrange(desc(avgQBpts))

# RB
scores.clean %>%
  group_by(owner) %>%
  mutate(seasons=n_distinct(year)) %>%
  filter(seasons > 2) %>%
  filter(pos=="RB") %>%
  summarize(avgRBpts=round(mean(FFLpts),1),medRBpts=round(median(FFLpts),1)) %>%
  arrange(desc(avgRBpts))

# WR
scores.clean %>%
  group_by(owner) %>%
  mutate(seasons=n_distinct(year)) %>%
  filter(seasons > 2) %>%
  filter(pos=="WR") %>%
  summarize(avgWRpts=round(mean(FFLpts),1),medWRpts=round(median(FFLpts),1)) %>%
  arrange(desc(avgWRpts))

# TE
scores.clean %>%
  group_by(owner) %>%
  mutate(seasons=n_distinct(year)) %>%
  filter(seasons > 2) %>%
  filter(pos=="TE") %>%
  summarize(avgTEpts=round(mean(FFLpts),1),medTEpts=round(median(FFLpts),1)) %>%
  arrange(desc(avgTEpts))

# K
scores.clean %>%
  group_by(owner) %>%
  mutate(seasons=n_distinct(year)) %>%
  filter(seasons > 2) %>%
  filter(pos=="K") %>%
  summarize(avgKpts=round(mean(FFLpts),1),medKpts=round(median(FFLpts),1)) %>%
  arrange(desc(avgKpts))
