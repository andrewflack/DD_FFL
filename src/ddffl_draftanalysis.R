## draft analysis

library(ProjectTemplate)
load.project()

draft <- ddffl.draft.values[,-7]

draft %>% 
  group_by(owner) %>% 
  mutate(seasons = n_distinct(year)) %>% 
  filter(seasons>2) %>% 
  filter(draft_type == "initial") %>% 
  group_by(slot, owner) %>% 
  summarize(avg_sal = round(mean(salary),1)) %>% 
  ggplot(aes(x = factor(slot), y = avg_sal, fill = owner)) + 
  geom_bar(stat = "identity", position = "dodge")

draft %>% 
  group_by(owner) %>% 
  mutate(seasons = n_distinct(year)) %>% 
  filter(seasons>2) %>% 
  filter(draft_type == "initial") %>% 
  group_by(slot, owner) %>% 
  summarize(avg_sal = round(mean(salary),1)) %>% 
  ggplot(aes(x = factor(owner), y = avg_sal)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~slot)

initial_draft <- draft %>% filter(draft_type == "initial")
colnames(initial_draft) <- c("player","slot","salary","owner","year","draft_type")
  
seasonpts.df <- scores.clean %>% group_by(player, year, pos) %>% summarize(tot_pts = sum(FFLpts))

salary_pts <- inner_join(seasonpts.df, initial_draft, by = c("player","year"))

med_sal <- salary_pts %>% ungroup() %>% filter(pos %in% c("QB", "RB", "WR")) %>% summarize(median(salary)) %>% unlist() %>% unname()
med_pts <- salary_pts %>% ungroup() %>% filter(pos %in% c("QB", "RB", "WR")) %>% summarize(median(tot_pts)) %>% unlist() %>% unname()

salary_pts %>% 
  filter(pos %in% c("QB", "RB", "WR")) %>% 
  ggplot(aes(x=tot_pts,y=salary, color = pos, size = 10, alpha = .2)) + 
  geom_point() + 
  geom_vline(xintercept = med_pts) + 
  geom_hline(yintercept = med_sal) +
  geom_text(aes(label = ifelse((tot_pts > 200 & salary < 10),as.character(player),'')),hjust=0.25,vjust=1, show_guide = FALSE) +
  geom_text(aes(label = ifelse((tot_pts < 50 & salary > 55),as.character(player),'')),hjust=0.25,vjust=1, show_guide = FALSE) +
  guides(size = FALSE, alpha = FALSE) +
  xlab("Total Points") +
  ylab("Salary") +
  ggtitle("Initial Draft Salaries vs. Total Points") +
  theme_light()

salary_pts %>% 
  mutate(ppd = round(tot_pts/salary,1)) %>% 
  group_by(pos) %>% 
  summarize(avg_ppd = round(mean(ppd),1), med_ppd = round(median(ppd),1), sd_ppd = round(sd(ppd),1)) %>% 
  arrange(desc(avg_ppd))

salary_pts %>% 
  mutate(ppd = round(tot_pts/salary,1)) %>% 
  ggplot(aes(x=pos,y=ppd)) + 
  geom_boxplot() + 
  theme_light()

salary_pts %>%
  group_by(owner) %>%
  mutate(ppd = round(tot_pts/salary),1) %>%
  summarize(avg_ppd = round(mean(ppd),1)) %>%
  arrange(desc(avg_ppd))

## How the mighty have fallen - ordered dot plot of difference in initial draft value for players 
## who have been drafted for 5 years in a row
five_years <- salary_pts %>% 
  filter(pos %in% c("QB", "WR", "RB")) %>% 
  group_by(player) %>% 
  tally() %>% 
  filter(n == 5)

five_years_ago <- max(salary_pts$year) - 4

sal_oldest <- salary_pts %>% 
  ungroup() %>%
  filter(year == five_years_ago)  %>% 
  filter(pos %in% c("QB", "RB", "WR")) %>% 
  filter(player %in% unlist(five_years$player)) %>% 
  select(player, pos, sal_five_years_ago = salary)

sal_latest <- salary_pts %>% 
  ungroup() %>%
  filter(year == max(year))  %>% 
  filter(pos %in% c("QB", "RB", "WR")) %>% 
  filter(player %in% unlist(five_years$player)) %>% 
  select(player, pos, current_sal = salary)

df <- merge(sal_latest, sal_oldest) %>% 
  mutate(sal_diff = current_sal-sal_five_years_ago) 
  
df %>% ggplot(aes(x = sal_diff, y = factor(player, levels = df[order(df$sal_diff),"player"]))) + 
  xlab("5 Year Change in Salary") +
  ylab("Player") + 
  ggtitle("5 Year Change in Salary") +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major.y = element_blank()) +
  geom_segment(aes(yend=player), xend=0, color='grey50') +
  geom_point(size = 3)