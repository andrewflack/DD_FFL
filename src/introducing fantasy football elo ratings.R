# Complete history of FFL Elo Ratings

# library(ProjectTemplate)
# load.project()

## ---- elo_history_plot
labels <- 2010:(params$current_season + 1)
breaks <- seq(0, (params$current_season + 1 - 2010)*weeks, by = weeks)

results_w_elo %>% 
  filter(year <= params$current_season) %>% 
  mutate(yrwk = paste0(year, week), period = CountDistinctAlong(yrwk)) %>% 
  ggplot(aes(x = period, y = elo_n, color = owner)) + 
  facet_wrap(~owner) + 
  geom_line(size = 1) + 
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none") + 
  scale_x_continuous(breaks = breaks, 
                     label = labels) + 
  labs(x = NULL, 
       y = NULL, 
       title = "Fantasy Football Elo Rating History",
       subtitle = paste0("Through the ", params$current_season, " season"))
## ---- end elo_history_plot

ggsave("graphs/elo_history.png")
