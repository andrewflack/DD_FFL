# Complete history of FFL Elo Ratings

library(ProjectTemplate)
load.project()

results_w_elo %>% 
  ggplot(aes(x = period, y = elo_n, color = owner)) + 
  facet_wrap(~owner) + 
  geom_line(size = 1) + 
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none") + 
  scale_x_continuous(breaks = c(0, 13, 26, 39, 52, 65, 78, 91), 
                     label = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")) + 
  labs(x = NULL, y = NULL, title = "DDFFL Elo Rating History")

ggsave("graphs/elo_history.png")
