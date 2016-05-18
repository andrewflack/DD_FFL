# 11 FEB 2016
# Proposed Rule Change: Higher seed gets 5 point advantage for each round of the playoffs
# What should this advantage be? Is 5 points appropriate? 

library(ProjectTemplate)
load.project()

#### Option 1: Change a certain number of outcomes ####

# What percentage of matchups are decided by less than 5, less than 7.5, and less than 10 pts?

# is copy
results.sort <- t(apply(results[,c("year", "week", "owner", "opp_owner")], 1, sort))
results$is_copy <- ifelse(duplicated(results.sort) == TRUE, 1, 0)

results %>% 
  filter(is_copy == 0) %>% 
  mutate(diff = abs(week_total - opp_week_total)) %>% 
  mutate(under5 = ifelse(diff <= 5, 1, 0), 
         under7 = ifelse(diff <= 7.5, 1, 0), 
         under10 = ifelse(diff <= 10, 1, 0)) %>% 
  summarize(under5 = round(sum(under5)/length(under5),2), 
            under7 = round(sum(under7)/length(under7),2), 
            under10 = round(sum(under10)/length(under10),2))

#   under5 under7 under10
#    0.11   0.15    0.21

#### Option 2: Increase an owner's probability of winning a matchup ####

# Fit a logistic regression on total points using data from 2010-2015
mylogit2 <- glm(won_matchup ~ week_total, data = results, family = "binomial")

summary(mylogit2)

# Call:
#   glm(formula = won_matchup ~ week_total, family = "binomial", 
#       data = results)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6231  -0.7899  -0.2173   0.8407   2.3534  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -6.626723   0.563365  -11.76   <2e-16 ***
#   week_total   0.065372   0.005517   11.85   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 864.95  on 623  degrees of freedom
# Residual deviance: 631.28  on 622  degrees of freedom
# AIC: 635.28
# 
# Number of Fisher Scoring iterations: 5

# Log odds of winning matchup increases by 0.065 for every additional point scored

# Look at the odds ratio and confidence intervals
exp(cbind(OR = coef(mylogit2), confint(mylogit2)))

#                       OR        2.5 %      97.5 %
#     (Intercept) 0.001324497 0.0004198745 0.003833353
#     week_total  1.067555851 1.0565125711 1.079643425

# For every point scored, the odds of winning matchup increase by a factor of 1.07

# Make predictions on scores ranging from 0-200 and plot
totalpred <- data.frame(week_total = seq(0,200,by = 1))

totalpred$prob <- predict(mylogit2, newdata = totalpred, type = "response")

ggplot(totalpred, aes(x = week_total, y = prob)) + 
  geom_line() +
  theme_light() +
  xlab("Total Score") + 
  ylab("Probability of Winning Matchup") +
  ggtitle("Win Probability vs. Total Score")

# Create a new DF to look at advantages of varying levels
# Start at average score and increase in increments of 2.5 pts
home_adv <- data.frame(adv = c(0, 2.5, 5, 7.5, 10), 
                       week_total = c(round(mean(results$week_total), 1), 
                                      round(mean(results$week_total) + 2.5, 1), 
                                      round(mean(results$week_total) + 5, 1), 
                                      round(mean(results$week_total) + 7.5, 1),
                                      round(mean(results$week_total) + 10, 1)))

home_adv$win_prob <- round(predict(mylogit2, home_adv, type = "response"),3)
home_adv

#    adv    week_total win_prob
#    0.0        101     0.49
#    2.5        104     0.54
#    5.0        106     0.58
#    7.5        109     0.62
#   10.0        111     0.65