## Logistic Regression

# Source: http://www.ats.ucla.edu/stat/r/dae/logit.htm
# Work through a relatively simple logistic regression model focused only on fantasy points at each position.

library(ProjectTemplate)
load.project()

### Get the data into shape - FFLwin, QBpts, RBpts, WRpts, DSTpts, TEpts, Kpts

lr_data <- scores.clean %>%
  select(year, teamID, week, name, owner, FFLpts, pos, player) %>%
  spread(pos, FFLpts) %>%
  select(year, teamID, week, QB, RB, WR, TE, K, DST) %>% 
  group_by(year, teamID, week) %>% 
  summarise_each(funs(sum(., na.rm = TRUE)))  %>% # this sums points per week by position (total of all 3 RBs started, for example)
  mutate(week_total = K + QB + RB + TE + WR + DST)

### Determine whether each row is a matchup win or loss
ddffl.weeklymatchups$year <- factor(ddffl.weeklymatchups$year)
ddffl.weeklymatchups$week <- factor(ddffl.weeklymatchups$week)
ddffl.weeklymatchups$teamID <- factor(ddffl.weeklymatchups$teamID)
lr_data$year <- factor(lr_data$year)
lr_data$week <- factor(lr_data$week)
lr_data$teamID <- factor(lr_data$teamID)
lr_data <- left_join(lr_data, ddffl.weeklymatchups, by = c("year", "week", "teamID"))
  
getFFLoppScore <- function(the_year, the_week, the_opp_teamID) { 
  FFLoppscore <- lr_data$week_total[lr_data$year == the_year & lr_data$week == the_week & lr_data$teamID == the_opp_teamID]
  FFLoppscore
}

lr_data$opp_week_total <- mapply(getFFLoppScore, lr_data$year, lr_data$week, lr_data$opp_teamID)

lr_data <- lr_data %>%
  mutate(won_matchup = ifelse(week_total > opp_week_total, 1, 0))

### Data is ready, now start working through the logistic regression.

mylogit <- glm(won_matchup ~ QB + RB + WR + TE + K + DST, data = lr_data, family = "binomial")

summary(mylogit)

# # Call:
# #   glm(formula = won_matchup ~ QB + RB + WR + TE + K + DST, family = "binomial", 
# #       data = lr_data)
# # 
# # Deviance Residuals: 
# #   Min       1Q   Median       3Q      Max  
# # -2.7152  -0.7422   0.0954   0.7859   2.3519  
# # 
# # Coefficients:
# #   Estimate Std. Error z value Pr(>|z|)    
# # (Intercept) -7.136805   0.672168 -10.618  < 2e-16 ***
# #   QB           0.073681   0.014209   5.186 2.15e-07 ***
# #   RB           0.078133   0.009356   8.351  < 2e-16 ***
# #   WR           0.069446   0.008225   8.443  < 2e-16 ***
# #   TE           0.082437   0.014386   5.730 1.00e-08 ***
# #   K            0.039055   0.023900   1.634    0.102    
# # DST          0.065009   0.015560   4.178 2.94e-05 ***
# #   ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 
# # (Dispersion parameter for binomial family taken to be 1)
# # 
# # Null deviance: 720.87  on 519  degrees of freedom
# # Residual deviance: 506.61  on 513  degrees of freedom
# # AIC: 520.61
# # 
# # Number of Fisher Scoring iterations: 5

# # For every 1 unit change in QB score, the log odds of winning the matchup increases by 0.074
# # For every 1 unit change in RB score, the log odds of winning the matchup increases by 0.078, etc.

# confint(mylogit)
# 
# # 2.5 %      97.5 %
# #   (Intercept) -8.51359344 -5.87418799
# # QB           0.04646608  0.10227926
# # RB           0.06043194  0.09716669
# # WR           0.05388017  0.08617462
# # TE           0.05498997  0.11156097
# # K           -0.00761173  0.08628013
# # DST          0.03501270  0.09614152

# Calculate the odds ratios and CIs
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# Look at some predicted probabilities - hold all positions at their mean and vary QB pts
QBpred <- with(lr_data, data.frame(RB = mean(RB), WR = mean(WR), TE = mean(TE), K = mean(K), DST = mean(DST), QB = seq(10, 40, by = 5)))

QBpred$rankP <- predict(mylogit, newdata = QBpred, type = "response")
# QBpred
# 
# # RB       WR       TE        K      DST QB     rankP
# # 1 32.00673 30.41346 4.557692 8.103846 7.821154 10 0.3574500
# # 2 32.00673 30.41346 4.557692 8.103846 7.821154 15 0.4457039
# # 3 32.00673 30.41346 4.557692 8.103846 7.821154 20 0.5375195
# # 4 32.00673 30.41346 4.557692 8.103846 7.821154 25 0.6268591
# # 5 32.00673 30.41346 4.557692 8.103846 7.821154 30 0.7083059
# # 6 32.00673 30.41346 4.557692 8.103846 7.821154 35 0.7782635
# # 7 32.00673 30.41346 4.557692 8.103846 7.821154 40 0.8353433

# Try varying RB and plot with a 95% CI
RBpred <- with(lr_data, data.frame(QB = mean(QB), WR = mean(WR), TE = mean(TE), K = mean(K), DST = mean(DST), RB = seq(0, 80, length.out = 40)))

RBpred2 <- cbind(RBpred, predict(mylogit, newdata = RBpred, type = "link", se = TRUE))

RBpred2 <- within(RBpred2, {PredictedProb <- plogis(fit)
                            LL <- plogis(fit - (1.96 * se.fit))
                            UL <- plogis(fit + (1.96 * se.fit))
                            })

ggplot(RBpred2, aes(x = RB, y = PredictedProb)) + geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + geom_line()

# Test whether the model with predictors fits significantly better than a model with just an intercept
# The test statistic is the difference between the residual deviance for the model with predictors and the null model (chi squared)

with(mylogit, null.deviance - deviance) # 214.2546

# The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the model

with(mylogit, df.null - df.residual) # 6

# find the p value for chi squared = 214.3 with 6 df

with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #1.746125e-43 - our model fits significantly better than an empty model



###### Create a new model that just uses weekly total and compare the two models

mylogit2 <- glm(won_matchup ~ week_total, data = lr_data, family = "binomial")
summary(mylogit2)
exp(cbind(OR = coef(mylogit2), confint(mylogit2)))
totalpred <- data.frame(week_total = seq(0,160,by = 1))
totalpred$prob <- predict(mylogit2, newdata = totalpred, type = "response")
ggplot(totalpred, aes(x = week_total, y = prob)) + geom_line()
