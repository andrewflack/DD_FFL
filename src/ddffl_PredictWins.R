# This script runs through development of a model for predicting wins 
# Topics addressed:
# - Various classification techniques (Decision Trees, Random Forests, Logistic Regression, K-Nearest Neighbors)
# - Model validation (including cross-validation)
# - Improving Models (feature engineering)

library(ProjectTemplate)
load.project()

# prepare the data for use in models

weekly_data <- scores.clean %>%
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
weekly_data$year <- factor(weekly_data$year)
weekly_data$week <- factor(weekly_data$week)
weekly_data$teamID <- factor(weekly_data$teamID)
weekly_data <- left_join(weekly_data, ddffl.weeklymatchups, by = c("year", "week", "teamID"))

getFFLoppScore <- function(the_year, the_week, the_opp_teamID) { 
  FFLoppscore <- weekly_data$week_total[weekly_data$year == the_year & weekly_data$week == the_week & weekly_data$teamID == the_opp_teamID]
  FFLoppscore
}

weekly_data$opp_week_total <- mapply(getFFLoppScore, weekly_data$year, weekly_data$week, weekly_data$opp_teamID)

weekly_data <- weekly_data %>%
  mutate(won_matchup = ifelse(week_total > opp_week_total, 1, 0))

weekly_data <- weekly_data[,c("QB","RB","WR","TE","K","DST","week_total","opp_week_total","won_matchup")]

# split data into training and validation sets

set.seed(1234)

sample_indices <- sample(1:nrow(weekly_data), size=0.2*nrow(weekly_data))

weekly_test <- weekly_data[sample_indices,]
dim(weekly_test) 
weekly_train <- weekly_data[-sample_indices,]
dim(weekly_train) 

### MODEL 1: K-Nearest Neighbors

# start with k=3
class.k3 <- knn(train = weekly_train[,c("QB","RB","WR","TE","K","DST","week_total")],
               test = weekly_test[,c("QB","RB","WR","TE","K","DST","week_total")],
               cl = factor(weekly_train$won_matchup),
               k = 3,
               use.all = FALSE)

table(predicted = class.k3, actual = weekly_test$won_matchup) #look at confusion matrix
#           actual
# predicted  0  1
#         0 37 16
#         1 13 38
# accuracy = .97

acc_k3 <- sum(class.k3 == weekly_test$won_matchup)/length(class.k3)

summary(class.k3 == weekly_test$won_matchup)
# Mode        FALSE  TRUE     NA's 
# logical      29     75       0 

# try different values for k

base_error_rate <- sum(weekly_train$won_matchup)/length(weekly_train$won_matchup)

knn_different_k <- sapply(seq(1,27,by = 2),
                         function(x){
                           class.k = knn(train = weekly_train[,c("QB","RB","WR","TE","K","DST","week_total")],
                                         test = weekly_test[,c("QB","RB","WR","TE","K","DST","week_total")],
                                         cl = factor(weekly_train$won_matchup),
                                         k = x,
                                         use.all = FALSE)
                           num_correct <- summary(class.k == weekly_test$won_matchup)
                           num_correct <- as.numeric(num_correct[3])
                           cbind(k = x,
                                 num_correct = num_correct,
                                 percent_accurate = num_correct/length(weekly_test$won_matchup),
                                 increase_from_base = (((num_correct/length(weekly_test$won_matchup))-base_error_rate)/base_error_rate)*100)})

# Transposing output to go from rows to columns
knn_different_k <- as.data.frame(t(knn_different_k))

colnames(knn_different_k) = c("k",
                              "num_correct",
                              "percent_accuracy",
                              "increase_from_base")

qplot(data = knn_different_k, x = k, y = percent_accuracy) + geom_line()

# see that k=7 improves model
class.k7 <- knn(train = weekly_train[,c("QB","RB","WR","TE","K","DST","week_total")],
                test = weekly_test[,c("QB","RB","WR","TE","K","DST","week_total")],
                cl = factor(weekly_train$won_matchup),
                k = 7,
                use.all = FALSE)

table(predicted = class.k7, actual = weekly_test$won_matchup) #look at confusion matrix

acc_k7 <- sum(class.k7 == weekly_test$won_matchup)/length(class.k7)

### MODEL 2: Classification Tree

library(rpart)

set.seed(1234)

fit_tree <- rpart(won_matchup ~ QB + RB + WR + TE + K + DST + week_total,  
                 method = "class",	
                 parms = list(split = "importance"), 
                 data = weekly_train)

printcp(fit_tree)

# Classification tree:
#   rpart(formula = won_matchup ~ QB + RB + WR + TE + K + DST + week_total, 
#         data = weekly_train, method = "class", parms = list(split = "importance"))
# 
# Variables actually used in tree construction:
#   [1] K          QB         week_total WR        
# 
# Root node error: 207/416 = 0.4976
# 
# n= 416 
# 
# CP nsplit rel error  xerror     xstd
# 1 0.507246      0   1.00000 1.07729 0.049138
# 2 0.011272      1   0.49275 0.54589 0.043827
# 3 0.010000      7   0.42029 0.61836 0.045476

plot(fit_tree, uniform = TRUE, main = "Won Matchup")
text(fit_tree, use.n = TRUE, all = TRUE)

tree_predict <- predict(fit_tree,weekly_test[,c("QB","RB","WR","TE","K","DST","week_total")], type = "class")

table(tree_predict, weekly_test$won_matchup)
# tree_predict  0  1
#             0 41 18
#             1  9 36

acc_tree <- sum(tree_predict == weekly_test$won_matchup)/length(tree_predict)

### MODEL 3: Random Forests

library(randomForest)

# create a copy of the weekly data to use specifically for the RF model
rf_train <- weekly_train
rf_test <- weekly_test

rf_train$won_matchup <- as.factor(rf_train$won_matchup)
rf_test$won_matchup <- as.factor(rf_test$won_matchup)

# train the model
fit_rf_train <- randomForest(won_matchup ~ QB + RB + WR + TE + K + DST + week_total,
                            data = rf_train,
                            ntree = 500,
                            nodesize = 5,
                            mtry = 5,
                            importance = TRUE)

# predict using rf model

# (get predicted classes)
rf_predict <- predict(fit_rf_train,rf_test[,c("QB","RB","WR","TE","K","DST","week_total","opp_week_total")])
# (or get probabilities)
rf_probs <- predict(fit_rf_train,rf_test[,c("QB","RB","WR","TE","K","DST","week_total","opp_week_total")], "prob")
rf_probwin <- rf_probs[,2]

table(rf_predict, as.factor(rf_test$won_matchup))
# rf_predict  0  1
#           0 39 11
#           1 11 43

acc_rfpredict <- sum(rf_predict == rf_test$won_matchup)/length(rf_predict)

varImpPlot(fit_rf_train, type = 1)

### MODEL 4: Logistic Regression

my_logit <- glm(won_matchup ~ QB + RB + WR + TE + K + DST, data = weekly_train, family = "binomial")

summary(my_logit)

# Call:
#   glm(formula = won_matchup ~ QB + RB + WR + TE + K + DST, family = "binomial", 
#       data = weekly_train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6585  -0.8047  -0.1631   0.8095   2.3445  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -6.808631   0.727455  -9.360  < 2e-16 ***
#   QB           0.070150   0.015071   4.655 3.24e-06 ***
#   RB           0.072339   0.010102   7.161 8.02e-13 ***
#   WR           0.067905   0.009219   7.366 1.76e-13 ***
#   TE           0.070100   0.015742   4.453 8.47e-06 ***
#   K            0.038595   0.027151   1.421    0.155    
#   DST          0.066083   0.016822   3.928 8.55e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 576.69  on 415  degrees of freedom
# Residual deviance: 418.24  on 409  degrees of freedom
# AIC: 432.24
# 
# Number of Fisher Scoring iterations: 5

# Calculate the odds ratios and CIs
exp(cbind(OR = coef(my_logit), confint(my_logit)))

# Produce probabilities using predict(), then classify by rounding
lr_predict <- predict(my_logit, newdata = weekly_test[,-9], type = "response")
lr_class <- round(lr_predict)

table(predicted = lr_class, actual = weekly_test$won_matchup)

#           actual
# predicted  0  1
#         0 38 11
#         1 12 43

acc_mylogit <- sum(lr_class==weekly_test[,"won_matchup"])/length(lr_class)

### Compare Models



### After selecting a model, try to engineer some features

# add win percentage at time of matchup
weekly_data %>% group_by(year, teamID, winpct = cumsum(won_matchup)/as.numeric(week)) 
# this isn't going to work because I stripped year, week, and teamID from weekly_data
# also, need to lag by 1 week to get win pct going into the game. as calculated currently, winpct includes current matchup result