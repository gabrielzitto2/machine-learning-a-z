# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Building the optimal model using Backward Elimination
# regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
#                data = dataset)
# summary(regressor)
# # Optional Step: Remove State2 only (as opposed to removing State directly)
# # regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + factor(State, exclude = 2),
# #                data = dataset)
# # summary(regressor)
# regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
#                data = dataset)
# summary(regressor)
# regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
#                data = dataset)
# summary(regressor)
# regressor = lm(formula = Profit ~ R.D.Spend,
#                data = dataset)
# summary(regressor)

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(x)
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
newDataset = backwardElimination(dataset, SL)

set.seed(123)
split = sample.split(newDataset$Profit, SplitRatio = 0.8)
training_set = subset(newDataset, split == TRUE)
test_set = subset(newDataset, split == FALSE)

regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred2 = predict(regressor, newdata = test_set)

#predict em unico observation

single_observation = data.frame(R.D.Spend = 10000, Administration = 20000, 
                                Marketing.Spend = 30000, State = 1)

y_pred = predict(regressor, newdata = single_observation)
