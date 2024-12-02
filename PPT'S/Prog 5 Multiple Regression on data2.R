# Multiple Linear Regression 

# Importing the dataset 
getwd()
dataset = read.csv('data2.csv')
View(dataset)

# Encoding categorical data 
dataset$State = factor(dataset$State, 
                       levels = c('New York', 'California', 'Florida'), 
                       labels = c(1, 2, 3)) 
dataset$State 
View(dataset)
# Splitting the dataset into the Training set and Test set 
# install.packages('caTools') 
library(caTools) 
set.seed(123) 
split = sample.split(dataset$Profit, SplitRatio = 0.8) 
View(split)
training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE) 

# Feature Scaling 
# training_set = scale(training_set) 
# test_set = scale(test_set) 

# Fitting Multiple Linear Regression to the Training set 
regressor = lm(formula = Profit ~ ., 
               data = training_set) 
regressor

# Predicting the Test set results 
y_pred = predict(regressor, newdata = test_set) 
y_pred
