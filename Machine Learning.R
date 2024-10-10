#ML is divided into three categories
# 1. Supervised machine learning
# 2. Unsupervised ML
# 3. Reinforcement learning

# in supervised learning we have labels
# wisc_bc_data.csv
  
data <- read.csv(file.choose())
data
View(data)
data <- data[-1]

View(data)
m_count <- sum(data$diagnosis == "M")
b_count <- sum(data$diagnosis == "B")
m_count
b_count
table(data$diagnosis)
install.packages("dplyr")
library(dplyr)

mutating<- data %>%
  mutate(diagnosis = recode(diagnosis, "M" = "Malignant", "B" = "Benign"))

mutating
View(mutating)

data$diagnosis[data$diagnosis == "B"] = "Benign"
data$diagnosis[data$diagnosis == "M"] = "Malignant"

View(data)
table(data$diagnosis)
summary(data$radius_mean)
summary(data$area_mean)
summary(data$smoothness_mean)

install.packages("gmodels")
library(gmodels)

normalize<-function(x){
  return ((x-min(x))/max(x)-min(x))
}
data$radius_mean <- normalize(data$radius_mean)
data$area_mean <- normalize(data$area_mean)
data$smoothness_mean <- normalize(data$smoothness_mean)
View(data)
library(class)
set.seed(123)

trainIndex <- sample(1: nrow(data),0.7*nrow(data))
trainIndex
train <- data[trainIndex,]
test <- data[-trainIndex,]
train
test
knn_pred <- knn(train[,-1],test[,-1],train$diagnosis,k=100)

table(knn_pred,test$diagnosis,dnn = c("prediction","actual"))
Accuracy<- sum(knn_pred==test$diagnosis)/nrow(test)
Accuracy
