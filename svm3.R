# svm on boston
library(MASS)
data<-as.data.frame(Boston)
str(data)
View(data)
ncol(data)
data$medv<-as.integer(data$medv)
data <- data[order(data$medv), ]
View(data)
data$medv <- cut(data$medv, breaks = 3, labels = c("Low", "Medium", "High"))
data$medv<-factor(data$medv)
str(data)
library(caTools)
set.seed(42)
split<-sample.split(data$medv,SplitRatio = 0.70)
training_data<-subset(data,split==TRUE)
testing_data<-subset(data,split==FALSE)
training_data[-14]<-scale(training_data[-14])
testing_data[-14]<-scale(testing_data[-14])
library(e1071)
boston_classifier<-svm(medv~.,
                       data,
                       type='C-classification',
                       kernel='linear'
                       )
y_predict<-predict(boston_classifier,testing_data[-14])
cm1<-table(testing_data[,14],y_predict)
cm1
print(paste("Accuracy of the model:",(sum(diag(cm1))/sum(cm1))*100))