# svm on iris dataset
data<-as.data.frame(iris)
#View(data)
str(data)
ncol(data)
library(caTools)
set.seed(42)
split<-sample.split(data$Species,SplitRatio = 0.70)
training_data<-subset(data,split==TRUE)
testing_data<-subset(data,split==FALSE)
#feature scaling
training_data[-5]<-scale(training_data[-5])
testing_data[-5]<-scale(testing_data[-5])
library(e1071)
iris_classifier<-svm(Species~.,
                     data=training_data,
                     type='C-classification',
                     kernel='linear'
                     )
y_predict<-predict(iris_classifier,testing_data[-5])
cm1<-table(testing_data[,5],y_predict)
cm1
print(paste("Accuracy of the model:",(sum(diag(cm1))/sum(cm1))*100))
