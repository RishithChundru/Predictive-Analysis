install.packages("randomForest")
library(randomForest) 

data(iris)
set.seed(42)
sample_index<-sample(1:nrow(iris),0.7*nrow(iris))
train_data<-iris[sample_index,]
test_data<-iris[-sample_index,]

rf_model=randomForest(Species~.,data = train_data,ntree=100)
plot(rf_model)
predictions<-predict(rf_model,test_data)

confusion_matrix<-table(predictions,test_data$Species)
print(confusion_matrix)

accuracy<-sum(diag(confusion_matrix))/sum(confusion_matrix)
print(paste("Accuracy is: ",round(accuracy*100,2),"%"))
