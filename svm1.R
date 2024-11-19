# SVM: Support Vector Machine

# load the dataset
data<-read.csv("C://lpu//5th sem//INT234//Datasets//social.csv")
str(data)
data<-data[3:5]  # the first column is userid, which doesn't contribute for the classification
# encoding the target feature as a factor
data$Purchased<-factor(data$Purchased,levels = c(0,1))
# splitting the dataset into training and testing
library(caTools)
set.seed(123)
split<-sample.split(data$Purchased,SplitRatio = 0.75)
training_set<-subset(data,split==TRUE)
testing_Set<-subset(data,split==FALSE)
# feature scaling
# Scaling is used to change the data within the range of data
training_set[-3]<-scale(training_set[-3])  # not considering the 3rd column of the data for scaling because it is the target feature
testing_Set[-3]<-scale(testing_Set[-3])
library(e1071)
classifier<-svm(formula=Purchased~.,
                data=training_set,
                type='C-classification',
                kernel='linear'
                )
# kernel is used to specify the type of hyperplane to be plotted.
# Predicting the test set results
y_pred<-predict(classifier,newdata = testing_Set[-3])   # not considering the 3rd column because that is the feature needed to be predicted
cm<-table(testing_Set[,3],y_pred)
cm
print(paste("Accuracy of the model:",(sum(diag(cm))/sum(cm))*100))
