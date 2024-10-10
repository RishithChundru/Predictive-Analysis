install.packages("rpart")
library("rpart")
data("iris")
View(iris)
str(iris)

# Sample(150,110) would mean selecting 110 random
# elements from a population of 150 unique elements
indexes=sample(150,110)
indexes
iris_train=iris[indexes,]
iris_train
iris_test=iris[-indexes,]
iris_test

target<-Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
target
#The "class" method is used when the target variable is 
#categorical (e.g., predicting species of flowers).
tree=rpart(target,data=iris_train,method="class")
install.packages("rpart.plot")
library("rpart.plot")

predictions=predict(tree,iris_test,type="class")
predictions
actual=iris_test$Species
rpart.plot(tree)

# step-7: evaluate the model performance by creating a confusion matrix
confusion_matrix<-table(iris_test$Species,predictions)
print(confusion_matrix)

# step-8: calculate the accuracy of the model
accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)  # the formula used is present in 'caret' library. this calculates the sum of diagonal values present in the confusion matrix divided by sum of all values present in confusion matrix
print(paste("Accuracy: ",round(accuracy*100,4)))
