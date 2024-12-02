data=iris
str(data)
summary(data)
library(caTools)
set.seed(123)
split = sample.split(data$Species, SplitRatio = 0.75) 
training_set = subset(data, split == TRUE) 
test_set = subset(data, split == FALSE) 
library(e1071)
?svm
classifier = svm(formula = Species ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear') 
#Linear Kernel is used when the data is Linearly separable, that is,
#it can be separated using a single Line.It is mostly used when there are a Large number of Features in a particular Data Set.
y_pred = predict(classifier, newdata = test_set[-5]) 

y_pred

cm = table(test_set[, 5], y_pred) 
cm
1 - sum(diag(cm)) / sum(cm)



plot(classifier, training_set, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
plot(classifier, test_set, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
#SVM plot visualizing the iris data. Support vectors are shown as ‘X’, true classesare highlighted through symbol color, predicted class regions are visualized using coloredbackground