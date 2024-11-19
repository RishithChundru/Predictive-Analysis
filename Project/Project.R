                      ### Naive Bayes ###
a <- read.csv(file.choose())
View(a)

# Change the targeted column to factor
a$test_preparation_course <- factor(a$test_preparation_course)

# Showing the levels in targeted column
factor(a$test_preparation_course)

# Splitting the data into testing and training data

indexes <- sample(1:nrow(a),0.7*nrow(a))

# Testing Data
testing <- a[indexes,]

# Training Data
training <- a[-indexes,]

install.packages("e1071")
library(e1071)

# Naive Bayes function
naivebayes <- naiveBayes(test_preparation_course ~ math_score + reading_score + writing_score, data=training)

# performing naivebayes predictions on testing data
naivebayes_predictions <- predict(naivebayes,testing)


# Confusion Matrix
confusion_matrix <- table(testing$test_preparation_course,naivebayes_predictions)

print(confusion_matrix)

# Finding out the Accuracy depending upon Confusion matrix

accuracy_naive=sum(diag(confusion_matrix))/sum(confusion_matrix) 
print(paste("Naive Bayes Accuracy : ", round(accuracy_naive*100,2),"%"))

# Storing it in the dataframe
df <- as.data.frame(confusion_matrix)
colnames(df) <- c('Actual','Predicted','Count')

install.packages("ggplot2")
library(ggplot2)

# Representing confusion matrix in a Heat Map
ggplot(df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +     
  ggtitle("Naive Bayes Confusion Matrix Heatmap") +
  xlab("Predicted Class") +
  ylab("Actual Class")



                    #### Decision Tree ####



install.packages("rpart")
library(rpart)


decision_tree <- rpart(test_preparation_course ~ math_score + reading_score + writing_score,data=training,method = "class")

# Decision tree predictions
decision_tree_predictions <- predict(decision_tree,testing,type="class")

# Confusion Matrix
confusion_matrix1 <- table(testing$test_preparation_course,decision_tree_predictions)

print(confusion_matrix1)

df <- as.data.frame(confusion_matrix1)
colnames(df) <- c('Actual','Predicted','Count')

install.packages("ggplot2")
library(ggplot2)

# Representing confusion matrix in a Heat Map
ggplot(df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +     
  ggtitle("Decision Tree Confusion Matrix Heatmap") +
  xlab("Predicted Class") +
  ylab("Actual Class")

# Accuracy of Decision tree model
accuracy_decision <- sum(diag(confusion_matrix1))/sum(confusion_matrix1)
print(paste("Decision Tree Accuracy : ", round(accuracy_decision*100,2),"%"))

install.packages("rpart.plot")
library(rpart.plot)

# Decision Tree
rpart.plot(decision_tree)


                  ### SVM ###

install.packages("caTools")
library(caTools)

a$test_preparation_course <- factor(a$test_preparation_course)

set.seed(123)
split <- sample.split(a$test_preparation_course, SplitRatio = 0.7)
training_set <- subset(a, split == TRUE)
testing_set <- subset(a, split == FALSE)

str(training_set)
# Feature scaling (excluding target variable)
numeric_cols <- sapply(training_set, is.numeric)
training_set[numeric_cols] <- scale(training_set[numeric_cols])
testing_set[numeric_cols] <- scale(testing_set[numeric_cols])

library(e1071)

# Build Svm classifier
classifier <- svm(
  formula = test_preparation_course ~ math_score + reading_score + writing_score,
  data = training_set,
  type = 'C-classification',
  kernel = 'linear'
)

# Predicting the testing set results
y_pred <- predict(classifier, newdata = testing_set[-5])

# Confusion Matrix
confusion_matrix_svm <- table(testing_set$test_preparation_course, y_pred)
print(confusion_matrix_svm)

# Calculate SVM Accuracy
accuracy_svm <- sum(diag(confusion_matrix_svm)) / sum(confusion_matrix_svm)
print(paste("SVM Accuracy : ", round(accuracy_svm * 100, 2), "%"))

df_svm <- as.data.frame(confusion_matrix_svm)
colnames(df_svm) <- c('Actual', 'Predicted', 'Count')

library(ggplot2)
ggplot(df_svm, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  ggtitle("SVM Confusion Matrix Heatmap") +
  xlab("Predicted Class") +
  ylab("Actual Class")


                      ### Neural Networks ###

# Load the dataset and normalize

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
numeric_cols1 <- sapply(a, is.numeric)
# Apply normalization only to numeric columns
a_norm <- as.data.frame(a) # Copy the original dataframe
a_norm[numeric_cols] <- lapply(a[numeric_cols], normalize)


# Splitting the dataset
train_idx <- 1:round(0.7 * nrow(a_norm))
ann_train <- a_norm[train_idx, ]
ann_test <- a_norm[-train_idx, ]

# Install and load the neuralnet package
install.packages("neuralnet")
library(neuralnet)

# Build Neural Network
neural_model <- neuralnet(test_preparation_course ~ math_score + reading_score + writing_score,
                          data = ann_train,
                          hidden = 5,
                          stepmax = 1e6, 
                          rep = 1)
plot(neural_model)

# Predicting results using the neural network model
prediction_nn <- predict(neural_model, ann_test)

# Confusion Matrix
confusion_matrix_nn <- table(ann_test$test_preparation_course, prediction_nn)
print(confusion_matrix_nn)

# Calculatin Accuracy
accuracy_nn <- sum(diag(confusion_matrix_nn)) / sum(confusion_matrix_nn)
print(paste("Neural Network Accuracy : ", round(accuracy_nn * 100, 2), "%"))

# Heatmap Visualization
df_nn <- as.data.frame(confusion_matrix_nn)
colnames(df_nn) <- c('Actual', 'Predicted', 'Count')

library(ggplot2)
ggplot(df_nn, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  ggtitle("Neural Network Confusion Matrix Heatmap") +
  xlab("Predicted Class") +
  ylab("Actual Class")




                      ### K-Means Clustering ###

install.packages("arules")
install.packages("cluster")
library(arules)
library(cluster)

a_1=a[,-5]

set.seed(240)
kmeans_result <- kmeans(a[, c("math_score", "reading_score", "writing_score")], centers = 3, nstart = 20)
kmeans_result

print(kmeans_result$cluster)
print(kmeans_result$centers)

cm<-table(a$test_preparation_course,kmeans_result$cluster)
cm

plot(a_1[c("reading_score","writing_score")],
     col=kmeans_result$cluster,
     main="K-Means Clustering of Math, Reading, and Writing Scores"
     )

# K-means Clustering
clusplot(a[, c("reading_score", "writing_score")],
         kmeans_result$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "K-Means Clustering of Students",
         xlab = "Reading Score",
         ylab = "Writing Score")

# Confusion Matrix
confusion_matrix_kmeans <- table(a$test_preparation_course, kmeans_result$cluster)
print(confusion_matrix_kmeans)

# Calculating Accuracy
accuracy_kmeans <- sum(diag(confusion_matrix_kmeans)) / sum(confusion_matrix_kmeans)
print(paste("K-Means Clustering Accuracy : ", round(accuracy_kmeans * 100, 2), "%"))

# Confusion matrix Visualization
df_kmeans <- as.data.frame(confusion_matrix_kmeans)
colnames(df_kmeans) <- c('Actual', 'Predicted', 'Count')

library(ggplot2)
ggplot(df_kmeans, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  ggtitle("K-Means Confusion Matrix Heatmap") +
  xlab("Predicted Class") +
  ylab("Actual Class")


### Accuracy of all models in bar plot Visualization ###


model_accuracies <- data.frame(
  Model = c("Naive Bayes", "Decision Tree", "SVM", "Neural Network", "K-Means"),
  Accuracy = c(accuracy_naive * 100, accuracy_decision * 100, accuracy_svm * 100, accuracy_nn * 100, accuracy_kmeans * 100)
)

# Bar Plot
ggplot(model_accuracies, aes(x = Model, y = Accuracy, fill = Model)) +geom_bar(stat = "identity") +
  geom_text(aes(label = round(Accuracy, 2)), vjust = -0.5, size = 5)+
  ggtitle("Model Accuracy Comparison") +
  xlab("Model") +
  ylab("Accuracy (%)") 
