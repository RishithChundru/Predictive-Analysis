# Load necessary library
install.packages("randomForest")
library(randomForest)

# Load the iris dataset
data(iris)

# Set seed for reproducibility
set.seed(42)

# Split the data into training and testing sets
sample_index <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[sample_index, ]
test_data <- iris[-sample_index, ]

# Train the Random Forest model
rf_model <- randomForest(Species ~ ., data = train_data, ntree = 100)

# Predict on the test set
predictions <- predict(rf_model, test_data)

# Confusion matrix to evaluate the model
confusion_matrix <- table(predictions, test_data$Species)
print(confusion_matrix)

# Calculate the accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
