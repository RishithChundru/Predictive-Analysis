data<-mtcars
View(data)
a<-data.frame(data)
a
View(a)
plot(a)
relation<-lm(mpg~hp,data=a)
install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x = a$mpg, y = a$hp),
             colour = 'red') +
  geom_line(aes(x = a$mpg, y = predict(relation, newdata = a)),
            colour = 'blue') +
  ggtitle(' (mpg vs hp)') +
  xlab('mpg') +
  ylab('hp')
summary(relation)
predict(relation,data.frame(hp=100))
summary(relation)$r.squared


## 2=
library(ggplot2)   
library(e1071)    
data(diamonds)
str(diamonds)
diamonds_data <- diamonds[, c("carat", "depth", "table", "price", "cut")]
diamonds_data$cut <- as.factor(diamonds_data$cut)
diamonds_data$cut
sample_index <- sample(1:nrow(diamonds_data), 0.8 * nrow(diamonds_data))
train_data <- diamonds_data[sample_index, ]
test_data <- diamonds_data[-sample_index, ]
model <- naiveBayes(cut ~ carat + depth + table + price, data = train_data)
predictions <- predict(model, test_data)
print("Predictions for the test data:")
head(predictions)
confusion_matrix <- table(predictions, test_data$cut)
print("Confusion Matrix:")
print(confusion_matrix)
accuracy <- sum(predictions == test_data$cut) / nrow(test_data)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
new_diamond <- data.frame(carat = 0.23, depth = 61.5, table = 55, price = 500)
custom_prediction <- predict(model, new_diamond)
print(custom_prediction)



## 3
install.packages("e1071")
install.packages("rpart")

library(e1071)
library(rpart)
a<-data.frame(department=c('cse','mech','civil','eee','ece'),gender=c('male','female','male','female','male'),numappicants=c(100,30,10,67,34),admitted=c('yes','yes','no','yes','no'))
a
a$department <- as.factor(a$department)
a$gender <- as.factor(a$gender)
a$admitted <- as.factor(a$admitted)
sample_index <- sample(1:nrow(a), 0.8 * nrow(a))
train_data <- a[sample_index, ]
test_data <- a[-sample_index, ]
nb_model <- naiveBayes(admitted ~ gender + department, data = train_data)
nb_predictions <- predict(nb_model, test_data)
nb_conf_matrix <- table(nb_predictions, test_data$admitted)
print(nb_conf_matrix)
nb_accuracy <- sum(nb_predictions == test_data$admitted) / nrow(test_data)
print(paste("Naive Bayes Accuracy:", round(nb_accuracy * 100, 2), "%"))

dt_model <- rpart(admitted ~ gender + department, data = train_data, method = "class")
dt_predictions <- predict(dt_model, test_data, type = "class")
head(dt_predictions)
dt_conf_matrix <- table(dt_predictions, test_data$admitted)
print(dt_conf_matrix)

dt_accuracy <- sum(dt_predictions == test_data$admitted) / nrow(test_data)
print(paste("Decision Tree Accuracy:", round(dt_accuracy * 100, 2), "%"))
  new_applicant <- data.frame(gender = factor("male", levels = levels(a$gender)),
              department = factor("cse", levels = levels(a$department)))

nb_custom_prediction <- predict(nb_model, new_applicant)
print(paste( nb_custom_prediction))

dt_custom_prediction <- predict(dt_model, new_applicant, type = "class")
print(paste( dt_custom_prediction))
%>% 