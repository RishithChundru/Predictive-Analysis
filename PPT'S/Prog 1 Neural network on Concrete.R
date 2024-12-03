getwd()
concrete = read.csv(file.choose())
View(concrete)
str(concrete)
#total 9 variables are there. one is 'strength' dependent on all the 
#other 8 variables.
#Neural networks work best when the input data are scaled to a 
#narrow range around zero, and here, we see values ranging anywhere from zero
#up to over a thousand. So normalize the data
hist(concrete$strength)
normalize <- function(x) {  return((x - min(x)) / (max(x) - min(x)))}
concrete_norm <- as.data.frame(lapply(concrete, normalize))

summary(concrete_norm$strength)
#Now all the values are in the range of zero and one

summary(concrete$strength)
#actual values are larger than the normalized values

concrete_train <- concrete_norm[1:773, ]
#75% training data

concrete_test <- concrete_norm[774:1030, ]
#25% testing data
install.packages("neuralnet")
library(neuralnet)
#intsall the package neuralnet for neural network implementation and 
#load it to the program

concrete_model <- neuralnet(strength ~ cement + slag + ash + water 
                            + superplasticizer
                            + coarseagg + fineagg + age,
                            data = concrete_train)
#training the simplest multilayer feedforward network with 
#only a single hidden node

plot(concrete_model)
#In this simple model, there is one input node for each of the eight features, followed by a single hidden node
#and a single output node that predicts the concrete strength.Lower errors means better prediction

model_results = compute(concrete_model,concrete_test[1:8])
model_results
#It returns a list with two components: $neurons, which stores the
#neurons for each layer in the network, and 
#$net.result, which stores the predicted values.
predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength)
#Correlations close to 1 indicate strong linear relationships between two variables.

concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water 
                             + superplasticizer + coarseagg + fineagg + age,
                           data = concrete_train, hidden = 5)
plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
