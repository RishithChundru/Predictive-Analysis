## dataset-> concrete_data.csv
a<-read.csv(file.choose())
View(a)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
a_norm <- as.data.frame(lapply(a, normalize))
concrete_test<-a_norm[1:773,] %>% 
concrete_train<-a_norm[774:1030,]


install.packages("neuralnet")
library(neuralnet)
concrete_model1<-neuralnet(strength~cement+slag+ash+water+superplasticizer+coarseagg+fineagg+age,data=concrete_train)
plot(concrete_model1)
model_results=compute(concrete_model1,concrete_test[1:8])

# it returns a list with two components: $neurons, which stores the neurons for each layer in the network, and $net, result, which stores the predicted values.
predicted_strength=model_results$net.result

cor(predicted_strength, concrete_test$strength)
# correlation close to 1 indicate strong linear relationships between two variables.

concrete_model2<-neuralnet(strength~cement+slag+ash+water+superplasticizer+coarseagg+fineagg+age,data=concrete_train,hidden=5)
plot(concrete_model2)

model_result<-compute(concrete_model2,concrete_test[1:8])

predicted_strength2<-model_result$net.result

cor(predicted_strength2,concrete_test$strength)
