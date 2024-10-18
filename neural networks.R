a<-read.csv(file.choose())
View(a)
summary(a)
hist(a$----)
normalize<-function(x){
  return (x-min(x))/max(x)-min(x)
}
a_norm<-as.data.frame(lapply(a,normalize))

a_train<-a_norm[1:773, ]
a_test<-a_norm[774:1030, ]

install.packages("neuralnet")
library(neuralnet)

a_model<-neuralnet(strength~cement+slag+asg+water+superplasticizer+coarse)