a<-iris
str(a)
sample(a)
summary(a)
install.packages("e1071")
library(e1071)
split=Sample.Split(a$Species,SplitRatio=0.75)
training_set=subset()