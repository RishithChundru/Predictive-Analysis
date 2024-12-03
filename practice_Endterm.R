## KNN 
## Wisc_data
a<-read.csv(file.choose())
View(a)

str(a)
sum(is.na(a))
na.omit(a)
b<-a[!duplicated(a),]
b
a<-a[-1]
a
a$diagnosis<-factor(a$diagnosis,levels=c("M","B"),labels=c("Male","Bale"))
a$diagnosis
table(a$diagnosis)
prop.table(table(a$diagnosis))
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
str(a)
ncol(a)
a_norm<-as.data.frame(lapply(a[2:31],normalize))
a_norm
a_scale<-scale(a_norm)
a_scale

library(class)
set.seed(123)
trainIndex<-sample(1:nrow(a_scale),0.7*nrow(a_scale))
trainIndex
a_train<-a_scale[trainIndex,]
a_test<-a_scale[-trainIndex,]

a_train_labels<-a[trainIndex,1]
a_test_labels<-a[-trainIndex,1]
knn_pred<-knn(train=a_train,test=a_test,cl=a_train_labels,k=10)
plot(knn_pred)

conf_mat<-table(predict=knn_pred,actual=a_test_labels)
conf_mat
accuracy<-sum(diag(conf_mat))/sum(conf_mat)
print(paste("Accuracy is: ",round(accuracy*100,2)," %"))


# Load the ggplot2 library
library(ggplot2)
# Create some sample data
data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 3, 5, 7, 11)
)

# Create a line graph
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  ggtitle("Line Graph Example") +
  xlab("X-axis Label") +
  ylab("Y-axis Label")

# Load the ggplot2 library
library(ggplot2)

# Create some sample data
data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(23, 45, 10, 32)
)

# Create a bar plot
ggplot(data, aes(x = category, y = value)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  ggtitle("Bar Plot Example") +
  xlab("Category") +
  ylab("Value")

#knn
a<-iris
sum(is.na(a))
a<-a[!duplicated(a),]
a

table(a$Species)

normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
str(a)
a_norm<-as.data.frame(lapply(a[1:4],normalize))
a_norm
a_scale<-scale(a_norm)
a_scale

library(class)
set.seed(123)
trainIndex<-sample(1:nrow(a),0.7*nrow(a))
a_train<-a_scale[trainIndex,]
a_test<-a_scale[-trainIndex,]
a_train_labels<-a[trainIndex,5]
a_test_labels<-a[-trainIndex,5]
knn_pred<-knn(a_train,a_test,cl=a_train_labels,k=10)
conf_mat<-table(predict=knn_pred,actual=a_test_labels)
conf_mat
accuracy<-sum(diag(conf_mat))/sum(conf_mat)
print(paste("Accuracy is: ",round(accuracy*100,digit=2)," %"))





## naive bayes
a<-iris
a<-a[!duplicated(a),]
na.omit(a)
a
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
a_norm<-as.data.frame(lapply(a[1:4],normalize))
a_norm
a_scale<-scale(a_norm)
a_scale

trainIndex<-sample(1:nrow(a_scale),0.7*nrow(a_scale))
a_train<-a[trainIndex,]
a_test<-a[-trainIndex,]
model<-naiveBayes(Species~.,data=a_train)
predictions<-predict(model,a_test)
conf_mat<-table(predict=predictions,actual=a_test$Species)
conf_mat

accuracy<-sum(diag(conf_mat))/sum(conf_mat)
print(paste("Accuracy is: ",round(accuracy*100,digits=2)," %"))


## Decision tree
library(rpart)
library(rpart.plot)
target<-Species~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
tree<-rpart(target,data=a_train,method="class")
rpart.plot(tree)
predictions<-predict(tree,a_test,type="class")
conf_mat<-table(predict=predictions,actual=a_test$Species)
conf_mat
save(conf_mat,file="confusion_mat.jpg")
getwd()
accuracy<-sum(diag(conf_mat))/sum(conf_mat)
print(paste("Accuracy is: ",round(accuracy*100,2)," %"))


## naive bayes
a<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(a)
a$type<-factor(a$type)
a$type

library(tm)
sms_corpus<-VCorpus(VectorSource(a$text))
sms_corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
as.character(sms_corpus_clean[[1]])
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
as.character(sms_corpus_clean[[3]])
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)

library(SnowballC)
sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
str(sms_dtm)
sms_dtm_train<-sms_dtm[1:4181, ]
sms_dtm_test<-sms_dtm[4182:5574, ]
sms_train_labels<-a[1:4181, ]$type
sms_test_labels<-a[4182:5574, ]$type

install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = FALSE )
sms_dtm_freq<-findFreqTerms(sms_dtm_train,5)
sms_dtm_freq_train<-sms_dtm_train[, sms_dtm_freq]
sms_dtm_freq_test<-sms_dtm_test[,sms_dtm_freq]

convert_count<-function(x){
  x<-ifelse(x>0,"yes","no")
}
sms_train<-apply(sms_dtm_freq_train,MARGIN=2,convert_count)
sms_test<-apply(sms_dtm_freq_test,MARGIN=2,convert_count)
library(e1071)
model<-naiveBayes(sms_train,sms_train_labels)
prediction<-predict(model,sms_test)                  
conf_mat<-table(predictions<-prediction,actual<-sms_test_labels)
conf_mat
accuracy<-sum(diag(conf_mat))/sum(conf_mat)
print(paste("Accuracy  : ",round(accuracy*100,2)," %"))


## linear Regression
a<-read.csv(file.choose())
str(a)
sum(is.na(a))
a<-a[!duplicated(a),]
a
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
a$charges<-normalize(a$charges)
a
a$charges<-scale(a$charges)
plot(a)
plot(a$charges,a$age)
relation<-lm(age~charges,data=a)
abline(relation,col="blue",lwd=4)
summary(relation)
summary(relation)$r.squared
predict(relation,data=a)


### Multiple Linear Regression
a<-read.csv(file.choose())
a
colnames(a)[colnames(a)=="Hours.Studied"]<-"hrs"
a
colnames(a)[colnames(a)=="Previous.Scores"]<-"pre-score"
colnames(a)[colnames(a)=="Extracurricular.Activities"]<-"act"
colnames(a)[colnames(a)=="Sleep.Hours"]<-"slp-hrs"
colnames(a)[colnames(a)=="Sample.Question.Papers.Practiced"]<-"practice"
colnames(a)[colnames(a)=="Performance.Index"]<-"performance"
a$act<-factor(a$act,levels=c("Yes","No"),labels = c(1,0))
a$act

library(caTools)
set.seed(123)
split=sample.split(a$performance,SplitRatio = 0.8)
a_train<-subset(a,split==TRUE)
a_test<-subset(a,split==FALSE)
model<-lm(formula = performance~.,data=a_train)
model
plot(a$hrs,a$performance)
abline(lm(performance~hrs,data=a),col="red",lwd=2)
summary(model)
summary(model)$r.squared
y_pred<-predict(model,newdata = a_test)
y_pred


## Multiple Linear Regression
a<-read.csv(file.choose())
a
a<-data.frame(a)
a
plot(a$age,a$bmi)
relation<-lm(age~.,data=a)
abline(lm(age~bmi,data=a),col="red",lwd=2)
summary(relation)
summary(relation)$r.squared
predict(relation,data.frame(sex="male",bmi=30.3,children=4,smoker="no",region="southwest",charges=1550.024))


### Polynomial Regression
a<-read.csv(file.choose())
a
library(ggplot2)
a$Level2<-a$Level^2
a$Level3<-a$Level^3
a$Level4<-a$Level^4
View(a)
plot(a$Level,a$Salary)
relation<-lm(Salary~Level+Level2+Level3+Level4,data=a)
ggplot()+
  geom_point(aes(x=a$Level,y=a$Salary),colour='red')+geom_line(aes(x=a$Level,y=predict(relation,newdata = a)),colour="blue")+ggtitle("Level vs Salary")+xlab("Level")+ylab("Salary")
predict(relation,data.frame(Level=6.5,
                            Level2=6.5^2,
                            Level3=6.5^3,
                            Level4=6.5^4))



## Ann
a<-read.csv(file.choose())
View(a)
str(a)
hist(a$strength)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
a_norm<-as.data.frame(lapply(a,normalize))
summary(a_norm)
trainIndex=sample(1:nrow(a),0.75*nrow(a))
concrete_train<-a_norm[trainIndex,]
concrete_test<-a_norm[-trainIndex,]
library(neuralnet)
model<-neuralnet(strength~.,data=concrete_train,hidden=5)
plot(model)
model_results<-compute(model,concrete_test[1:8])
model_results
predicted_strength<-model_results$net.result
predicted_strength
cor(predicted_strength,concrete_test$strength)



## SVM
a<-iris
str(a)
na.omit(a)
a<-a[!duplicated(a),]
a
library(caTools)
sample<-sample.split(a$Species,SplitRatio = 0.75)
a_train<-subset(a,sample==TRUE)
a_test<-subset(a,sample==FALSE)
library(e1071)
model<-svm(formula=Species~.,
           data=a_train,
           type='C-classification',
           kernel='linear')
y_pred<-predict(model,a_test[-5])
y_pred
cm<-table(y_pred,a_test[,5])
cm
accuracy<-sum(diag(cm))/sum(cm)
print(paste("Accuracy is: ",round(accuracy*100,2)," %"))
plot(model,a_train,Petal.Length~Petal.Width,
     slice=list(Sepal.Length=3,Sepal.Width=4))
plot(model,a_test,Petal.Length~Petal.Width,
     slice=list(Sepal.Length=3,Sepal.Width=4))


## Svm
library(MASS)
a<-Boston
View(a)
sum(is.na(a))
a<-a[!duplicated(a),]
a
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
str(a)
a_norm<-as.data.frame(lapply(a[1:14],normalize))
a_norm
library(caTools)
sample<-sample.split(a_norm,SplitRatio = 0.75)
a_train<-subset(a,sample==TRUE)
a_test<-subset(a,sample==FALSE)
a_train[-9]<-scale(a_train[-9])
a_test[-9]<-scale(a_test[-9])
library(e1071)
model<-svm(formula=rad~.,
           data=a_train,
           type='C-classification',
           kernel='linear')
model
pred<-predict(model,a_test[-9])
pred
cm<-table(pred,a_test[,9])
cm
accuracy<-sum(diag(cm))/sum(cm)
print(paste("Accuracy is : ",round(accuracy*100,2)," %"))


## K-Means Clustering
library(arules)
library(cluster)
a<-iris
a<-a[,-5]
a
set.seed(240)
model<-kmeans(a,centers=3,nstart=20)
model

model$cluster
model$centers

cm<-table(iris$Species,model$cluster)
cm

plot(a[c("Sepal.Length","Sepal.Width")],
     col=model$cluster,
     main="K means with 3 clusters")

model$centers
model$centers[,c("Sepal.Length","Sepal.Width")]
points(model$centers[,c("Sepal.Length","Sepal.Width")],
     col=1:3,pch=4,cex=4)
y_kmeans<-model$cluster
clusplot(a[,c("Sepal.Length","Sepal.Width")],
         y_kmeans,
         lines=0,
         shade=TRUE,
         color=TRUE,
         labels=2,
         plotchar=FALSE,
         span=TRUE,
         main=paste("K means Clust"),
         xlab="Sepal.Length",
         ylab="Sepal.Width"
         )

accuracy<-sum(diag(cm))/sum(cm)
print(paste("Accuracy is: ",round(accuracy*100,2)," %"))



## Hierarchial Clustering
a<-iris
a$Species<-NULL
a
d=dist(a,method="euclidean")
hfit=hclust(d,method="average")
plot(hfit)
grps=cutree(hfit,k=2)
grps
cm<-table(iris$Species,grps)
cm
accuracy<-sum(diag(cm))/sum(cm)
print(paste("Accuracy is: ",round(accuracy*100,2)," %"))
rect.hclust(hfit,k=2,border='red')


### Apriori Algo 
## Market Basket Analysis
library(arules)
setwd("C:/College PPTs/5th SEM/INT234")
a<-read.transactions("groceries.csv",sep=",")
summary(groceries)
inspect(groceries[1:5])
itemFrequency(groceries[,1:3])
itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=20)
apriori(groceries)
groceryrules<-apriori(groceries,parameter=list(support=0.006,confidence=0.25,minlen=2))
groceryrules
inspect(groceryrules[1:3])
inspect(sort(groceryrules,by="lift")[1:5])
berryrules<-subset(groceryrules,items%in% "berries")
inspect(berryrules[1:3])
groceryrules_df<-as(groceryrules,"data.frame")
str(groceryrules_df)


## Random Forest
install.packages("randomForest")
set.seed(42)
library(randomForest)
a<-iris
trainIndex<-sample(1:nrow(a),0.7*nrow(a))
a_train<-a[trainIndex,]
a_test<-a[-trainIndex,]
model<-randomForest(Species~.,data=a_train,ntree=100)
pred<-predict(model,a_test)
cm<-table(pred,a_test$Species)
cm
Accuracy<-sum(diag(cm))/sum(cm)
print(paste("Accuracy is : ",round(Accuracy*100,2)," %"))
sensitivity <- specificity <- precision <- f1_score <- c()
for (class in rownames(cm)) {
  TP <- cm[class, class]
  FN <- sum(cm[class, ]) - TP
  FP <- sum(cm[, class]) - TP
  TN <- sum(cm) - (TP + FP + FN)
  
  sensitivity[class] <- TP / (TP + FN)
  specificity[class] <- TN / (TN + FP)
  precision[class] <- TP / (TP + FP)
  f1_score[class] <- 2 * (precision[class] * sensitivity[class]) / (precision[class] + sensitivity[class])
}
for (class in rownames(cm)) {
  print(paste("For class", class, ":"))
  print(paste("Sensitivity:", round(sensitivity[class], 4)))
  print(paste("Specificity:", round(specificity[class], 4)))
  print(paste("Precision:", round(precision[class], 4)))
  print(paste("F1 Score:", round(f1_score[class], 4)))
}
# Assuming 'cm' is your confusion matrix

# Calculate TP, FN, FP, TN for all classes at once
TP <- diag(cm)
FN <- rowSums(cm) - TP
FP <- colSums(cm) - TP
TN <- sum(cm) - (TP + FP + FN)

# Calculate sensitivity, specificity, precision, and F1 score for all classes
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

# Print the results
results <- data.frame(
  Class = rownames(cm),
  Sensitivity = round(sensitivity, 4),
  Specificity = round(specificity, 4),
  Precision = round(precision, 4),
  F1_Score = round(f1_score, 4)
)

print(results)
