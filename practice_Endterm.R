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
tree<-rpart(target,data=a_test,method="class")
rpart.plot(tree)
predictions<-predict(tree,a_test,type="class")
conf_mat<-table(predict=predictions,actual=a_test$Species)
conf_mat
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
