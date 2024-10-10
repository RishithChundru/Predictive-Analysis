# Knn
setwd("C:/College PPTs/5th SEM/INT234")
library(readxl)
data=read_excel("winequality_red.xlsx")
View(data)
three_count<-sum(data$quality=="3")
four_count <- sum(data$quality == "4")
five_count <- sum(data$quality == "5")
six_count <- sum(data$quality == "6")
seven_count <- sum(data$quality == "7")
eight_count <- sum(data$quality == "8")
three_count
four_count
five_count
six_count
seven_count
eight_count
table(data$quality)
install.packages("dplyr")
library(dplyr)
mutating<- data %>%
  mutate(quality = recode(quality, "3"="three" ,"4" = "four", "5" ="five", "6"="six","7"="seven","8"="eight"))
mutating
View(mutating)
summary(data$density)
summary(data$pH)
summary(data$sulphates)

install.packages("gmodels")
library(gmodels)
normalize<-function(x){
  return ((x-min(x))/max(x)-min(x))
}
data$density <- normalize(data$density)
data$pH <- normalize(data$pH)
data$sulphates <- normalize(data$sulphates)
View(data)
library(class)
set.seed(123)

trainIndex <- sample(1: nrow(data),0.7*nrow(data))
trainIndex
train <- data[trainIndex,]
test <- data[-trainIndex,]
train
test
knn_pred <- knn(train[,-1],test[,-1],train$quality,k=162)

table(knn_pred,test$quality,dnn = c("prediction","actual"))
Accuracy<- sum(knn_pred==test$quality)/nrow(test)
Accuracy

# Naive Bayes
View(data)
str(data)
data$quality=factor(data$quality)
install.packages("tm")
library(tm)
sms_corpus=VCorpus(VectorSource(mutating))
print(sms_corpus)
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2],as.character)
sms_corpus_clean=tm_map(sms_corpus,content_transformer(tolower))
print(sms_corpus_clean)
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
sms_corpus_clean=tm_map(sms_corpus_clean,removeNumbers)
print(sms_corpus_clean)
as.character(sms_corpus_clean[[3]])
sms_corpus_clean=tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean=tm_map(sms_corpus_clean,removePunctuation)
install.packages("SnowballC")
library(SnowballC)
sms_corpus_clean=tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean=tm_map(sms_corpus_clean,stripWhitespace)
sms_dtm=DocumentTermMatrix(sms_corpus_clean)

sms_dtm_train=sms_dtm[1:6,]
sms_dtm_test=sms_dtm[7:12,]
sms_train_labels=a[1:6,]$mutating
sms_test_labels=a[7:12,]$mutating
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = FALSE )
sms_freq_words<-findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)
sms_dtm_freq_train=sms_dtm_train[ ,sms_freq_words]
sms_dtm_freq_test=sms_dtm_test[ ,sms_freq_words]

convert_counts<-function(x){
  x<-ifelse(x>0,"Yes","No")
}
sms_train=apply(sms_dtm_freq_train,MARGIN = 2,convert_counts)
sms_test=apply(sms_dtm_freq_test,MARGIN = 2,convert_counts)
install.packages("e1071")
library(e1071)
sms_classifier<-naiveBayes(sms_train,sms_train_labels)
sms_test_pred<-predict(sms_classifier,sms_test)


#Decision tree
install.packages("rpart")
library("rpart")
View(data)
str(data)
indexes=sample(162,110)
indexes
data_train=data[indexes,]
data_train
data_test=data[-indexes,]
data_test

target<-quality~chlorides+density+pH+sulphates
target

tree=rpart(target,data=data_train,method="class")
install.packages("rpart.plot")
library("rpart.plot")

predictions=predict(tree,data_test,type="class")
predictions
actual=data_test$quality
rpart.plot(tree)
