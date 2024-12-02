# Sms_spam dataset
a<-read.csv(file.choose())
View(a)
str(a)
a$type=factor(a$type)
a$type
install.packages("tm")
library(tm)
sms_corpus=VCorpus(VectorSource(a$text))
print(sms_corpus)
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:10],as.character)
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
str(sms_dtm)
sms_dtm_train=sms_dtm[1:4181,]
sms_dtm_test=sms_dtm[4182:5574,]
sms_train_labels=a[1:4181,]$type
sms_test_labels=a[4182:5574,]$type
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


# Calculate accuracy by comparing predictions with actual test labels
accuracy <- sum(sms_test_pred == sms_test_labels) / length(sms_test_labels)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
