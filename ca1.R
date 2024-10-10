name<-c("Ram","Sai","Shyam","Nayak","Rishith","Sasank","Puneeth","Rahul","Gowtham","Subhash")
id<-c(1,2,3,4,5,6,7,8,9,12)
branch<-c("CSE","MECH","EEE","CSE","MCA","CSE","MECH","EEE","MBA","MCA")
marks<-c(95,34,57,91,89,67,55,43,78,77)
df<-data.frame(name,id,branch,marks)
df
library(sqldf)
a<-sqldf(c("select name from df where marks>90 and branch='CSE'"))
a
b<-sqldf(c("update df set name='Aarav Sharma' where id=12","select * from df"))
b
c<-sqldf(c("select avg(marks) from df"))
c
d<-sqldf(c("delete from df where name='Rahul'","select * from df"))
d


install.packages("ggplot2")
library(ggplot2)
a<-diamonds
View(a)
table(a$color)
a$depth[is.na(a$depth)]<-mean(a$depth,na.rm = TRUE)
View(a)
summary(a$depth)
summary(a$table)
summary(a$price)
install.packages("gmodels")
library(gmodels)
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
b<-normalize(a$price)
b
c<-normalize(a$x)
c
View(a)
d<-sample(1:nrow(a),0.7*nrow(a))
d
train<-a[d,]
test<-a[-d,]
train
test
knn_pred <- knn(train[,-1],test[,-1],train$x,k=100)

table(knn_pred,test$x,dnn = c("prediction","actual"))
Accuracy<- sum(knn_pred==test$x)/nrow(test)
Accuracy


a<-read.csv(file.choose())
a
View(a)
head(a)
b<-na.omit(a)
View(b)
a$Gender[a$Gender=="M"]="Male"
a$Gender[a$Gender=="F"]="Female"
a$Salary[a$Salary=="Fifty Thousand"]="55000"
a$Survey_Score[a$Survey_Score=="four"]="4"
View(a)
a$Gender<-as.factor(a$Gender)
a$Gender[is.na(a$Gender)] <- sample(
  levels(a$Gender),
  size = sum(is.na(a$Gender)),
  replace = TRUE
)
View(a)
a$Survey[is.na(a$Survey)]<-mean(a$Survey,na.rm=TRUE)
View(a)
