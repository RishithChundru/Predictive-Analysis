# find out the location of current working directory .define any three variables save those variables in name 'abc' into some other directory. list a all the variables and remove the first three variables.open a csv file and store one dataframe consisting of records of three students in the file. then display file using 

getwd()
data<-data.frame(Name=c("Sai","Ram","Shyam"),
              age=c(12,13,15))
data
save(data,file="abc.csv")
setwd("C:/College PPTs/5th SEM/INT234")
write.csv(data,file="abc.csv",row.names=FALSE)
ls()
rm(a,c)
ls()
write.csv(data,file="abc.csv",row.names=FALSE)
mydata<-read.csv("abc.csv")
View(mydata)
