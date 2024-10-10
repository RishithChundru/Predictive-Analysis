id=c(1,2,3,4,5)
name=c('a','b','c','d','e')
marks=c(23,34,45,21,56)
df<-data.frame(id,name,marks)
df
install.packages("sqldf")
library(sqldf)
sqldf("select * from df")
df<-sqldf(c("insert into df values(10,'rishith',88)","select * from df"))
df
df<-sqldf(c("delete from df where id=3","select * from df"))
View(df)
df<-sqldf(c("update df set name='ankit' where id=1","select * from df"))
View(df)

## Create a dataframe emp having attributes empid,name,salary,dept
# display entire df
# insert 2 new entries in the existing df
# delete entry where dept is admin
# change dept of an emp where id=100

emp=data.frame(empid=c(1,2,100,4,5,6),
               name=c('a','b','c','d','e','f'),
               salary=c(100,200,300,400,500,600),
               dept=c('admin','manager','wager','worker','machinary','labour'))
emp
sqldf("select * from emp")
emp<-sqldf(c("insert into emp values(32,'w',900,'cooking')","select * from emp"))
View(emp)
emp<-sqldf(c("insert into emp values(44,'y',800,'washing')","select * from emp"))
emp
View(emp)
emp<-sqldf(c("delete from emp where dept='admin'","select * from emp"))
View(emp)
emp<-sqldf(c("update emp set dept='cleaning' where empid=100","select * from emp"))
View(emp)



# iris dataset
# display sepal width as sw from iris
# display max sepal width as max value 
# then insert into iris values
# apply some function on petal length as some pl
# display species of versacile only
data=iris
View(data)
sqldf("select `Sepal.Width` as sw from data")
sqldf("select max([Sepal.Width]) as maxvalue from data")
sqldf("select sum([Petal.Length]) as sumpl from data")
sqldf("select * from data where species='versicolor'")
