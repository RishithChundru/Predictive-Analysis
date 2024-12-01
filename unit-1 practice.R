data<-mtcars
a<-subset(data,data$mpg>15)
a
b<-ifelse(data$hp>150,"high","low")
b
summary(data)
dim(data)
colnames(data)
sum(is.na(data))
str(data)
sapply(data, class)
data$cyl<-factor(data$cyl)
data$cyl
unique(data$cyl)
summary(data$mpg)
hist(data$mpg,main="histogram",xlab="mpg")
boxplot(data$mpg,main="boxplot",col="orange")
table(data$cyl)
barplot(data$mpg,main="barplot",xlab="mpg",ylab="freq",col="green")
pie(table(data$cyl),main="piechart",col=rainbow(length(unique(data$cyl))))
plot(data$cyl,data$hp,main="scatterplot",xlab="cyl",ylab="hp",col="violet")
cor(data$hp,data$mpg)
boxplot(mpg~cyl,data=data,main="boxplot",xlab="xyl",ylab="mpg",col="skyblue")
table(data$cyl,data$gear)
mosaicplot(table(data$cyl, data$gear), main = "Cylinders vs Gears", col = c("skyblue", "pink", "yellow"))


# Dplyr
library(dplyr)
head(data)
a<-data %>% select(cyl,mpg,hp)
a
filtered_data<- data %>% filter(mpg>20)
filtered_data

b<-data %>% mutate(performance=ifelse(hp>150,"high","low"))
b
