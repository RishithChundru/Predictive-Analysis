x<-c(5.1,5.5,5.8,6.1,6.4,6.7,6.4,6.1,5.10,5.7)
y<-c(63,66,69,72,75,78,75,722,69,66)
plot(x,y)
relation<-lm(y~x)
abline(relation,col="red",lwd=2)
summary(relation)
summary(relation)$r.squared
predict(relation,data.frame(x=6.3))


x<-c(8,10,12)
y<-c(10,13,16)
plot(x,y)
relation<-lm(y~x)
abline(relation,col="red",lwd=2)
summary(relation)
predict(relation,data.frame(x=20))
