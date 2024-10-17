a<-data.frame(
size=c(1.4,2.6,1.0,3.7,5.5,3.2,3.0,4.9,6.3),
weight=c(0.9,1.8,2.4,3.5,3.9,4.4,5.1,5.6,6.3))
plot(a$weight,a$size)
#plot(mouse.data)
relation<-lm(size~weight,data=a)
abline(relation,col="red",lwd=2)
summary(relation)
predict(relation,data.frame(weight=4.1))

