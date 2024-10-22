# predict petal length using sepal width

# Single linear regression
x<-iris
x
a<-data.frame(x)
plot(a$Sepal.Width,a$Petal.Length)
rel<-lm(Petal.Length~Sepal.Width,data=a)
summary(rel)$r.squared
abline(rel,col="red",lwd=2)
summary(rel)$r.squared
predict(rel,data.frame(Sepal.Width=3.2))


# Multiple linear regression
plot(a$Sepal.Width,a$Petal.Length)
rel<-lm(Petal.Length~Sepal.Length+Sepal.Width+Petal.Width,data=a)
summary(rel)$r.squared
abline(lm(Petal.Length~Sepal.Width,data=a),col="blue",lwd=2)
summary(rel)
predict(rel1,data.frame(Sepal.Length=4.5,Sepal.Width=3.2,Petal.Width=5.6))


#polynomial regression
library(ggplot2)
a$Slength=a$Sepal.Length^2
a$swidth=a$Sepal.Width^3
a$pwidth=a$Petal.Width^4
View(a)

plot(a$Sepal.Width,a$Petal.Length)
rel<-lm(Petal.Length~Slength+swidth+pwidth,data=a)
summary(rel)$r.squared
ggplot()+geom_point(aes(x=a$Sepal.Width,y=a$Petal.Length),colour="red")+geom_line(aes(x=a$Sepal.Width,y=predict(rel,newdata = a)),colour="blue")+ggtitle("Iris Data")+xlab("Sepal Width")+ylab("Petal Length")
predict(rel,data.frame(Slength=3.4^2,swidth=3.4^3,pwidth=3.4^4))



cat("R-squared (Linear):", summary(rel)$r.squared, "\n")
cat("R-squared (Multiple):", summary(rel1)$r.squared, "\n")
cat("R-squared (Polynomial):", summary(model2)$r.squared, "\n")

########
model2 <- lm(Petal.Length ~ poly(Sepal.Width, 2), data = a)
summary(model2)

summary(model)$r.squared
summary(model2)$r.squared

plot(a$Sepal.Width, a$Petal.Length)
lines(sort(a$Sepal.Width), predict(model2, data.frame(Sepal.Width = sort(a$Sepal.Width))), col = "blue",lwd=2)
