data<-mtcars
View(data)
a<-data.frame(data)
a
View(a)
plot(a)
relation<-lm(mpg~hp,data=a)
install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x = a$mpg, y = a$hp),
             colour = 'red') +
  geom_line(aes(x = a$mpg, y = predict(relation, newdata = a)),
            colour = 'blue') +
  ggtitle(' (mpg vs hp)') +
  xlab('mpg') +
  ylab('hp')
summary(relation)
predict(relation,data.frame(hp=100))
summary(relation)$r.squared



## 2
b<-diamonds
View(b)
a<-data.frame(b)
a
plot(a)
relation()