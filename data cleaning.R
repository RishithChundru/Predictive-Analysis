a<-iris
View(iris)
diff(range(iris$Petal.Width))
quantile(iris$Petal.Width,seq(from=0,to=1,by=0.30))
table(iris$Sepal.Length)
b<-table(iris$Petal.Width)
prop.table(b)
