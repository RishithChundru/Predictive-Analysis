install.packages("arules")
install.packages("cluster")
library(arules)
library(cluster)
iris_1=iris[,-5]
set.seed(240)
kmeans.re<- kmeans(iris_1,centers=2,nstart=20)
kmeans.re

kmeans.re$cluster
kmeans.re$centers

cm<-table(iris$Species,kmeans.re$cluster)
cm

plot(iris_1[c("Sepal.Length","Sepal.Width")],
     col=kmeans.re$cluster,
     main="K-means with 3 clusters")

kmeans.re$centers
kmeans.re$centers[,c("Sepal.Length","Sepal.Width")]

points(kmeans.re$centers[,c("Sepal.Length","Sepal.Width")],
       col=1:3,pch=8,cex=3)

y_kmeans<-kmeans.re$cluster
clusplot(iris_1[,c("Sepal.Length","Sepal.Width")],
         y_kmeans,
         lines=0,
         shade=TRUE,
         color=TRUE,
         labels=2,
         plotchar=FALSE,
         span=TRUE,
         main=paste("Cluster iris"),
         xlab = "Sepal.Length",
         ylab = "Sepal.Width")
