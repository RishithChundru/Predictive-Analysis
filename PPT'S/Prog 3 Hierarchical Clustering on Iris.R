iris
iris1=iris
iris1
iris1$Species=NULL
iris1
d = dist(iris1, method = "euclidean")
hfit = hclust(d, method = "average")
plot(hfit)
grps = cutree(hfit,k=4)
grps
rect.hclust(hfit,k=4, border = "red")
