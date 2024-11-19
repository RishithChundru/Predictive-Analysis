a=pressure
View(a)
d=dist(a,method="euclidean")
hfit=hclust(d,method="average") ## single,average,complete
plot(hfit)
grps=cutree(hfit,k=2)
grps
rect.hclust(hfit,k=2,border='red')
