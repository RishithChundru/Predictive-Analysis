# Installing Packages
install.packages("arules")
install.packages("cluster")

library(arules)
library(cluster)

# Removing initial label of
# Species from original dataset
iris_1 <- iris[, -5]

# Fitting K-Means clustering Model
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(iris_1, centers = 3, nstart= 20)
#nstart means initial random number of centroids
#centers means no of clusters
kmeans.re

# Cluster identification for
# each observation
kmeans.re$cluster
kmeans.re$centers
# Confusion Matrix
cm <- table(iris$Species, kmeans.re$cluster)
cm

# Model Evaluation and visualization
#plot(iris_1[c("Sepal.Length", "Sepal.Width")])
#plot(iris_1[c("Sepal.Length", "Sepal.Width")],col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")],
     col = kmeans.re$cluster,
     main = "K-means with 3 clusters")

## Plotiing cluster centers
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]

# cex is font size, pch is symbol
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 1:3, pch = 8, cex = 3)

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')
#lines =0 no distance lines between the elipses will be there
#shade = TRUE means elipses are shaded in relation to their intensity
#color = TRUE means eplises colored wrt density
#labels = 2 all points and ellipses are labelled in the plot
#plotchar= TRUE, then the plotting symbols differ for points 
#belonging to different clusters.
#span = TRUE each cluster is represented by the ellipse with smallest
#area containing all its points.

