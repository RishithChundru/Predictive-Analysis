library(ggplot2)

# Scatter plot of mpg vs wt in mtcars
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "Scatter Plot", x = "Weight", y = "Miles Per Gallon")

# Line chart of horsepower vs weight in mtcars
ggplot(mtcars, aes(x = wt, y = hp)) +
  geom_line() +
  labs(title = "Line Chart", x = "Weight", y = "Horsepower")

# Bar chart of cylinder counts in mtcars
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar() +
  labs(title = "Bar Chart", x = "Cylinders", y = "Count")

# Histogram of mpg in mtcars
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram", x = "Miles Per Gallon", y = "Frequency")


# Density plot of mpg in mtcars
ggplot(mtcars, aes(x = mpg)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot", x = "Miles Per Gallon", y = "Density")


# Boxplot of mpg grouped by cylinders in mtcars
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  labs(title = "Boxplot", x = "Cylinders", y = "Miles Per Gallon")

# Violin plot of mpg grouped by cylinders in mtcars
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_violin(fill = "blue") +
  labs(title = "Violin Plot", x = "Cylinders", y = "Miles Per Gallon")

# Area chart of horsepower vs weight in mtcars
ggplot(mtcars, aes(x = wt, y = hp)) +
  geom_area(fill = "blue", alpha = 0.5) +
  labs(title = "Area Chart", x = "Weight", y = "Horsepower")

# Heatmap of correlation matrix in mtcars
library(reshape2)
cor_data <- melt(cor(mtcars))
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Heatmap", x = "", y = "")

# Pie chart of cylinder counts in mtcars
cyl_data <- as.data.frame(table(mtcars$cyl))
ggplot(cyl_data, aes(x = "", y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Pie Chart", fill = "Cylinders")

# Bubble chart of mpg vs wt, size by hp in mtcars
ggplot(mtcars, aes(x = wt, y = mpg, size = hp, color = factor(cyl))) +
  geom_point(alpha = 0.5) +
  labs(title = "Bubble Chart", x = "Weight", y = "Miles Per Gallon", size = "Horsepower")
