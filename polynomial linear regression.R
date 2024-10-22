a<-read.csv(file.choose()) # Position_salaries dataset
View(a)
# Visualising the Polynomial Regression results
# install.packages('ggplot2')
library(ggplot2)
a$Level2 <- a$Level^2
a$Level3 <- a$Level^3
a$Level4 <- a$Level^4
View(a)
# Fitting the polynomial regression model
poly_reg <- lm(Salary ~ Level + Level2 + Level3 + Level4, data = a)
ggplot() +
  geom_point(aes(x = a$Level, y = a$Salary),
             colour = 'red') +
  geom_line(aes(x = a$Level, y = predict(poly_reg, newdata = a)),
            colour = 'blue') +
  ggtitle(' (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))

