a <- read.csv(file.choose())
View(a)
b <- data.frame(a)
b
plot(b)
relation <- lm(charges ~ age + bmi + children, data = b)
abline(lm(charges ~ age, data = b), col = "blue", lwd = 2)
summary(relation)
predict(relation, data.frame(age = 20, bmi = 25, children = 1))
