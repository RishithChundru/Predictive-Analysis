
getwd()
insurance= read.csv(file.choose(), stringsAsFactors = TRUE)
str(insurance)
#Compactly display the internal structure
#model's dependent variable is charges, which measures the medical costs 
#each person charged to the 
#insurance plan for the year.

summary(insurance$charges)
#display the summary of charges column from the insurance table
#mean value is greater than the median, this implies 
#that the distribution of insurance charges is 
#right-skewed.

hist(insurance$age)
# computes a histogram of the given data values.

table(insurance$region)
#build a contingency table of the counts at each combination of factor levels.

cor(insurance[c("age", "bmi", "children", "charges")])
#cor is used to find the correlation between the columns of x and y
#regression model works on numeric data. rest of the columns are non numeric
#There is also a moderate positive correlation between age and charges, bmi 
#and charges,and children 
#and charges. These associations imply that as age, body mass, and 
#number of children increase, the 
#expected cost of insurance goes up.

pairs(insurance[c("age", "bmi", "children", "charges")])
#A matrix of scatterplots is produced.It is used to detect patterns among 
#three or more variables.
#The relationship between age and charges displays several relatively
#straight lines, while the bmi 
#versus charges plot has two distinct groups of points. It is difficult 
#to detect trends in any of the
#other plots.

install.packages("psych")
#install package so that pairs.panels can be used

library(psych)
#load the package

pairs.panels(insurance[c("age", "bmi", "children", "charges")])
#pairs.panels(insurance[c("age","bmi")])
#An enhanced scatterplot matrix can be created with the pairs.panels() function in the psych package.

ins_model <- lm(charges ~ age + children + bmi + sex +
                  smoker + region, data = insurance)
#fits a linear regression model relating the six independent variables 
#to the total medical charges.
#tilde character~ to describe the model; the dependent variable charges 
#goes to the left of the tilde
#while the independent variables go to the right, separated by + signs.
ins_pred <- predict(ins_model, data = insurance)

#same as above ins_model <- lm(charges ~ ., data = insurance)

ins_model
#to see the estimated beta coefficients
summary(ins_model)







insurance$age2 <- insurance$age^2
#the effect of age on medical expenditure may not be constant throughout all the age values; the 
#treatment may become disproportionately expensive for oldest populations.
#create another variable for age to separate the linear and non linear effect of age

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
#ifelse() function, which for each element in a vector tests a specified condition and returns a value
#depending on whether the condition is true or false. For BMI greater than or equal to 30, we will 
#return 1, otherwise 0

ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)
#train the model using the lm() function as before, but this time we'll add the newly constructed 
#variables and the interaction term

summary(ins_model2)
