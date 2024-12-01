setwd("C:/College PPTs/5th SEM/INT234")
getwd()
a<-read.csv(file.choose())
View(a)
sum(is.na(a))
b<-na.omit(a)
b
sum(is.na(b))
View(b)

# Remove duplicate rows
data <- a[!duplicated(a), ]
data

# Rename a column
colnames(a)[colnames(a) == "Order.Date"] <- "Date"
View(a)
# Remove leading/trailing white spaces in a specific column
a$Customer.Name <- trimws(a$Customer.Name)

# Convert text to lowercase
a$Ship.Mode <- tolower(a$Ship.Mode)

# Convert text to uppercase
a$Ship.Mode <- toupper(a$Ship.Mode)


View(a)
# Replacing the missing values with the mean value of each variable
a$Sales[is.na(a$Sales)]<-mean(a$Sales,na.rm=TRUE)
View(a)


# Replacing the missing values with random value between min and max of each variable
a$Sales[is.na(a$Sales)]<-runif(n=sum(is.na(a$Sales)),
                               min=min(a$Sales,na.rm=TRUE),
                               max=max(a$Sales,na.rm=TRUE))
View(a)
# Missing values for categorical variables by random value from each variable
# Convert to factor if not already
a$Order.Priority <- as.factor(a$Order.Priority)
a$Order.Priority
# Replace NA values with random levels from the factor
a$Order.Priority[is.na(a$Order.Priority)] <- sample(
  levels(a$Order.Priority),
  size = sum(is.na(a$Order.Priority)),
  replace = TRUE
)

# View the result
View(a)


a$Unit.Price<-(a$Unit.Price-min(a$Unit.Price))/(max(a$Unit.Price)-min(a$Unit.Price))
a$Unit.Price

a$Profit<-scale(a$Profit)
a$profit


a$Order.Priority<-factor(a$Order.Priority)
a$Order.Priority
