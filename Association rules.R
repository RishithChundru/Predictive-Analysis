install.packages("arules")
library(arules)
library(tm)
getwd()
setwd("C:/College PPTs/5th SEM/INT234")
groceries <- read.transactions("groceries.csv", sep = ",")
#it results in a sparse matrix suitable for transactional data.

summary(groceries)
#details about no of transactions, no of items, density of non zero,

inspect(groceries[1:5])
#provide the detail of first 5 transactions

itemFrequency(groceries[, 1:3])
#allows us to see the proportion of transactions that contain the
#item and to view the support level for
#the first three items in the grocery data

itemFrequencyPlot(groceries, support = 0.1)

#plot the bar chart using atleast 10% of support

itemFrequencyPlot(groceries, topN = 20)
#plot with 20 items

#image(groceries[1:5])
#visualize the sparse matrix including 5 transactions and 169 items.
#cell will be black where transaction is done


#image(sample(groceries, 100))
#combining it with the sample() function, you can view the sparse matrix
#for a randomly sampled set of transactions.


apriori(groceries)
#by default support is 0.1 and confidence is 0.8


groceryrules <- apriori(groceries, parameter =
                          list(support =
                                 0.006, confidence = 0.25, minlen = 2))
#for support, assuming one item is getting purchased 2 times a day,means 60 times
#a month. it means 60/9835 equals 0.006.
#for confidence, consider a rule moving the smoke detectors closer to the
#batteries increase sale

groceryrules
#463 rules created
#summary(groceryrules)
#if lift more than 1, it means that the two items are found
#together more often than one would expect
#by chance. Lift of greater than 1 means products A and B are more likely to be bought together.


inspect(groceryrules[1:3])

inspect(sort(groceryrules, by = "lift")[1:5])
#sort the best 5 rules from all the rules


berryrules <- subset(groceryrules, items %in% "berries")
#filter out all the rules having berries

inspect(berryrules)

write(groceryrules, file = "groceryrules.csv",
      sep = ",", row.names = FALSE)
#write rules to the csv file


groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
