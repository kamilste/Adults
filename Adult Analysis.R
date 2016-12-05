#I'm setting working directory folder
setwd("E:/Level 5/CT5018 - Data Analytics/My project/Official Dataset - Adult")

#here I'm assigning adults to read the csv file
adults <- read.csv("Adults.csv", stringsAsFactors = FALSE)

#showing the structure of a data frame
str(adults)

#displays several common summary statistics (numeric values only)
summary(adults$Age)
summary(adults[c("Age", "Education.num", "Hours.per.week")])

#between the minimum and maximum value
range(adults$Age)

#maximum - minimum
diff(range(adults$Age))

#interquartile range (The difference between Q1 and Q3)
IQR(adults$Age)

#returns the five-number summary
quantile(adults$Age)

#1st and 99th percentiles
quantile(adults$Age, probs = c(0.01, 0.99))

#1% to 100% by 20%
quantile(adults$Age, seq(from = 0, to = 1, by = 0.20))

#Boxplots for the age and hours worked per week of adults dataset
boxplot(adults$Age, main = "Boxplot for Adults age", ylab="Age")
boxplot(adults$Hours.per.week, main = "Boxplot for hours worked per week", ylab = "Hours")

#histogram for same features as above
hist(adults$Age, main = "Histogram of adults age", xlab = "Age")
hist(adults$Hours.per.week, main = "Histogram for hours worked per week", xlab = "Hours")

#variance and standard deviation
var(adults$Age)
sd(adults$Age)
var(adults$Hours.per.week)
sd(adults$Hours.per.week)

#tables to explore not just a numeric data but also categorical
table(adults$Education.num)
table(adults$Education)
table(adults$Workclass)

#proportions of the table
relationship_table <- table(adults$Relationship)
prop.table(relationship_table)

#proportions by a procentage
race_table <- table(adults$Race)
race_pct <- prop.table(race_table) * 100
round(race_pct, digits = 1)

#Scatterplots
plot(x = adults$Age, y = adults$Hours.per.week,
     main = "Scatterplot of Hours per week vs Age",
     xlab = "Age",
     ylab = "Hours worked per week")
plot(x = adults$Age, y = adults$Capital.loss,
     main = "Scatterplot of Age vs Capital loss",
     xlab = "Age",
     ylab = "Capital loss")
plot(x = adults$fnlwgt, y = adults$Education.num,
     main = "scatter",
     xlab = "fnlwgt",
     ylab = "education number")

#installing package for cross tables
install.packages("gmodels")
library(gmodels)

#two-way-cross-tabulations
adults$conservative <- adults$Marital.Status %in% c("Married-civ-spouse", "Married-spouse-absent", 
                                                        "Married-AF-spouse")
table(adults$conservative)

#check against dependance on other variable
CrossTable(x = adults$Education, y = adults$conservative)

#this is how you drop a variable
# adults$Age <- NULL
