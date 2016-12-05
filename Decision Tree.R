#-----------------------------------------------------Data preparation----------------------------------------------------

#I'm setting working directory folder
setwd("F:/Level 5/CT5018 - Data Analytics/My project/Official Dataset - Adult")

#Start calculating the time
k <-Sys.time()

#here I'm assigning adults to read the csv file
adults <- read.csv("Adults.csv")

#examine the structure of the adultsTr data frame
str(adults)

#looking at characteristics of the applicant
table(adults$Native.Country)

#looking at two characteristics of the adults
summary(adults$Education.num)
summary(adults$Capital.loss)

#here I'm analysing the gender
table(adults$Sex)

#----------------------------------------setting data to be random--------------------------------

#create a random sample for training and test data
#use set.seed to use the same random number sequence
set.seed(23456)
adultsR <- adults[order(runif(20000)), ]

#compare the Age data frames
summary(adults$Age)
summary(adultsR$Age)

#check the first few values
head(adults$Age)
head(adultsR$Age)

#split the data frames
adultsTrain <- adultsR[1:18000, ]
adultsTest <- adultsR[18001:20000, ]

#check the proportion of class variable
prop.table(table(adultsTrain$Sex))
prop.table(table(adultsTest$Sex))

#------------------------------Starting Decision tree-------------------------

#Installin package
#install.packages("C50")
#library(C50)

#build the simplest decision tree
adultsM <- C5.0(adultsTrain[-10], adultsTrain$Sex)
adultsM
summary(adultsM)

#-----------------------------------Evaluating model performance------------------------------

# display simple facts about the tree
adultsP <- predict(adultsM, adultsTest)

#installing package
#install.packages("gmodels")
#library(gmodels)

#cross tabulation of predicted versus actual classes
CrossTable(adultsTest$Sex, adultsP, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Sex', 'predicted Sex'))

#--------------------------------Improving model performance-------------------------------------

#boosting the accuracy of decision trees
#boosted decision tree with 10 trials
#boosting the train data
adultsB10 <- C5.0(adultsTrain[-10], adultsTrain$Sex, trials = 10)
adultsB10
summary(adultsB10) 

#boosting the test data
adultsBP10 <- predict(adultsB10, adultsTest)
CrossTable(adultsTest$Sex, adultsBP10,
           prod.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#boosted decision tree with 100 trials 
adultsB100 <- C5.0(adultsTrain[-10], adultsTrain$Sex, trials = 10)
adultsB100
summary(adultsB100) 

#boosting the test data with 100 trials
adultsBP100 <- predict(adultsB100, adultsTest)
CrossTable(adultsTest$Sex, adultsBP100,
           prod.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#------------------------------------error cost-------------------------------------------------

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

adultsC <- C5.0(adultsTrain[-10], adultsTrain$Sex,
                costs = error_cost)
adultsCP <- predict(adultsC, adultsTest)
CrossTable(adultsTest$Sex, adultsCP,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#---------------------------------------Time and space---------------------------------------

l <-Sys.time()
l-k

install.packages('pryr')
#library(pryr)
mem_used()

#-------------------------------------------ZeroR---------------------------------

#installing package
install.packages("RWeka")
library(RWeka)

# train OneR() on the data
adults1R <- OneR(type ~ ., data = adults)
adults1R
summary(adults1R)

#Improving model performance
adultsJRip <- JRip(type ~ ., data = adults)
adultsJRip
summary(adultsJRip)
