#-----------------------------------------------------Data preparation----------------------------------------------------

#I'm setting working directory folder
setwd("H:/Level 5/CT5018 - Data Analytics/My project/Official Dataset - Adult")

#start calculating the time to run the code
k <-Sys.time()

#here I'm assigning adults to read the csv file
adults <- read.csv("Adults.csv", stringsAsFactors = FALSE)

#examine the structure of the adultsTr data frame
str(adults)

#drop the fnlwgt feature
adults <- adults[-3]

#table of gende
table(adults$Sex)

#recode Sex as a factor
adults$Sex <- factor(adults$Sex, levels = c("Female","Male"),
                       labels = c("Women", "Men"))

#table or proportions with more informative labels
round(prop.table(table(adults$Sex)) * 100, digits = 1)

#summarize all numeric features
summary(adults[c("Age", "Education.num", "Capital.gain", "Capital.loss", "Hours.per.week")])

#----------------------------------------------Min-Max normalisation-----------------------------------------------------------

#create normalization function
normalize <- function(x) {
  return ((x - min (x)) / (max(x) - min(x)))
}

#test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

adults$Workclass <- NULL
adults$fnlwgt <- NULL
adults$Education <- NULL
adults$Marital.Status <- NULL
adults$Occupation <- NULL
adults$Relationship <- NULL
adults$Race <- NULL
adults$Sex <- NULL
adults$Capital.gain <- NULL
adults$Native.Country <- NULL


#normalize the adultsTr data
adultsN <- as.data.frame(lapply(adults[2:3], normalize))

#confirm that normalization worked
summary(adultsN$Age)

# create training and test data
adultsTrain <- adultsN[1:14999, ]
adultsTest <- adultsN[15000:19999, ]

# create labels for training and test data
adultsTrainLabels <- adults[1:14999, 1]
adultsTestLabels <- adults[15000:19999, 1]

#instaling package class
#install.packages("class")
#library(class)

adultsTestPred <- knn(train = adultsTrain, test = adultsTest,
                      cl = adultsTrainLabels, k=122)

#knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

#installing package for cross tables
#install.packages("gmodels")
#library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = adultsTestLabels, y = adultsTestPred,
           prop.chisq=FALSE)

#install.packages("descr")
#library(descr)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = adultsTestLabels, y = adultsTestPred,
           prop.chisq=FALSE)




#------------------------------------trying first method---------------------------------

library(class)

adultsTestPred <- knn(train=adultsTrain, test = adultsTest,
                      cl = adultsTrainLabels, k=122)

#installing package for cross tables
#install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = adultsTestLabels, y = adultsTestPred,
           prop.chisq=FALSE)



#-----------------------------------------------z-score standardization---------------------------------------------

#use the scale() function to z-score standardize a data frame
adultsZ <- as.data.frame(scale(adults[c("Age", "Education.num", "Capital.gain", "Capital.loss", "Hours.per.week")]))

#confirm that the transformation was applied correctly
summary(adultsZ$Education.num)

# create training and test data
adultsTrain <- adultsZ[1:14999, ]
adultsTest <- adultsZ[15000:19999, ]

# create labels for training and test data
adultsTrainLabels <- adults[1:14999, 1]
adultsTestLabels <- adults[15000:19999, 1]

# re-classify test cases
adultsTestPred <- knn(train = adultsTrain, test = adultsTest,
                      cl = adultsTrainLabels, k=122)

#Create the cross tabulation of predicted vs. actual
CrossTable(x = adultsTestLabels, y = adultsTestPred,
           prop.chisq=FALSE)

#-------------------------------Time and space--------------------------------------------------------------

#end calculating the time and show the output
l <-Sys.time()
l-k

#check how big is the file
install.packages('pryr')
library(pryr)
mem_used()

#--------------------------------------cross-validation----------------------------------------

folds <- createFolds(adults$Sex, k = 10)
str(folds)
