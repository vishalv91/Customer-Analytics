#Load the required packages.
library(randomForest)#random forest function
library(caret)#confusion matrix

#Set working directory where dataset is located.
setwd("C:/Users/Family/Desktop/Term3project_IMS")

#Import the dataset.
data <- read.csv("train.csv")

#create a copy of the dataset.
data1 <- data

#remove ID column.
data1$ID <- NULL

#rename variables.
names(data1) <- c("warehouseblock", "modeofshipment", "customercarecalls", "customerrating", "costoftheproduct", "priorpurchase", "productimportance", "gender", "discountoffered", "weightingrams", "reachedontime")

#check for any missing values column-wise.
colSums(is.na(data)) #No missing values in the dataset.

#structure and summary of the dataset
str(data1)
summary(data1)

#check the proportion of target variable - "reachedontime"
table(data1$reachedontime) #4436 orders were delivered on time. 6563 orders were not delivered on time. 

#convert target variable to factor.
data1$reachedontime <- as.factor(data1$reachedontime)

#check for unique levels in the following variables.
unique(data1$priorpurchase) 
unique(data1$customercarecalls)
unique(data1$customerrating)

#convert the above three variables to factor.
data1$priorpurchase <- as.factor(data1$priorpurchase)
data1$customercarecalls <- as.factor(data1$customercarecalls)
data1$customerrating <- as.factor(data1$customerrating)

#structure and summary of the dataset.
str(data1)
summary(data1)

#build the model
set.seed(17)
model_rf <- randomForest(reachedontime~., data = data1)

model_rf #Out of bag accuracy is 66.06 %
model_rf$classes
model_rf$importance #dignificant variables

#find important variables
varImpPlot(model_rf, pch = 20, main = "Significant Variables")
#“the mean decrease in Gini coefficient is a measure of how each variable contributes to the homogeneity of the nodes and leaves in the resulting random forest…Variables that result in nodes with higher purity have a higher decrease in Gini coefficient.” This importance chart displays the variables that affected the random forest, from greatest impact to least impact, from top to bottom. 
#weightingrams, discountoffered, costoftheproduct, warehouseblock,customerrating, and priorpurchase.

#Increasing the number of trees (ntree = )
#predict values for test set
datatest <- read.csv("test.csv")
datatest$ID <- NULL
colSums(is.na(datatest))
names(datatest) <- c("warehouseblock", "modeofshipment", "customercarecalls", "customerrating", "costoftheproduct", "priorpurchase", "productimportance", "gender", "discountoffered", "weightingrams", "reachedontime")

summary(datatest)
#convert customerrating into factor

datatest$priorpurchase <- as.factor(datatest$priorpurchase)
datatest$customerrating <- as.factor(datatest$customerrating)
datatest$customercarecalls <- as.factor(datatest$customercarecalls)
datatest$reachedontime <- NULL
pred_testrf <- predict(model_rf, newdata = datatest)
head(pred_testrf)
datatest$reachedontime <- pred_testrf
