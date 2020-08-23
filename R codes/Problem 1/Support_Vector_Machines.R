#Load the required packages.
library(e1071) #for svm function
library(caret) #confusion matrix #to build a model using method = svmLinear

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

#split dataset into train and test data. The latter is for validation purpose.
set.seed(1234)
trainsample1 <- sample(1:nrow(data1), 0.7 * nrow(data1))
train1 <- data1[trainsample1,]
test1 <- data1[-trainsample1,]

#Model with tuning parameter C set at = 1
fit <- svm(reachedontime ~., data = train1, method = "C-classification", kernel = "linear", scale = T)
summary(fit)

#predict for unseen data
pred <- predict(fit, newdata = test1, type = "response")

#confusion matrix
confusionMatrix(pred, test1$reachedontime, positive = "0") 
#Accuracy is 66.09 %

#Try with other cost values
#for cost = 0.5 - accuracy is 66.03 %
#for cost = 1 (default) - accuracy is  66.09%
#for cost = 1.5 - accuracy is  66.09 %
#for cost = 2 - accuracy is 66.12 %
#for cost = 5 - accuracy is 66.15 %
#for cost = 5.5 - accuracy is 66.18 %
#for cost = 10 - accuracy is 66.15 %
#for cost = 11 - accuracy is 66.12 %
#for cost = 15 - accuracy is 66.12%

#New model with cost value set at 5.5
fit2 <- svm(reachedontime ~., data = train1, cost = 5.5,
            method = "C-classification", kernel = "linear", scale = T)

#Predict for test data
pred2 <- predict(fit2, newdata = test1)

#confusion matrix
confusionMatrix(pred2, test1$reachedontime, positive = "0")
#Accuracy is 66.15 %

#Only significant variables and Cost value set at 5.5
fit3 <- svm(reachedontime ~discountoffered + weightingrams + costoftheproduct + priorpurchase + customercarecalls + productimportance, data = train1, cost = 5.5,
            method = "C-classification", kernel = "linear", scale = T)

#Predict for test data
pred3 <- predict(fit3, newdata = test1, type = "response")

#confusion matrix
confusionMatrix(pred3, test1$reachedontime, positive = "0")
#Accuracy is 66.24 %

#SVM classifier (Caret package)
set.seed(12)
trncntrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_linear <- train(reachedontime ~., data = data1, method = "svmLinear",
                    trControl = trncntrl, 
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_linear
#66.08 % accuracy

#Identify significant variables
varImp(svm_linear)

