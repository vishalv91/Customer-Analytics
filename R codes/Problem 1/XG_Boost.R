#Load the required packages.
library(xgboost) #to build xg boost model
library(caret) #to check performance using confusion matrix

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

#using xgboost package
param <-  list(nrounds=c(100), max_depth = c(2),eta =c(0.3),gamma=c(0),
               colsample_bytree=c(0.8),min_child_weight=c(1),subsample=c(1))   

xgbtrain <- xgb.DMatrix(data = data.matrix(train1[,-11]),
                        label = data.matrix(train1$reachedontime))
xgbtest <- xgb.DMatrix(data = data.matrix(test1[,-11]),
                       label = data.matrix(test1$reachedontime))

xgmodel <- xgb.train(params = param, 
                     data = xgbtrain, nrounds = param$nrounds,
                     objective = "binary:logistic")

#predict for test with threshold greater than 0.5
predxgb <- as.factor(as.numeric(predict(xgmodel, xgbtest) > 0.5))
head(predxgb)

#confusion matrix for calculating accuracy
confusionMatrix(predxgb, test1$reachedontime)
#68.27% accuracy

xgb.importance(model = xgmodel)
xgb.plot.importance(xgb.importance(model = xgmodel ), xlab = "Gain",  main = "Significant Variables")
xgb.ggplot.importance(xgb.importance(model = xgmodel)) + ggtitle("Significant variables")
p_load(Ckmeans.1d.dp)

#The significant variables are discount offered, weight in grams, prior purchase, cost of the product, customer care calls, and product importance.
