#Load the required packages.
library(caret) #confusion matrix
library(car) #to check for multicollinearity

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
#There is class imbalance.

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

#proportion of target variable in train and test data
table(train1$reachedontime)
#Baseline model
model1 <- glm(reachedontime ~ ., data = train1, family = binomial(link = "logit"))
summary(model1)
#AIC : 8387.8

#Check for multicollinearity
vif(model1) #all variables are independent

#predict for test data using baseline model 
pred <- predict(model1, newdata = test1, type = 'response')

#Confusion matrix
confusionMatrix(data= as.factor(as.numeric(pred >= 0.5)), reference = test1$reachedontime, positive = "0")
#Accuracy for the baseline model is 64.48 %


#stepwise model from baseline model 
stepmodel1 <-  step(model1 , direction = "both")

formula(stepmodel1)
#reachedontime ~ customercarecalls + costoftheproduct + 
#priorpurchase + productimportance + discountoffered + weightingrams

summary(stepmodel1)
#AIC : 8370.7

#predict for stepwise model 1
predstep1 <-  predict(stepmodel1, newdata = test1)

#confusion matrix for stepwise model 1
confusionMatrix(data= as.factor(as.numeric(predstep1 >= 0.5)), reference = test1$reachedontime, positive = "0")
#Accuracy for stepwise model 1 is 66.55%

#CONCLUSION
#Accuracy comparison
#Baseline model (Model 1)                                           : 64.48%
#Stepwise model from Model 1                                        : 66.55%


#I will chose Stepwise model because it has fairly better accuracy than the base model and includes only significant variables
#Important variables are customercarecalls, costoftheproduct, priorpurchase.
#productimportance, discountoffered, weightingrams

#Now to predict if shipments reached on time or not using stepwise model on new data
datatest <- read.csv("test.csv")
datatest$ID <- NULL
colSums(is.na(datatest))
names(datatest) <- c("warehouseblock", "modeofshipment", "customercarecalls", "customerrating", "costoftheproduct", "priorpurchase", "productimportance", "gender", "discountoffered", "weightingrams", "reachedontime")

summary(datatest)
#convert customerrating into factor

datatest$priorpurchase <- as.factor(datatest$priorpurchase)
datatest$customerrating <- as.factor(datatest$customerrating)
datatest$customercarecalls <- as.factor(datatest$customercarecalls)

pred_datatest <- predict(stepmodel1, newdata = datatest)
datatest$reachedontime <- NULL
datatest$reachedontime <- round(exp(pred_datatest)/(1 + exp(pred_datatest)), digits = 0)

summary(stepmodel1)
#coeffients  are in a form called logits
#if coefficients (logits) are positive, the effect on timely delivery is positive
#when negative, higher the value for signifcant variables, lower the probability of timely delivery
