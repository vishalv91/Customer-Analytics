#Loading the required packages
library(tidyverse)#for data manipulation
library(ggplot2) #for data visualization

##Set working directory where dataset is located.
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

#identify the best customers with rating is equal to and greater than 3
good_rating <- data1 %>% filter(as.numeric(customerrating) >= 3 ) 
prop.table(table(good_rating$reachedontime)) #60.26 % customers whith rating of 3 or more didnt receive shipments on time.

good_rating$reachedontime <-  ifelse(good_rating$reachedontime == "0", "Yes", "No")

#Visualize the finding
g1 <- ggplot(good_rating, aes(reachedontime))  + geom_bar(aes(fill = reachedontime))
g1 + labs(title = "Have shipments reached on time for customers with a good rating?", subtitle = "Identified customers who were rated 3 and higher.", x = "Reached on time", y = "No of customers", caption = "About 39.7% of the customers received shipment on time") + geom_text(stat = "count",aes(label = ..count..), vjust = 3, size =  9)

#Customer score
#Average order value = Total Revenue / Total number of orders
sum(data1$costoftheproduct) #2311955 (approx. 2.3 Million US dollars)
averageordervalue <-2311955/10999
data1$customerscore <- averageordervalue * as.numeric(data1$priorpurchase)
summary(data1$customerscore) #Median score is 420.4

#identify the best customers with score greater than or eual to median score (420.4).
best_customerscore <- data1 %>% filter(customerscore >= median(customerscore))
prop.table(table(best_customerscore$reachedontime)) #41.21 % on-time delivery rate.
best_customerscore$reachedontime <-  ifelse(best_customerscore$reachedontime == "0", "Yes", "No")

#Visualize the finding
g2 <- ggplot(best_customerscore, aes(reachedontime)) + geom_bar(aes(fill = reachedontime))
g2 + labs(title = "Have shipments reached on time for customers with a good score?", subtitle = "Identified customers with score equal to and greater than the median score(420.4).", x = "Reached on time", y = "No of customers", caption = "About 41.21% of the customers received shipment on time") + geom_text(stat = "count",aes(label = ..count..), vjust = 3, size =  9)


#Recurring orders
#identify customers who make recurring orders greater than or equal to 6.
recurring_orders <- data1 %>% filter(as.numeric(priorpurchase) >= 6)
prop.table(table(recurring_orders$reachedontime))
recurring_orders$reachedontime <-  ifelse(recurring_orders$reachedontime == "0", "Yes", "No")


#visualize the finding
g3 <- ggplot(recurring_orders, aes(reachedontime)) + geom_bar(aes(fill = reachedontime))
g3 + labs(title = "Have shipments reached on time for customers who make recurring orders?", subtitle = "Identified customers who made prior purchases of 6 and more.", x = "Reached on time", y = "No of customers", caption = "About 37.33% of the customers received shipment on time") + geom_text(stat = "count",aes(label = ..count..), vjust = 3, size =  9)

#identify customers who make the highest payment
max(data1$costoftheproduct) #310 US dollars
min(data1$costoftheproduct) #96 US dollars
median_value <- median(data1$costoftheproduct)
median_value #214 US dollars

#identify the customers who made payments greater than or equal to the median value
highest_payments <- data1 %>% filter(costoftheproduct >= median_value)
prop.table(table(highest_payments$reachedontime))
highest_payments$reachedontime <-  ifelse(highest_payments$reachedontime == "0", "Yes", "No")

#visualize the finding
g4 <- ggplot(highest_payments, aes(reachedontime)) + geom_bar(aes(fill = reachedontime))
g4 + labs(title = "Have shipments reached on time for customers who made highest payments?", subtitle = "Identified customers who made payments more than the median value (214 US dollars).", x = "Reached on time", y = "No of customers", caption = "About 43.43% of the customers received shipment on time") + geom_text(stat = "count",aes(label = ..count..), vjust = 3, size =  9)

#Identify best customers who satisfy all the above criteria
mostvaluable_customers <- data1 %>% filter(as.numeric(customerrating) >= 3,customerscore >= median(customerscore), as.numeric(priorpurchase) >= 6, costoftheproduct >= median_value)
prop.table(table(mostvaluable_customers$reachedontime))
mostvaluable_customers$reachedontime <- ifelse(mostvaluable_customers$reachedontime == "0", "Yes", "No")

#visualize the finding
g5 <- ggplot(mostvaluable_customers, aes(reachedontime)) + geom_bar(aes(fill = reachedontime))
g5 + labs(title = "Potential customers who satisfied all four criteria", subtitle = "Out of the 10999 observations, only 115 observations were identified as most valuable customers", x = "Reached on time", y = "No of customers", caption = "About 35.65% of the customers recieved shipment on time") + geom_text(stat = "count",aes(label = ..count..), vjust = 3, size =  9)

