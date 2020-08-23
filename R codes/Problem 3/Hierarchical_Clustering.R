#HIERARCHICAL CLUSTERING

#Load the required packages.
library(tidyverse) #for data manipulation
 

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

#structure and summary of the dataset.
str(data1)
summary(data1)

#Ony delayed customer data for cluster segmentation
clustdata <- data1 %>% filter(reachedontime == "1") #6563 observations

#Only need variables that are related to/ impact the customer.
clustdata2 <- clustdata[,c(3,4,5,6,8,9)]
clustdata2$gender <- ifelse(clustdata2$gender == "F", 0, 1) #dummy variable

#scale the data
clustdata2.scale <- scale(clustdata2)

#Hierarchial clustering
d <-  dist(clustdata2.scale, method = "euclidean")

H.fit <- hclust(d, method = "ward")

#display dendrogram
plot(H.fit, labels=FALSE, hang=-1) #it is observe that the tree can be cut into three clusters

#draw borders on the dendrogram indicating the cluster groups
rect.hclust(H.fit, k = 3,  border = "red")

#cut the tree into 3 clusters
clustergroups <- cutree(H.fit, k = 3) 
table(clustergroups) #cluster size

#assign clustergroups to the dataset clustdata2
clustdata2$cluster  <- clustergroups

#understand customer profiles from the clusters
customer_profiles <-  clustdata2 %>% group_by(cluster) %>% summarise_all(funs(mean))

cluster_count <- clustdata2 %>% group_by(cluster) %>% summarise(counts = n())

customer_profiles <- mutate(customer_profiles, counts = cluster_count$counts)
customer_profiles

#lets explore each of the three clusters
#CLUSTER 1
cluster1 <- subset(clustdata2, cluster == 1)
cluster1
prop.table(table(cluster1$customercarecalls)) #Most customers made 3 calls to the customer care
prop.table(table(cluster1$customerrating)) #Most of the customers were rated 2
median(cluster1$costoftheproduct) #Median cost of the product is 198 US dollars
prop.table(table(cluster1$priorpurchase)) #Most of the customers made 3 prior purchases
prop.table(table(cluster1$gender))#Majority of customers in cluster 1 are male.
median(cluster1$discountoffered)#46 %

#CLUSTER 2
cluster2 <- subset(clustdata2, cluster == 2)
cluster2
prop.table(table(cluster2$customercarecalls)) #Most customers made 5 calls to the customer care
prop.table(table(cluster2$customerrating)) #Most of the customers were rated 5
median(cluster2$costoftheproduct) #Median cost of the product is 244 US dollars
prop.table(table(cluster2$priorpurchase)) #Most of the customers made 4 prior purchases
prop.table(table(cluster2$gender))#Majority of customers in cluster 2 are female.
median(cluster2$discountoffered)#7 %

#CLUSTER 3
cluster3 <- subset(clustdata2, cluster == 3)
cluster3
prop.table(table(cluster3$customercarecalls)) #Most customers made 3 calls to the customer care
prop.table(table(cluster3$customerrating)) #Most of the customers were rated 3
median(cluster3$costoftheproduct) #Average cost of the product is 197 US dollars
prop.table(table(cluster3$priorpurchase)) #Most of the customers made 3 prior purchases
prop.table(table(cluster3$gender))#Majority of customers in cluster 3 are male.
median(cluster3$discountoffered)#7%
