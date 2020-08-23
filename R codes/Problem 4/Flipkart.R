#PROBLEM 4 - TWITTER SENTIMENTAL ANALYSIS of FLIPKART

#Loading the required packages
library(syuzhet)#to extract sentiments
library(twitteR)#to collect tweets
library(dplyr)#data manipulation
library(ggplot2) #data visualization


#authenticate keys from Twitter API
consumer_key <- "   " #Enter your consumer key
consumer_secret <- " " #Enter your consumer secret key
access_token <- "   " #Enter your access token
access_secret <- "  " #Enter your access secret key

setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret, access_token = access_token, access_secret = access_secret)

flipkart <- userTimeline("flipkart", n = 2000)
flipkart.df <- twListToDF(flipkart)
flipkart.text <- flipkart.df$text #extract text variable

#Data pre-processing
#remove retweet entity
flipkart.text <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)','', flipkart.text)
head(flipkart.text)

#remove @name
flipkart.text <-  gsub('@\\w+','',flipkart.text)
head(flipkart.text)

#remove punctuation
flipkart.text <- gsub('[[:punct:]]','', flipkart.text)
head(flipkart.text)

#remove numbers
flipkart.text <- gsub('[[:digit:]]', '', flipkart.text)
head(flipkart.text)

#remove url
flipkart.text <- gsub('http\\w+','', flipkart.text)
flipkart.text <- gsub('https\\w+','', flipkart.text)
head(flipkart.text)

#remove whitespaces
flipkart.text <-  gsub('[\t]{2,}','', flipkart.text)
head(flipkart.text)
flipkart.text <- gsub('^\\s+|\\s+$','', flipkart.text)
head(flipkart.text)

#remove emojis or special charcters
flipkart.text <- gsub('<.*>','', flipkart.text)
head(flipkart.text)

#convert the text to lower case
flipkart.text <- tolower(flipkart.text)
head(flipkart.text)

#remove duplicates due to retweets
flipkart.text <- flipkart.text[!duplicated(flipkart.text)]

#Extract sentiment and polarity
nrc.flipkart <- get_nrc_sentiment(flipkart.text)

#change nrc.amazonIN to a data frame and transpose it 
nrc.flipkart <- data.frame(t(nrc.flipkart))

#use rowSums to compute column sums across rows for each level of a variable
nrc.flipkart <- data.frame(rowSums(nrc.flipkart))

#rename rows and columns of the dataframe
names(nrc.flipkart)[1] <- "count"
nrc.flipkart <- cbind("sentiment" = rownames(nrc.flipkart), nrc.flipkart)
rownames(nrc.flipkart) <- NULL

#plot the first eight rows - emotions
qplot(sentiment, data = nrc.flipkart[ 1:8,], weight = count, geom = "bar", fill = sentiment) + ggtitle("How do customers feel about Flipkart?")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=nrc.flipkart[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiment for Flipkart")
