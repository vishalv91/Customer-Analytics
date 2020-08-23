#PROBLEM 4 - TWITTER SENTIMENTAL ANALYSIS of AMAZON INDIA

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

amazonIN <- userTimeline("amazonIN", n = 2000)
amazonIN.df <- twListToDF(amazonIN)
amazonIN.text <- amazonIN.df$text

#remove retweets
amazonIN.text <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)','', amazonIN.text)
head(amazonIN.text)

#remove @name
amazonIN.text <-  gsub('@\\w+','',amazonIN.text)
head(amazonIN.text)

#remove punctuation
amazonIN.text <- gsub('[[:punct:]]','', amazonIN.text)
head(amazonIN.text)

#remove numbers
amazonIN.text <- gsub('[[:digit:]]', '', amazonIN.text)
head(amazonIN.text)

#remove url
amazonIN.text <- gsub('http\\w+','', amazonIN.text)
amazonIN.text <- gsub('https\\w+','', amazonIN.text)
head(amazonIN.text)

#remove whitespaces
amazonIN.text <-  gsub('[\t]{2,}','', amazonIN.text)
head(amazonIN.text)
amazonIN.text <- gsub('^\\s+|\\s+$','', amazonIN.text)
head(amazonIN.text)

#remove emojis or special charcters
amazonIN.text <- gsub('<.*>','', amazonIN.text)
head(amazonIN.text)

#convert the text to lower case
amazonIN.text <- tolower(amazonIN.text)
head(amazonIN.text)

#remove duplicates due to retweets
amazonIN.text <- amazonIN.text[!duplicated(amazonIN.text)]

#Extract sentiment and polarity
nrc.amazonIN <- get_nrc_sentiment(amazonIN.text)

#change nrc.amazonIN to a data frame and transpose it 
nrc.amazonIN <- data.frame(t(nrc.amazonIN))

#use rowSums to compute column sums across rows for each level of a variable
nrc.amazonIN <- data.frame(rowSums(nrc.amazonIN))

#rename rows and columns of the dataframe
names(nrc.amazonIN)[1] <- "count"
nrc.amazonIN <- cbind("sentiment" = rownames(nrc.amazonIN), nrc.amazonIN)
rownames(nrc.amazonIN) <- NULL

#plot the first eight rows - emotions
qplot(sentiment, data = nrc.amazonIN[ 1:8,], weight = count, geom = "bar", fill = sentiment) + ggtitle("How do customers feel about Amazon India?")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=nrc.amazonIN[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiment for Amazon India")


