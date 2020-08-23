#PROBLEM 4 - TWITTER SENTIMENTAL ANALYSIS of SNAPDEAL

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

snapdeal <- userTimeline("snapdeal", n = 2000)
snapdeal.df <- twListToDF(snapdeal)
snapdeal.text <- snapdeal.df$text #extract text variable

#Data pre-processing
#remove retweet entity
snapdeal.text <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)','', snapdeal.text)
head(snapdeal.text)

#remove @name
snapdeal.text <-  gsub('@\\w+','',snapdeal.text)
head(snapdeal.text)

#remove punctuation
snapdeal.text <- gsub('[[:punct:]]','', snapdeal.text)
head(snapdeal.text)

#remove numbers
snapdeal.text <- gsub('[[:digit:]]', '', snapdeal.text)
head(snapdeal.text)

#remove url
snapdeal.text <- gsub('http\\w+','', snapdeal.text)
snapdeal.text <- gsub('https\\w+','', snapdeal.text)
head(snapdeal.text)

#remove whitespaces
snapdeal.text <-  gsub('[\t]{2,}','', snapdeal.text)
head(snapdeal.text)
snapdeal.text <- gsub('^\\s+|\\s+$','', snapdeal.text)
head(snapdeal.text)

#remove emojis or special charcters
snapdeal.text <- gsub('<.*>','', snapdeal.text)
snapdeal.text <- gsub('ðŸ‘','', snapdeal.text)
head(snapdeal.text)

#remove duplicates due to retweets
snapdeal.text <- snapdeal.text[!duplicated(snapdeal.text)]

#Extract sentiment and polarity
nrc.snapdeal <- get_nrc_sentiment(snapdeal.text)

#change nrc.amazonIN to a data frame and transpose it 
nrc.snapdeal <- data.frame(t(nrc.snapdeal))

#use rowSums to compute column sums across rows for each level of a variable
nrc.snapdeal <- data.frame(rowSums(nrc.snapdeal))

#rename rows and columns of the dataframe
names(nrc.snapdeal)[1] <- "count"
nrc.snapdeal <- cbind("sentiment" = rownames(nrc.snapdeal), nrc.snapdeal)
rownames(nrc.snapdeal) <- NULL

#plot the first eight rows - emotions
qplot(sentiment, data = nrc.snapdeal[ 1:8,], weight = count, geom = "bar", fill = sentiment) + ggtitle("How do customers feel about Snapdeal?")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=nrc.snapdeal[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiment for Snapdeal")

