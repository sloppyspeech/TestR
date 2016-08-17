#Perform Sentiment Analysis
library(wordcloud)
library(SnowballC)
library(tm)
library(stringr)
library(RColorBrewer)
library(data.table)
library(dplyr)

setwd("~/Documents/LearnR")
l.pwords <- scan(paste("~/Documents/LearnR","/Data/SentimentAnalysis/positive-words.txt",sep=""),what = 'character',comment.char = ";")
l.nwords <- scan(paste("~/Documents/LearnR","/Data/SentimentAnalysis/negative-words.txt",sep=""),what = 'character',comment.char = ";")
# Add some extra words
l.nwords <- c(l.nwords,'wtf','wait','waiting','epicfail', 'crash', 'bug', 'bugy', 'bugs', 'slow', 'lie')
#Remove some words for example sap and Cloud
l.nwords <- l.nwords[!l.nwords=='sap']
l.nwords <- l.nwords[!l.nwords=='cloud']

#inp <- readLines("./Data/Twitter/kejri_may182016_to_Aug2015.txt")
inp <- readLines("./Data/Twitter/modi.txt")

#inp
length(inp)

cleanText <- function (x){
  x=tolower(x)
  x=grep("^    ",x,value=TRUE)
  x=gsub("    ","",x)
  x=gsub("rt @","",x)
  #-- Remove user references @<username>
  x=gsub(" @\\w+","",x)
  
  x=gsub("[[:punct:]]","",x)
  #x=gsub("[^[:graph:]]","",x)
  x=gsub("[[:cntrl:]]","",x)
  x=gsub("http\\w+","",x)
  
  return(x)
}

inp.cleaned <- cleanText(inp)
inp.cleaned<-iconv(inp.cleaned,"utf-8","ASCII",sub="")


getSentimentScore <- function(tweets) {
    scores <- laply(tweets, function(tweet){
        tweetWords <- unlist(str_split(tolower(tweet),'\\s+'))
        pos.matches <- !is.na(match(tweetWords, l.pwords))
        neg.matches <- !is.na(match(tweetWords, l.nwords))
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score <- sum(pos.matches) - sum(neg.matches)
        return (score)
    })
  return (data.frame(Tweet=tweets,SentimentScore=scores))
}

ss <-getSentimentScore(inp.cleaned)
View(ss)
# Get rid of tweets that have zero score and seperate +ve from -ve tweets
ss$posTweets <- as.numeric(ss$SentimentScore >=1)
ss$negTweets <- as.numeric(ss$SentimentScore <=-1)

# Let's summarize now
summary <- list(TweetsFetched=length(ss$SentimentScore),
                PositiveTweets=sum(ss$posTweets), NegativeTweets=sum(ss$negTweets),
                AverageScore=round(mean(ss$SentimentScore),3))
# some tweets have no score - positive offsets negative - so the next line is necessary
summary$TweetsWithScore <- summary$PositiveTweets + summary$NegativeTweets

#Get Sentiment Score
summary$SentimentScore  <- round(summary$PositiveTweets/summary$TweetsWithScore, 2)
View(summary)
