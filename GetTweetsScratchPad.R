library(twitteR)
consumer_key <- "myRFmTmbQH5ioyRDwfjnIzJ0D"
consumer_secret <- "LtGrw12yXYxV8V4GYiWiO3ADruDVIm1LBn0PKMwhISSwcNtBlL"
access_token <- "2170278542-HHtY1COPLzq7c8hNeft1tw202rJUNZXwl58SNVI"
access_secret <- "REy430ydigzyZGVbyUoKJ7N3XMMEKf8NkGfCsUsdWJnAW"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
timeLine <- homeTimeline(n = 800, retryOnRateLimit = 1000)
beginning <-  as.POSIXct("2016-05-15 09:00:00 UTC")
end <-  as.POSIXct("2016-05-15 12:00:00 UTC")
times <- lapply(timeLine, function(x) x$created)
these <- which(times > beginning & times < end)
myMorningTweets <- timeLine[these]
myMorningTweetsDF <- twListToDF(myMorningTweets)
#-------------------------------------
#install the necessary packages
install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
 
library("twitteR")
library("wordcloud")
library("tm")
 
#necessary file for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
 
#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'your key'
consumer_secret <- 'your secret'
access_token <- 'your access token'
access_secret <- 'your access secret'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)
 
#the cainfo parameter is necessary only on Windows
r_stats <- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")
#should get 1500
length(r_stats)
#[1] 1500
 
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
 
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))
 
#clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)
 
#alternative steps if you're running into problems 
r_stats<- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))
 
#if you get the below error
#In mclapply(content(x), FUN, ...) :
#  all scheduled cores encountered errors in user code
#add mc.cores=1 into each function
 
#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1
                              )
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(r_stats_text_corpus)

#-----------------------------------------------------

library(twitteR)
library(wordcloud)
library(SnowballC)
library(tm)
library(stringr)

consumer_key <- "myRFmTmbQH5ioyRDwfjnIzJ0D"
consumer_secret <- "LtGrw12yXYxV8V4GYiWiO3ADruDVIm1LBn0PKMwhISSwcNtBlL"
access_token <- "2170278542-HHtY1COPLzq7c8hNeft1tw202rJUNZXwl58SNVI"
access_secret <- "REy430ydigzyZGVbyUoKJ7N3XMMEKf8NkGfCsUsdWJnAW"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

r_stats <- searchTwitter("#Rstats", n=1500)
r_stats_text <- sapply(r_stats, function(x) x$getText())

usableText=str_replace_all(tweets$text,"[^[:graph:]]", " ") 

rdc <- Corpus(VectorSource(r_stats_text))
rdc <- Corpus(VectorSource(r_stats_text))
rdc <- tm_map(rdc,content_transformer(tolower),lazy=TRUE)
rdc <- tm_map(rdc,removePunctuation,lazy=TRUE)
rdc <- tm_map(rdc,removeWords,lazy=TRUE,stopwords("english"))
rdtdm <- as.matrix(TermDocumentMatrix(rdc))