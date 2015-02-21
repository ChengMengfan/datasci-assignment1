rm()
setwd("/Users/Mengfan/Desktop/CMF/2015Spring/670Sood/Assignment1/socialmedia")
## toInstall <- c("ROAuth", "twitteR", "streamR", "ggplot2", "stringr","tm", "RCurl", "maps", "Rfacebook", "topicmodels", "devtools")
## install.packages(toInstall, repos = "http://cran.r-project.org")
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "pEzzwUWTREXrSSo9HiykPzFay"
consumerSecret <- "Zt4cR9xnpWH4fXIoZrKeNesjsoSuaMN9GG1lLqoPiqjFrxQ7LE"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
## change callback URL to http://127.0.0.1:1410 if necessary
library(twitteR)
setup_twitter_oauth(consumerKey,consumerSecret)

## save oauth token
save(my_oauth, file="/Users/Mengfan/Desktop/CMF/2015Spring/670Sood/Assignment1/socialmedia/oauth_token.Rdata")

## load token
load("/Users/Mengfan/Desktop/CMF/2015Spring/670Sood/Assignment1/socialmedia/oauth_token.Rdata")

## one way to collect tweets
## library(twitteR)
## nytimes <- userTimeline('nytimes',n=500)
## ft <- userTimeline('FinancialTimes',n=500)
## npr <- userTimeline('nprnews',n=500)
## cnn <- userTimeline('CNN',n=500)
## newyorker <- userTimeline('NewYorker',n=500)
## afp <- userTimeline('AFP',n=500)
## wsj <- userTimeline('WSJ',n=500)
## ap <- userTimeline('AP',n=500)
## gd <- userTimeline('guardian',n=500)
## reuters <- userTimeline('Reuters',n=500)

## another way to collect tweets
## install RJSONIO package
## install.packages("RJSONIO")

## store the raw JSON data
source("functions.r")

getTimeline(filename="tweets_nytimes.json", screen_name="nytimes", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_ft.json", screen_name="FinancialTimes", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_npr.json", screen_name="npr", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_cnn.json", screen_name="cnn", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_newyorker.json", screen_name="newyorker", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_afp.json", screen_name="AFP", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_wsj.json", screen_name="wsj", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_ap.json", screen_name="ap", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_guardian.json", screen_name="guardian", n=500, oauth=my_oauth, trim_user="false")
getTimeline(filename="tweets_reuters.json", screen_name="reuters", n=500, oauth=my_oauth, trim_user="false")

## tweets analysis

library(streamR)
library(ggplot2)
library(grid)
library(maps)

# Loading tweets
nytimes <- parseTweets("tweets_nytimes.json")
ft <- parseTweets("tweets_ft.json")
npr <- parseTweets("tweets_npr.json")
cnn <- parseTweets("tweets_cnn.json")
newyorker <- parseTweets("tweets_newyorker.json")
afp <- parseTweets("tweets_afp.json")
wsj <- parseTweets("tweets_wsj.json")
ap <- parseTweets("tweets_ap.json")
guardian <- parseTweets("tweets_guardian.json")
reuters <- parseTweets("tweets_reuters.json")


# loading lexicon of positive and negative words (from Neal Caren)
lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]

# function to clean the text
clean_tweets <- function(text){
  # loading required packages
  lapply(c("tm", "Rstem", "stringr"), require, c=T, q=T)
  # avoid encoding issues by dropping non-unicode characters
  utf8text <- iconv(text, to='UTF-8-MAC', sub = "byte")
  # remove punctuation and convert to lower case
  words <- removePunctuation(utf8text)
  words <- tolower(words)
  # spliting in words
  words <- str_split(words, " ")
  return(words)
}

# clean the text
nytimes$text[1]
guardian$text[1]

nytimes_text <- clean_tweets(nytimes$text)
ft_text <- clean_tweets(ft$text)
npr_text <- clean_tweets(npr$text)
cnn_text <- clean_tweets(cnn$text)
newyorker_text <- clean_tweets(newyorker$text)
afp_text <- clean_tweets(afp$text)
wsj_text <- clean_tweets(wsj$text)
ap_text <- clean_tweets(ap$text)
guardian_text <- clean_tweets(guardian$text)
reuters_text <- clean_tweets(reuters$text)

nytimes_text[[1]]
guardian_text[[7]]

# function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

# function to classify many tweets...
classifier <- function(text, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(nytimes_text, pos.words, neg.words)
classifier(ft_text, pos.words, neg.words)
classifier(npr_text, pos.words, neg.words)
classifier(cnn_text, pos.words, neg.words)
classifier(newyorker_text, pos.words, neg.words)
classifier(afp_text, pos.words, neg.words)
classifier(wsj_text, pos.words, neg.words)
classifier(ap_text, pos.words, neg.words)
classifier(guardian_text, pos.words, neg.words)
classifier(reuters_text, pos.words, neg.words)

