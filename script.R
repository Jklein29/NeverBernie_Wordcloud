### Load packages

library(rtweet)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)

### Import tweets

nbern <- search_tweets("#NeverBernie", n = 18000, include_rts = F)

# Filter since 1/25

newbern <- filter(nbern, created_at >= "2019-01-25")

# Create corpus of tweet text

bern_tweet <- Corpus(VectorSource(newbern$text))

# Clean tweets

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

bern_tweet <- tm_map(bern_tweet, toSpace, "https://t.co/[a-z,A-Z,0-9]*{8}")
bern_tweet <- tm_map(bern_tweet, toSpace, "@\\w+")
bern_tweet  <- tm_map(bern_tweet, toSpace, "#[a-z,A-Z]*")
bern_tweet <- tm_map(bern_tweet, toSpace, "&amp")
bern_tweet <- tm_map(bern_tweet, toSpace, "[^[:alnum:][:blank:]?&/\\-]")
bern_tweet <- tm_map(bern_tweet, toSpace, "U00..")
bern_tweet <- tm_map(bern_tweet, content_transformer(tolower))
bern_tweet <- tm_map(bern_tweet, removeNumbers)
bern_tweet <- tm_map(bern_tweet, removeWords, stopwords("english"))
bern_tweet <- tm_map(bern_tweet, removePunctuation)
bern_tweet <- tm_map(bern_tweet, stripWhitespace)




TermDocumentMatrix(bern_tweet) %>% 
  as.matrix(tdm) -> b_matr

sort(rowSums(b_matr), decreasing = T) -> v

%>% 
  data.frame(word = names(.), freq = .)



bern_dtm <- TermDocumentMatrix(bern_tweet)
bern_m <- as.matrix(bern_dtm)
v <- sort(rowSums(bern_m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)



ts_plot(newbern)

## load rtweet package
library(twitteR)
library(rtweet)
library(tidyverse)

consumer_key <- "IfbZ7YKEEywOsHirlaiF4mtnV"
consumer_secret <- "aXzpgtkhwCcEUL7YWoSZugPcSEowUz0iNcnXf8dtpBPWxemV18"
access_token <- "968689359808589824-4G1sItdXDbfekG8y0Oupvems6sf6iOZ"
access_secret <- "HeLyLYGGdvlcQNV2Se03Z5mU2ogdTT676SZPr0SIRtopi"
options(httr_oauth_cache=T) 
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)



table(nbern$location)

as.data.frame(table(nbern$location)) %>% 
  .[order(.$Freq, decreasing = T), ] %>% 
  .[1:25, ]


[order(nbern$location, decreasing = T), ]
