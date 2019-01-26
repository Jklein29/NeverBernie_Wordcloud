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

# Drop words

bern_tweet <- tm_map(bern_tweet, removeWords, c("bernie", "sanders", "never", "will", "vote", 
                                                "don", "just", "can", "run", "get", "one", 
                                                "now", "let", "isn")) 

# Create dataframe of results

TermDocumentMatrix(bern_tweet) %>% 
  as.matrix() -> b_matr

b_vect <- sort(rowSums(b_matr), decreasing = T)


bern_data <- data.frame(word = names(b_vect), freq = b_vect)

# Generate worldcloud

wordcloud(words = bern_data$word, freq = bern_data$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

