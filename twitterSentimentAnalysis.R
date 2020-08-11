######### Twitter Sentiment Analysis Project ########

library("twitteR")
library("ROAuth")
library("RCurl")
library("bitops")
library("rjson")

###########
library(tm)
library(SnowballC)
############

# Twitter Developer API Key and Secret
t.api.key <- "<<REPLACE WITH TWITTER API KEY>>"
t.api.secret <- "<<REPLACE WITH TWITTER API SECRET>>"

# ---------  version 1.1.8 --------- 
setup_twitter_oauth(t.api.key, t.api.secret, access_token=NULL, access_secret=NULL)
# setup_twitter_oauth(t.api.key, t.api.secret, access_token=t.access.token, access_secret=t.access.secret) # Alternate
save(list=(c("t.api.key","t.api.secret")), file="twitter_credentials.RData")

# For everyday use
load("twitter_credentials.RData")
setup_twitter_oauth(t.api.key, t.api.secret, access_token=NULL, access_secret=NULL)

tweets1 <- searchTwitter("$ZEN",since='2017-10-12', n = 100)
#tweets1 <- searchTwitter("$ZEN", n = 100)
head(tweets1)

tweets2 <- searchTwitter("$TWTR",since='2017-10-12', n=100)
head(tweets2)

tweets3 <- searchTwitter("$SNAP",since='2017-10-12', n=100)

head(tweets3)

tweets.gainers <- c(tweets1,tweets2,tweets3)

tweets.gainersTxt <- 
  lapply(tweets.gainers, 
         function(t) {
           iconv(t$getText(), 
                 "latin1", "ASCII", sub="")
         })


head(tweets.gainersTxt, n = 2)
length(tweets.gainersTxt)

############## loser stocks

tweets4 <- searchTwitter("$HAWK",since='2017-10-12', n = 100)

head(tweets4)

tweets5 <- searchTwitter("$ULTA",since='2017-10-12', n = 100)
head(tweets5,n=2)

tweets6 <- searchTwitter("$T",since='2017-10-12', n = 100)
head(tweets6,n=2)

tweets.losers <- c(tweets4,tweets5,tweets6)

tweets.losersTxt <- 
  lapply(tweets.losers, 
         function(t) {
           iconv(t$getText(), 
                 "latin1", "ASCII", sub="")
         })


head(tweets.losersTxt, n = 2)
length(tweets.losersTxt)

####function that takes a tweets and return a corpus ####

Return.corpus<- function(t)
{
  S<- VectorSource(t)
  result<- Corpus(S)
  
}

data.corpus1<- Return.corpus(tweets.gainersTxt)
inspect(data.corpus1[1:2])
data.corpus2<- Return.corpus(tweets.losersTxt)
inspect(data.corpus2[1:2])
# #########
# data.source1 <- VectorSource(tweets.gainersTxt)
# data.corpus1 <- Corpus(data.source1)
# data.corpus1
# inspect(data.corpus1[1:2])
# #########
# data.source2 <- VectorSource(tweets.losersTxt)
# data.corpus2 <- Corpus(data.source2)
# inspect(data.corpus2[1:2])
#########
mypath<-setwd("<<REPLACE WITH PATH>>")

writeCorpus(data.corpus1, path = mypath)
writeCorpus(data.corpus2, path = mypath)

##### preprocessing #####

data.corpus1 <- 
  tm_map(data.corpus1, 
         content_transformer(tolower))

inspect(data.corpus1[1:2])

removeURL <- function(x) {
  gsub("(http[^ ]*)", "", x)
}

data.corpus1 <- 
  tm_map(data.corpus1, 
         content_transformer(removeURL))

inspect(data.corpus1[1:2])

data.corpus1 <- 
  tm_map(data.corpus1, 
         content_transformer(removePunctuation))

english.stopwords <- stopwords("en")
head(english.stopwords)

data.corpus1 <-
  tm_map(data.corpus1,
          content_transformer(removeWords),
          english.stopwords)

 #inspect the first two values
inspect(data.corpus1[1:2])

removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
}

data.corpus1 <- 
  tm_map(data.corpus1, 
         content_transformer(removeNumberWords))


# inspect the first two values
inspect(data.corpus1[1:2])


data.corpus1 <- 
  tm_map(data.corpus1,
         content_transformer(stemDocument))

data.corpus1 <- 
  tm_map(data.corpus1,
         content_transformer(stripWhitespace))

# inspect the first two values
inspect(data.corpus1[1:2])

####### preprocessing for the losers stocks ########

data.corpus2 <- 
  tm_map(data.corpus2, 
         content_transformer(tolower))

inspect(data.corpus2[1:2])

removeURL <- function(x) {
  gsub("(http[^ ]*)", "", x)
}

data.corpus2 <- 
  tm_map(data.corpus2, 
         content_transformer(removeURL))

inspect(data.corpus2[1:2])

data.corpus2 <- 
  tm_map(data.corpus2, 
         content_transformer(removePunctuation))

english.stopwords <- stopwords("en")
head(english.stopwords)

data.corpus2 <-
  tm_map(data.corpus2,
         content_transformer(removeWords),
         english.stopwords)

#inspect the first two values
inspect(data.corpus2[1:2])

removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
}

data.corpus2 <- 
  tm_map(data.corpus2, 
         content_transformer(removeNumberWords))


# inspect the first two values
inspect(data.corpus2[1:2])


data.corpus2 <- 
  tm_map(data.corpus2,
         content_transformer(stemDocument))

data.corpus2 <- 
  tm_map(data.corpus2,
         content_transformer(stripWhitespace))

# inspect the first two values
inspect(data.corpus2[1:2])

####################################
# Build the term document matrix for stock gainers data

tdm1 <- TermDocumentMatrix(data.corpus1)

# inspect part of the matrix

inspect(tdm1[250:260, 50:60])

save(list=("tdm1"),
     file = "tdm1.RData")

load(file="tdm1.RData")

# Build the term document matrix for stock gainers data

tdm2 <- TermDocumentMatrix(data.corpus2)

# inspect part of the matrix

inspect(tdm2[250:260, 50:60])

save(list=("tdm2"),
     file = "tdm2.RData")

load(file="tdm2.RData")

#####################################

## find frequent terms for stock gainers data

# convert TDM to a matrix

m1 <- as.matrix(tdm1)

# View portion of the matrix
m1[250:260, 51:60]

# calculate the frequency of words 
wordFreq1 <- rowSums(m1)

# Examine part of the frequencies
cbind(wordFreq1[150:160])

# Sort the words by descending order of frequency
wordFreq1 <- sort(wordFreq1, decreasing=TRUE)

# Examine the top ten words
cbind(wordFreq1[1:10])

# frequent terms
findFreqTerms(tdm1, lowfreq=15)

# associations
findAssocs(tdm1, "stock", 0.4) 

#############
## find frequent terms for stock losers data
# convert TDM to a matrix

m2 <- as.matrix(tdm2)

# View portion of the matrix
m2[290:300, 51:60]

# calculate the frequency of words 
wordFreq2 <- rowSums(m2)

# Examine part of the frequencies
cbind(wordFreq2[250:260])

# Sort the words by descending order of frequency
wordFreq2 <- sort(wordFreq2, decreasing=TRUE)

# Examine the top ten words
cbind(wordFreq2[1:10])

# frequent terms
findFreqTerms(tdm2, lowfreq=15)

# associations
findAssocs(tdm2, "stock", 0.4) 

################################

# word cloud stock gainers data

library(wordcloud)

palette <- brewer.pal(8,"Dark2")
palette

set.seed(137)
wordcloud(words=names(wordFreq1), 
          freq=wordFreq1, 
          min.freq=3, 
          random.order=F,
          colors=palette)
#############

# word cloud stock losers data

set.seed(137)
wordcloud(words=names(wordFreq2), 
          freq=wordFreq2, 
          min.freq=3, 
          random.order=F,
          colors=palette)

#########################
#### Sentiment Analysis

sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}

# Lexicons
pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

head(pos.words)

head(neg.words)

###################

######### Sentiment score for stock gainers dataset
sink(tempfile())
scores1 <- sapply(tweets.gainersTxt, 
                    sentiment, 
                    pos.words, neg.words)
sink()

table(scores1)

barplot(table(scores1), 
        xlab="Score", ylab="Count",
        col="cyan")

######### Sentiment score for stock losers dataset
sink(tempfile())
scores2 <- sapply(tweets.losersTxt, 
                  sentiment, 
                  pos.words, neg.words)
sink()

table(scores2)

barplot(table(scores2), 
        xlab="Score", ylab="Count",
        col="cyan")

##########
sentiment.na <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0)
    return (NA)
  else
    return (p - n)
}

scores.na <- sapply(tweets.losersTxt, 
                    sentiment.na, 
                    pos.words, neg.words)

table(scores.na)

barplot(table(scores.na), 
        xlab="Score", ylab="Count",
        ylim=c(0,40), col="cyan")

###############
scores.na <- sapply(tweets.gainersTxt, 
                    sentiment.na, 
                    pos.words, neg.words)

table(scores.na)

barplot(table(scores.na), 
        xlab="Score", ylab="Count",
        ylim=c(0,90), col="cyan")
###############

# Data frame of scores and tweets

# mbta.vector <- sapply(mbta.texts,
#                       function (t) {(t)})
# x <- data.frame(Score=scores.na, Text=mbta.vector)
# View(x)

######################

setwd("<<REPLACE WITH PATH>>")
stock.data <- read.csv("GainerStock.csv")

library(devtools)
library(googleVis)

chart1 <- 
  gvisLineChart(stock.data,
                "ï..Stock", 
                c("Open","Close"),
                options=list(
                  legend="top",
                  title="gainer stock price on the day 0ct12/2017"))
plot(chart1) 

chart2 <- gvisColumnChart(stock.data,
                          xvar="ï..Stock", 
                          yvar=c("Open","Close"),
                          options=list(
                            legend="top",
                            title="gainer stock price on the day 0ct12/2017"))
plot(chart2)

chart3 <- gvisMerge(chart1, chart2, horizontal = TRUE)

plot(chart3)
