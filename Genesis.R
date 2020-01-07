## Working with Genesis Reviews ##

#1. Reading Data File
genesis <- read.csv("Scraped_Car_Review_genesis.csv")
head(genesis)
str(genesis)

#clean the text of special characters such as symbols and emoticons
genesis$Review <- sapply(genesis$Review,function(row) iconv(row, "latin1", "ASCII", sub=""))

#2. Building Corpus
install.packages('tm')
install.packages('NLP')
library(tm)
library(NLP)
corpus <-iconv(genesis$Review, to='utf-8-mac') #need only the first col text from file
corpus <- Corpus(VectorSource(corpus)) #corpus is a collection of texts
inspect(corpus[1:5]) #inspect the first five reviews

#3. Cleaning Data
#convert data to lower case for analysis
corpus <-tm_map(corpus, tolower) #convert all alphabet to lower case
inspect(corpus[1:5]) #inspect the first five reviews

#remove punctuations
corpus <-tm_map(corpus, removePunctuation)
inspect(corpus[1:5]) #inspect the first five reviews

#remove numbers
corpus <-tm_map(corpus, removeNumbers)
inspect(corpus[1:5]) #inspect the first five reviews

#remove common words-they dont add any informational value
#use the stopwords function in english
#select stopwords(english) to see what words are removed
cleanset <-tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#remove URLs (https://etc.)
#make use of function http
removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
cleanset <-tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:100])

#products from the text that aren't meaningful
cleanset <-tm_map(cleanset, removeWords, c('genesis', 'g', 'car'))

#remove white spaces
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#lets now provide some structure to tweets by creating a matrix of rows/coloums
#this is called term document matrix (tdm)
#Create term document matrix

tdm <- TermDocumentMatrix(cleanset)
tdm 

tdm <- as.matrix(tdm)
tdm[1:40, 1:20]

###VISUALIZE TEXT DATA
w <- rowSums(tdm)
w <- subset(w, w>=30)
barplot(w, las = 2, col=rainbow(40), main = "Frequency of Words in Genesis Reviews")

Word Cloud
install.packages('wordcloud')
install.packages('RColorBrewer')
library(wordcloud)
library(RColorBrewer)
w <- sort(rowSums(tdm), decreasing=TRUE) #sort words in decreasing order
set.seed(9999)
wordcloud(words = names(w), 
          freq=w, max.words = 300, 
          random.order =FALSE)  #words are specified in names in w dataframe, frequency is stored in w, random order=false

wordcloud(words = names(w), 
          freq=w, 
          random.order =FALSE,
          max.words = 100, 
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'), 
          scale = c(2.8, 0.1), 
          rot.per = .3) 

#SENTIMENT ANALYSIS USING R
#load packages
install.packages('syuzhet')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('scales')
install.packages('dplyr')
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

#Reading Files
genesis_sent <- iconv(genesis$Review, to="utf-8-mac")

#obtain sentiment scores for each 1000 tweets
s <-get_nrc_sentiment(genesis_sent)
head(s) 

#plot sentiment scores
barplot(colSums(s), 
        las = 2,
        ylab = 'Total Count', 
        main ='Sentiment Scores for Genesis Reviews')

####SOCIAL NETWORK ANALYSIS###
install.packages('igraph')
library(igraph)

tdm[tdm>1] <-1 
termM <-tdm %*% t(tdm)
termM[1:10, 1:10]
g <- graph.adjacency(termM, weighted=T, mode ='undirected') #convert it into graph, no direction for edges
g

#remove terms that have loops (going to self) 
g <- simplify(g)

#set labels and degrees of Vertices (V), each word is a vertices
V(g)$label <- V(g)$name #label is name
V(g)$label

V(g)$degree <- degree(g) #degree is the number of connections between terms
V(g)$degree

#Histogram of node degree, lets just use 100 bars (too many words), label of y and x axis
hist(V(g)$degree, 
     breaks=100, 
     col='green', 
     main ='histogram of node degree', 
     ylab ='frequency',
     xlab='degree of vertices') #right skewed 

#Network diagram
set.seed(9999)
plot(g) 

tdm <- tdm[rowSums(tdm)>30,]

tdm[tdm>1] <-1 
termM <-tdm %*% t(tdm)
g <- graph.adjacency(termM, weighted=T, mode ='undirected')
g <- simplify(g)
V(g)$label <- V(g)$name 
V(g)$degree <- degree(g)

#clustering on betweeness
comm <- cluster_edge_betweenness(g)
plot(comm, g)

#Label Propogation
prop <-cluster_label_prop(g)
plot(prop, g)
