install.packages("tm")
install.packages("wordcloud")
install.packages("syuzhet")

#load packages into R
library(tm)   #text analysis-text mining
library(wordcloud)  #create wordcloud
library(syuzhet)

#import data into R
reviews <- read.csv("reviews.csv")


#check the structure of data
str(reviews)

#creating corpus
#This function uses the base package function iconv to translate value labels into a specified
corpus <- iconv(reviews$text)
corpus <- Corpus(VectorSource(corpus))

#To see the corpus
inspect(corpus[1:5])

#Cleaning corpus
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)

corpus <- tm_map(corpus,removeNumbers)

corpus <- tm_map(corpus,removeWords,stopwords("english"))

corpus <- tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

reviews_final <- corpus

#Create term document
tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:10, 1:5]


#bar plot of words
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,las =2,col = "blue")

#create word Cloud
w <- sort(rowSums(tdm),decreasing = T)
set.seed(2000)
wordcloud(words = names(w),
          freq = w,
          max.words = 50,
          random.order = T,
          min.freq = 5,
          colors = brewer.pal(25,"Dark2"),
          scale = c(3,0.3))

#obtain sentiment scores
sentiment_data <- iconv(reviews$text)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

#calculate review wise score
s$score <- s$positive - s$negative
s[1:10]

#write scores into a csv file
write.csv(x=s,file = "C:/Users/hp/Desktop/final.xlsx")

#check product sentiment
review_score <- colSums(s[,])
print(review_score)


#plot product sentiment
barplot(colSums(s),
        las=2,
        col = rainbow(10),
        ylab = "Count",
        main = "Sentiment")
 

