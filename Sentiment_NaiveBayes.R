setwd("/Users/alinazhiltsova/Desktop/NCI/Data Mining/")
reviews <- read.csv(file = "yelp_review.csv", header = TRUE, sep = ",")   
business <- read.csv(file= "yelp_business.csv", header = TRUE, sep = ",")
business$is_restaurant = grepl("Restaurants", business$categories)
business = subset(business, is_restaurant == TRUE)
business[144:147,]
restaurant_reviews <- reviews[which(reviews$business_id %in% business$business_id),]

write.csv(restaurant_reviews[1:10000,], file = "subset.csv")

reviews <- read.csv(file = "subset.csv", header = TRUE, sep = ",")
#deleting useless columns 
reviews$user_id <- NULL
reviews$date <- NULL
reviews$cool <- NULL
reviews$funny <- NULL
reviews$useful <- NULL
reviews$X <- NULL
reviews$business_id <-NULL
reviews$review_id <- NULL

reviews$text <- as.character(reviews$text)


fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is', 'has' or could be possessive: it has no expansion, so we are removing this
  doc <- gsub("'s", "", doc)
  return(doc)
}

#removing contractions
reviews$text <- sapply(reviews$text, fix.contractions)



#removing punctuation and all that
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
reviews$text <- sapply(reviews$text, removeSpecialChars)
reviews$text<- sapply(reviews$text, tolower)

#install.packages("textcat") for labelling the text - we want only English
library(textcat)

#assigning a language (it takes a whlie)
reviews$language <- textcat(reviews$text) 
#factorizing the language
reviews$language <- as.factor(reviews$language) 
#deleting reviews that are not english. the library treats some english as scots, so we keeping it too
reviews <- subset(reviews, language =='english' | language =='scots') 
#we don't need the language anymore as it's done its job - we have deleted non-english reviews
reviews$language <- NULL 

# Sentiment comes into play
#there are different ways to do this, we are assigning a numeric sentiment based on a sentence.
#the sentiment is unbounded (+-infinity), + is positive, - is negative
library(sentimentr)
text <- get_sentences(reviews$text) #we are assigning the sentiment based on sentences, because of the nature of reviews
sent <- sentiment(text)
reviews$sentiment <- sent$sentiment

#let's see what we have
hist(reviews$sentiment)

#binning the numbers to binary
reviews$sentBinned1 <- cut(reviews$sentiment,
                           breaks = c(-3,0,max(reviews$sentiment)),
                           labels = c( "negative","positive"))

#we'll bin the stars here to compare with the sentiment
#since sentiment is either positive or negative, we'll do the same for stars
reviews$starsBinned1 <- cut(reviews$stars,
                           breaks = c(1,3,max(reviews$stars)),
                           labels = c( "negative","positive"))

#let's compare these! :)
plot(reviews$sentBinned1)
plot(reviews$starsBinned1)
#they are very different


#now let's build a classifier so that Yelp could work with it
library(tm)
library(dplyr)
corpus <- VCorpus(VectorSource(reviews$text))

corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removePunctuation)) %>%
  tm_map(content_transformer(removeNumbers)) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(content_transformer(stripWhitespace))


dtm <- DocumentTermMatrix(corpus.clean)

reviews_sent <- reviews[,c(2,4)]
reviews_sent$length <- sent$word_count
reviews_sent$stars <-reviews$stars 
str(reviews_sent)

confusionMatrix(reviews$sentBinned1, reviews$starsBinned1)

library(caret)
sample <- createDataPartition(reviews_sent$sentBinned1, p = .7, list = FALSE) 

reviews.train <- reviews_sent[sample,]
reviews.test <- reviews_sent[-sample,]

table(reviews.train$sentBinned1)
6022/nrow(reviews.train)

dtm.train <- dtm[sample,]
dtm.test <- dtm[-sample,]

corpus.clean.train <- corpus.clean[sample]
corpus.clean.test <- corpus.clean[-sample]

dim(dtm.train)
#6878 25048

#we'll remove all the words that appear fewer than 10 times
tenfreq <- findFreqTerms(dtm.train, 10)
length((ffreq))

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = tenfreq))
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = tenfreq))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}


trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

library(e1071)
classifier <- naiveBayes(trainNB, reviews.train$sentBinned1, laplace = 1) 
# 6.096   3.887  11.087 
pred <- predict(classifier, newdata=testNB) 
#316.015  25.331 359.751 


confusm1 <- confusionMatrix(pred, reviews.test$sentBinned1)
confusm1
#accuracy 0.85

#remove words that appear fewer than 5 times
ffreq <- findFreqTerms(dtm.train, 5)
length((ffreq))


dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = ffreq))
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = ffreq))


trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

classifier <- naiveBayes(trainNB, reviews.train$sentBinned1, laplace = 1) 
# 6.096   3.887  11.087 
pred <- predict(classifier, newdata=testNB) 
#316.015  25.331 359.751 



dim(dtm.train.nb)

confusm <- confusionMatrix(pred, reviews.test$sentBinned1)
confusm
#accuracy 0.88



#now we need to see what data gets misclassified
reviews.test$pred <- pred

#if the classification is correct we'll see TRUE in the column
reviews.test$isSame <- (reviews.test$pred == reviews.test$sentBinned1)


#we'll divide the classified and misclassified reviews into two dfs to do some eda and see the reason
misclassified = subset(reviews.test, isSame == FALSE)
classified = subset(reviews.test, isSame == TRUE)

hist(classified$stars)
# not so many 1-, 2-stared reviews, a lot of 5-star reviews
hist(misclassified$stars)
#mostly 1- and 2-stared reviews


mean(classified$length)
#135.1012
mean(misclassified$length)
#131.2446

#let's EDA this 

#turn everything into words and remove stop words
library(tidytext)
misclassified1 <- misclassified %>%
  unnest_tokens(word, text)
data(stop_words)

misclassified1 <- misclassified1 %>%
  anti_join(stop_words)

#see what words are most popular
misclassified1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#same for classified

classified1 <- classified %>%
  unnest_tokens(word, text)

classified1 <- classified1 %>%
  anti_join(stop_words)

classified1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#ok, words are mostly the same, but it's most likely because of the sample size



table(reviews.test$isSame)
