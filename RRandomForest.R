library(data.table)
reviewdf <- fread('C:/Users/Jahnavi/Documents/ADM/yelp_review.csv')
businessdf <- read.csv('C:/Users/Jahnavi/Documents/ADM/yelp_business.csv')

library(tictoc)
library(ggplot2)
library(zoo)
library(plyr)
library(igraph)
library(tm)
library(SnowballC)
library(textcat)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(wordcloud)

#keep only required columns
reviewdf <- subset(reviewdf, select=c(3,4,5,6))
businessdf <- subset(businessdf, select=c(1,2,5,10,11,13))

#clean columns
reviewdf$date=as.Date.character(reviewdf$date, tryFormats = c("%Y-%m-%d"))
reviewdf$stars=as.numeric(reviewdf$stars)
reviewdf$text=as.character(reviewdf$text)
businessdf$is_restaurant = grepl("Restaurants", businessdf$categories)
businessdf = subset(businessdf, is_restaurant == TRUE)
businessdf[144:147,]
reivew_rest <- reviewdf[which(reviewdf$business_id %in% businessdf$business_id),]


#EDA

#clustering
#subset to include cities
reviewsClus <- fread('C:/Users/Jahnavi/Documents/ADM/yelp_review.csv')
businessClus <- read.csv('C:/Users/Jahnavi/Documents/ADM/yelp_business.csv')

save(businessClus,file = "businessClus.Rdata")

torontobiz <- businessClus[which(businessClus$city=="Toronto"), ]
nrow(torontobiz)

onReview <- reviewsClus[ which(reviewsClus$business_id %in% torontobiz$business_id), ]
nrow(onReview)

clus_ReviewTor <- onReview[0:20000,]

dataClust = data.frame(clus_ReviewTor$business_id, clus_ReviewTor$user_id)

count_clus <- count(dataClust, "clus_ReviewTor.business_id")
head(count_clus, n=10)

graphed_data <- graph.data.frame(dataClust, directed=F)
degrees <- degree(graphed_data)
plot(degrees, main="Number of Edges Per Vertex", sub="Shared locations per user")

graph <- graph_from_data_frame(dataClust)
clusters <- cluster_walktrap(graph)
plot(clusters, graph, vertex.shape="none", vertex.color=NA, vertex.label=NA, edge.lty="blank", edge.arrow.mode=0, main="Clustering of User Reviews Based on Business IDs", sub="Cluster Walktrap Method")

#look at reviews of business
businessdf$review_count=as.numeric(businessdf$review_count)
summary(businessdf$review_count)

#visual
ggplot(businessdf, aes(x="review_count", y=review_count))+
  geom_boxplot( col="red") + 
  ggtitle("Review Counts") +
  xlab("") + ylab("Review Count") +
  theme_minimal()

businessdf_subset = subset(businessdf, review_count > 1000)
ggplot(businessdf_subset, aes(x=review_count))+
  geom_histogram( fill="red") + 
  ggtitle("Review Counts") +
  xlab("") + ylab("Review Count") +
  theme_minimal()

businessdf_top = subset(businessdf, review_count > 5000)
businessdf_top
#average reviews a rest ha
mean(businessdf$review_count)
#stars reviews distri
ggplot(reviewdf, aes(x=stars))+
  geom_bar(stat="bin", bins= 9, fill="red") + 
  
  ggtitle("Star Counts Reviews") +
  xlab("Stars") + ylab("Count") +
  theme_minimal()

#Text Analysis
#sample the reviews to check language distribution
reivew_rest = reivew_rest[1:3000,]
nrow(reivew_rest)

#setting up for sentiment using 1/2 as negitive and 4/5 as positive
#removing 3 star reviews
reivew_rest = subset(reivew_rest, stars != 3)

#create a binary variable for the sentiment
reivew_rest$positive = as.factor(reivew_rest$stars > 3)
#check what reviews are positive or not
table(reivew_rest$positive)

#bag of words apporach el is a simple algorithm used in Natural Language Processing. In BoW model a sentence or a document is considered as a 'Bag' containing words. It will take into account the words and their frequency of occurrence in the sentence
#ransforming it into lowercase, removing the puntuation and the common English words ( such as "this", "and"
corpus = VCorpus(VectorSource(reivew_rest$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords,stopwords("english"))
corpus = tm_map(corpus, stemDocument)

#checking one of the reviews
corpus[[1]]$content

#building Document Term Matrix
#each term(i.e. each word from our reviews) is a column, and each review is a row, with corresponding values for the number of times each term appears in each review.
#We then remove infrequent terms and focus on the ones that appear in multiple reviews.
frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.99)
reviewsSparse = as.data.frame(as.matrix(sparse))
colnames(reviewsSparse) = make.names(colnames(reviewsSparse))

#check the result
reviewsSparse$positive = reivew_rest$positive

#checking one of the reviews
corpus[[1]]$content

#building Document Term Matrix
#each term(i.e. each word from our reviews) is a column, and each review is a row, with corresponding values for the number of times each term appears in each review.
#We then remove infrequent terms and focus on the ones that appear in multiple reviews.
frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.99)
reviewsSparse = as.data.frame(as.matrix(sparse))
colnames(reviewsSparse) = make.names(colnames(reviewsSparse))

#check the result
reviewsSparse$positive = reivew_rest$positive

#Model Preparation
split = sample.split(reviewsSparse$positive, SplitRatio = 0.70)
reviewsSparse$split = split
train = subset(reviewsSparse, split==TRUE)
test = subset(reviewsSparse, split==FALSE)

#randomForest

library(randomForest)

model1 <- randomForest(positive ~ ., data=train, importance = TRUE)
model1

#Variable importance
varImpPlot(model1)

# Fine tuning parameters of Random Forest model
#number of trees = 500, split = 32, error rate = 17.29

model2 <- randomForest(positive ~ ., data = train, ntree = 500, mtry = 200, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, train, type = "class")
# Checking classification accuracy
table(predTrain, train$positive) 

# Predicting on test set
predtest <- predict(model2, test, type = "class")
# Checking classification accuracy
mean(predtest == test$positive)   #82.33% accuracy                 
table(predtest,test$positive)     #53 missclassified

#10 cross fold validation

library(mlbench)

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
  metric <- "Accuracy"
  grid_rf<-expand.grid(mtry=c(8,16, 30,34, 38, 44))
  
rf_default <- train(positive  ~., 
                    data= train, 
                    method='rf', 
                    metric='Accuracy', ntree= 1000, 
                    tuneGrid=grid_rf, 
                    trControl=control)
print(rf_default)
#best model using mtry =16
save(rf_default, file = "tunedRFmodel.Rdata")

##RF tuned model with mtry = 16
rf_tuned_v1 <- randomForest(positive ~ ., data = train, ntree = 1000, mtry = 16, importance = TRUE)

#now train using params
predTest_V1 <- predict(rf_tuned_v1, test)
caret::confusionMatrix(predTest_V1, test$positive)


save(rf_tuned_v1, file = "mtry16tunedRFmodel.Rdata")

