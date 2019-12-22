library(dplyr)

business_yelp <- read.csv(file = "yelp_business.csv", stringsAsFactors = T)
save(business_yelp, file = "business_yelp.Rdata")

business_attributes_yelp <- read.csv(file = "yelp_business_attributes.csv", stringsAsFactors = T)
save(business_attributes_yelp, file = "business_attributes_yelp.Rdata")

user_yelp <- read.csv(file = "yelp_user.csv", stringsAsFactors = T)
save(user_yelp, file = "user_yelp.Rdata")

checkin_yelp <- read.csv(file = "yelp_checkin.csv", stringsAsFactors = T)
save(checkin_yelp, file = "checkin_yelp.Rdata")

review_yelp <- read.csv(file = "yelp_review.csv", stringsAsFactors = F)
save(review_yelp, file = "review_yelp.Rdata")

# lets filer check-in data with business df to confirm which all checkin exist in business_yelp

checkin_vs_business_df <- checkin_yelp %>% 
                            filter(business_id %in% business_yelp$business_id)

tail(checkin_vs_business_df)

#look at user_yelp
colnames(user_yelp)

mean(user_yelp$review_count)

which.max(user_yelp$review_count)

head(sort(user_yelp$review_count,decreasing = T))

max_review_user <- user_yelp[1308092,]

#filter reviews of business present in business_yelp

review_vs_business <- review_yelp %>% filter(business_id %in% business_yelp$business_id)

#distribution of reviews of users

summary(user_yelp$review_count)
sort(user_yelp$review_count, decreasing = T)

#format date column 

#business_yelp
leastReviewed_business <- tail(arrange(business_yelp,desc(review_count)), n = 100)

#review_yelp
leastReviewed_business_reviews <- review_yelp %>% filter(business_id %in% leastReviewed_business$business_id)

#format as date type
leastReviewed_business_reviews$date <- as.Date(leastReviewed_business_reviews$date, format = "%Y-%m-%d")

#extract month & day & year info                                    
leastReviewed_business_reviews <- leastReviewed_business_reviews %>% mutate(month = format(date, "%m"), year = format(date, "%Y"), day = format(date, "%d"))

#checkin_yelp

leastReviewed_business_checkins <- checkin_yelp %>% filter(business_id %in% leastReviewed_business$business_id)

#group reviews
leastReviewed_business_reviews_group_ymd <- leastReviewed_business_reviews %>% group_by(business_id, date, year, month, day) %>% summarize(total = n())


#df_review_date  <-  df_super_review  %>% select('user_id', 'business_id', 'review_date')

# elite are top performing /active users as told by yelp
elite_users <- user_yelp[user_yelp$elite != "None",]

elite_users <- elite_users[!duplicated(elite_users),]

#remove friend column

elite_users <- elite_users[,-5]

#get reviews of elite users

elite_users_reviews <- review_yelp %>% filter(user_id %in% elite_users$user_id)

#get reviews of only elite users for least reviewed businesses
elite_users_reviews_for_leastreviews <- leastReviewed_business_reviews %>% filter(user_id %in% elite_users_reviews$user_id)

#businesses reviwewd by elite users

elite_users_business <- business_yelp %>% filter(business_id %in% elite_users_reviews$business_id)

elite_users_business <- elite_users_business[!duplicated(elite_users_business),]

#elite user checkins

elite_users_business_checkins <- checkin_yelp %>% filter(business_id %in% elite_users_business$business_id)


#format as date type
elite_users_reviews$date <- as.Date(elite_users_reviews$date, format = "%Y-%m-%d")

#extract month & day & year info                                    
elite_users_reviews <- elite_users_reviews %>% mutate(month = format(date, "%m"), year = format(date, "%Y"), day = format(date, "%d"))


#group reviews of elite users by ymd
elite_users_reviews_group_ymd <- elite_users_reviews %>% group_by(business_id, date, year, month, day) %>% summarize(total = n())

elite_users_reviews_group_ym <- elite_users_reviews %>% group_by(business_id, year, month) %>% summarize(total = n())





elite_users_reviews_group_ym <- elite_users_reviews %>% group_by(business_id, year, month) %>% summarize(total = n())

library(ggplot2)
leastReviewed_business_checkins %>% ggplot(aes(x = weekday, y = checkins)) + geom_point() + geom_smooth()

elite_users_business_checkins_groupby_weekdayhr %>% ggplot(aes(x = hour, y = total)) + geom_point() + geom_smooth()

boxplot(elite_users_business_checkins_groupby_weekdayhr$total ~ elite_users_business_checkins_groupby_weekdayhr$weekday)


elite_users_business_checkins_groupby_weekdayhr <- elite_users_business_checkins %>% group_by(weekday,hour) %>% summarize(total = n())




####### analysis

review_and_users <- inner_join(review_yelp, user_yelp, by = "user_id")
rev_and_users_and_business <- inner_join(review_and_users, business_yelp, by = "business_id")


#no of chinese restro, with chineses in catg
sum(grepl("Chinese", rev_and_users_and_business$categories)) #198635

#no of indian , with indian in catg
sum(grepl("Indian", rev_and_users_and_business$categories))   #61006

rev_and_users_and_business$is_chinese <- grepl("Chinese", rev_and_users_and_business$categories) == TRUE


#make a df for chinese restro

chinese_restro <- subset(rev_and_users_and_business, is_chinese == TRUE)

# summary of reviews of chinese cuisine done by each reviewer
num_reviews_Chinese <- chinese_restro %>% select(user_id, name.x, is_chinese) %>%
  group_by(user_id) %>% 
  summarise(total_reviews = sum(is_chinese))

table(num_reviews_Chinese$total_reviews)
count(num_reviews_Chinese)
mean(num_reviews_Chinese$total_reviews)
summary(num_reviews_Chinese$total_reviews)


#######################
library(ggvis)
# create a histogram of review counts
business_yelp %>% ggvis(~review_count, fill := "#fff8dc") %>%
  layer_histograms(width = 20) %>%
  add_axis("x", title = "Number of reviews") %>%
  add_axis("y", title = "Number of businesses")

# City distribution businesses using ddplyr


business_city_distr <- business_yelp %>% group_by(city) %>% summarise(length(city))

#which cities most business 
  business_city_distr<- business_city_distr[order(-business_city_distr$`length(city)`),]                                                                 
  
  
########  user yelp #######
#  graph the users' stars density plot
  
star_density_plot <- ggplot(user_yelp, aes(x= average_stars, y =..density..)) + 
  geom_density(fill = "red", adjust =2)+
  ggtitle("Density of users' stars") 
  
#  plot for users' review count density

ggplot(user_yelp, aes(x= review_count, y =..density..)) + 
geom_density(fill = "red", adjust =2)+
ggtitle("Density of users' review count")
  
# review count plot is not clear, seem to have big outliers .

#inspect boxplot for outlier
ggplot(user_yelp, aes(y= review_count)) + 
  geom_boxplot()+
  ggtitle("Boxplot of users' review count")
summary(user_info$review_count)

boxplot(user_yelp$review_count)

#lot of outliers , we need to remove them

length(boxplot.stats(user_yelp$review_count)$out)
#backup
rmOutliers_user_yelp <- user_yelp
source("https://datascienceplus.com/rscript/outlier.R")

#shows graphs to see what happends after outlier removal & puts outlier as NA
outlier_users <- outlierKD(rmOutliers_user_yelp, review_count)

#remove outliers
rmOutliers_user_yelp <-  na.omit(rmOutliers_user_yelp)

summary(rmOutliers_user_yelp$review_count)

#see boxplot now, looks much better than ealier one
library(ggplot2)
ggplot(rmOutliers_user_yelp, aes(y= review_count)) + 
  geom_boxplot()

ggplot(rmOutliers_user_yelp, aes(x= review_count, y =..density..)) + 
  geom_density(fill = "red", adjust =2)+
  ggtitle("Density of users' review count after outliers rm")
#density plot is fixed now after outlier remove

#feature extraction : number of years yelping
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
pacman::p_load(knitr,ggplot2, tidyverse, dplyr, ggraph, stringr, wordcloud, tidytext,tidyr,lubridate, widyr,jsonlite,sentimentr, benford.analysis, magrittr,wordcloud2, webshot ,htmlwidgets)
webshot::install_phantomjs()

rmOutliers_user_yelp$years_yelping <- 2018 - year(rmOutliers_user_yelp$yelping_since)

unique(rmOutliers_user_yelp$years_yelping)
#   5  1  3  2  6  8  7  4 10  9 11 12 14 13

#lets bin the years_yelping into 4 factor level

rmOutliers_user_yelp$years_yelping_level <- cut(rmOutliers_user_yelp$years_yelping, breaks = c(0,3,6,9,14), labels = c("0-2","3-5","6-9","10-14"))

yelping_years_range <- table(rmOutliers_user_yelp$years_yelping_level)

#histogram of distributon of yelping experiece in years
barplot(table(rmOutliers_user_yelp$years_yelping_level), ylab = "no of yelp users", xlab = "Years of yelping", ylim = c(5000,500000))
 

sub_plot <- rmOutliers_user_yelp %>% select(c(user_id,review_count, average_stars,years_yelping_level))

#density plot of reviews and years yelping year
ggplot(sub_plot, aes(x= review_count, y =..density.. )) + 
  geom_density(fill = "pink", color =NA, adjust =2)+
  facet_grid(years_yelping_level ~.)+
  ggtitle("Density of users' review count by yelping year")

#stars vs yelping years plot

ggplot(sub_plot, aes(x= average_stars, y =..density.. ,group = years_yelping_level)) + 
  geom_density(fill = "salmon", colour = NA, adjust = 2)+
  ggtitle("Density of users' stars by yelping year")

#filter reviews based on stars == 1

review.star1 <- review_yelp %>% filter(stars == 1)

#most common 2 grams in review for 1 star
review.star1.count <-review.star1[1:100000,] %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  transmute(bigram = str_extract(bigram , "[a-z'\\s]+"))  %>%
  separate(bigram, c("word1", "word2", sep = " ")) %>%
  filter(!word1 %in% stop_words$word ,
         !word1 %in% "[\\s]+",
         !word1 %in% "") %>%
  filter(!word2 %in% stop_words$word,
         !word2 %in% "[\\s]+",
         !word2 %in% "")%>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2 ,sep = " ")

save(review.star1.count, file = "review.star1.count.Rdata")

review_star1_wordcloud <- wordcloud(review.star1.count$bigram, review.star1.count$n, max.words = 250, color = "red")
review_star1_wordcloud2 <- wordcloud2(review.star1.count, shape = 'circle', color = "darkorchid")


#filter reviews based on stars == 5

review.star5 <- review_yelp %>% filter(stars == 5)

review.star5.count <- review.star5[1:200000,] %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  transmute(bigram = str_extract(bigram , "[a-z'\\s]+"))  %>%
  separate(bigram, c("word1", "word2", sep = " ")) %>%
  filter(!word1 %in% stop_words$word ,
         !word1 %in% "[\\s]+",
         !word1 %in% "") %>%
  filter(!word2 %in% stop_words$word,
         !word2 %in% "[\\s]+",
         !word2 %in% "")%>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2 ,sep = " ")


save(review.star5.count, file = "review.star5.count.Rdata")

review.star5_wordcloud <- wordcloud2(review.star5.count, shape = 'circle', color = "tomato",size=.8)

#####join businees attrributes file + business_info

businessyelp_JOIN_busnATTR <- inner_join(business_yelp, business_attributes_yelp, by = "business_id")
save(businessyelp_JOIN_busnATTR, file = "businessyelp_JOIN_busnATTR.Rdata")

#########filter restaurnts & non-restro
restro <- (businessyelp_JOIN_busnATTR %>% filter(str_detect(categories,"Restaurants")) )
index_restro <- which(restro$business_id %in% businessyelp_JOIN_busnATTR$business_id)

non_restro <- businessyelp_JOIN_busnATTR[-index_restro,]


restro_2 <- (non_restro %>% filter(str_detect(categories,"Restaurants")) )
index_restro_2 <- which(restro_2$business_id %in% non_restro$business_id)

non_restro_2 <- non_restro[-index_restro_2,]

index_of_restro <- str_detect(businessyelp_JOIN_busnATTR$categories,"Restaurants")

businessyelp_JOIN_busnATTR$categories <- as.character(businessyelp_JOIN_busnATTR$categories)
########## didnt work as expected

##categories split

categories <- str_split(business_yelp$categories,";")
categories <- as.data.frame(unlist(categories))
colnames(categories) = c("Category Name")

#######
######### what causes business closures? ########
library(spatstat)

sum(is.na(business_yelp))
sapply(business_yelp, function(x) sum(is.empty(x)))

str(business_yelp)

#distribution of open/closed businessses
counts <-table(business_yelp$is_open)
is_open_business <- barplot(counts, main = "Is business open ?", xlab = "0 : Closed, 1: Open", ylab = "Number of businesses")

#distribution of stars in business
counts_stars <- table(business_yelp$stars)
barplot(counts_stars, main = "Number of stars", xlab = "Stars")
#most business have rating of 3 and above

#visualise reviews using log1p as skewed
hist(log1p(business_yelp$review_count))

#business by state eda
state_wise_busn <- business_yelp %>% group_by(state) %>% count()

library(treemap)
treemap(state_wise_busn, index = c("state"), vSize = "n", title = "Business distribution by STATE")

#### is_open is imbalanced
prop.table(table(business_yelp$is_open))*100

# 16 % business closed / 84 % open
miss_idx <- which(is.na(business_yelp$latitude))
miss_idx2 <- which(is.na(business_yelp$longitude))

business_yelp <- business_yelp[-miss_idx,]
#removed missing value row for lat/long


# build model to predict is_open from lat/long/stars/review_count logistic reg model

#use SMOTE for imbalance

library(DMwR)
data_SMOTE <- business_yelp[,c(8,9,10,11,12)]
SMOTED_business_yelp <-DMwR::SMOTE(is_open ~.,data_SMOTE)

save(SMOTED_business_yelp, file ="SMOTED_business_yelp.Rdata")
load("SMOTED_business_yelp.Rdata")
table(SMOTED_business_yelp$is_open)
# more balanced class now for is_open

#make train and test
library(caret)
sample <- createDataPartition(SMOTED_business_yelp$is_open, p = .70, list = FALSE)
train <- SMOTED_business_yelp[sample,]
test <- SMOTED_business_yelp[-sample,]

#model
library(LiblineaR)
# Center and scale data
scaled_train=scale(train[-5],center=TRUE,scale=TRUE)
# Find the best model with the best cost parameter via 10-fold cross-validations
tryTypes=c(0:7)
tryCosts=c(0.0001,0.001,0.01,0.1,1,10)
bestCost=NA
bestAcc=0
bestType=NA
for(ty in tryTypes){
  for(co in tryCosts){
    acc=LiblineaR(data=scaled_train,target=train$is_open,type=ty,cost=co,bias=1,cross=5,verbose=FALSE)
    cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
    if(acc>bestAcc){
      bestCost=co
      bestAcc=acc
      bestType=ty
    }
  }
}

#best accuracy for C=0.001

#train model
model <- LiblineaR(data=scaled_train,target=train$is_open,cost=.001,bias=1,verbose=FALSE)
model_nonscaled <- LiblineaR(data=train[-5],target=train$is_open,cost=.001,bias=1,verbose=FALSE)

#loading of rdata
load("SMOTED_business_yelp.Rdata")
load("review_yelp.Rdata")
load("user_yelp.Rdata")
load("checkin_yelp.Rdata")
load("business_yelp.Rdata")

# Scale the test data
scaled_test <- scale(test[-5],center=TRUE,scale=TRUE)

#prediction
#predict(model, newdata= scaled_test)

p=  predict(model,scaled_test,proba=TRUE,decisionValues=TRUE)
p_nonscaled =  predict(model_nonscaled,test[-5],proba=TRUE,decisionValues=TRUE)
#confusion matrix

confusionMatrix(p$predictions, test$is_open)
confusionMatrix(p_nonscaled$predictions, test$is_open)

#accuracy =56%

#glm logit model

model_glm <- glm(is_open ~., data = train, family = binomial(link = logit))
model_glm_nonscale <- glm(is_open ~., data = train, family = binomial(link = logit))

summary(model_glm)
anova(model_glm, test = "Chisq")



fitted.results <- predict(model_glm, newdata =test[-5], type = "response")

fitted.results <- ifelse(fitted.results > 0.5,1,0)

fitted.results <- as.factor(fitted.results)
caret::confusionMatrix(data = fitted.results, reference = test$is_open)


###not great accuracy of 56%
## need more features

#group reviews by business_id
library(dplyr)
review_groupby_business_count <- review_yelp %>% group_by(business_id)  %>% count() 

review_groupby_business <- review_yelp %>% group_by(business_id) %>% summarise(mean(stars), median(stars))

review_groupby_business <- cbind(review_groupby_business,review_groupby_business_count$n)

names(review_groupby_business) <- c("business_id","mean_Stars","median_Stars","Number_of_Reviews")

# a succesul business must ve more checkins, look at checkin data

load("checkin_yelp.Rdata")

checkin_by_businessid <- checkin_yelp %>% group_by(business_id) %>% count()

review_groupby_business <- plyr::join(review_groupby_business, checkin_by_businessid, type = "left")

library(plyr)


names(review_groupby_business)[5] <- "Number of Checkins"

#merge with yelp_business

business_yelp_merged <- plyr::join(business_yelp, review_groupby_business, type = "left")

business_yelp_merged_model <- business_yelp_merged[,c(10,11,12,14,15,16,17)]

#check missing
sapply(business_yelp_merged_model, function(x) sum(is.na(x)))

#replace NA in checkins with 0
business_yelp_merged_model[is.na(business_yelp_merged_model)] <- 0


#smote again as imbalance

data_SMOTE_2 <- business_yelp_merged_model

data_SMOTE_2$is_open <- as.factor(data_SMOTE_2$is_open)

SMOTED_business_yelp_v2 <-DMwR::SMOTE(is_open ~.,data_SMOTE_2)

table(SMOTED_business_yelp_v2$is_open) #imbalanace better now with smote

save(SMOTED_business_yelp_v2, file ="SMOTED_business_yelp_v2.Rdata")
load("SMOTED_business_yelp_v2.Rdata")

#make train and test
library(caret)
sample2 <- createDataPartition(SMOTED_business_yelp_v2$is_open, p = .70, list = FALSE)
train_v2 <- SMOTED_business_yelp_v2[sample2,]
test_v2 <- SMOTED_business_yelp_v2[-sample2,]

#glm logit model 2

model_glm_v2 <- glm(is_open ~., data = train_v2, family = binomial(link = logit))

summary(model_glm_v2)
anova(model_glm_v2, test = "Chisq")

fitted.results_v2 <- predict(model_glm_v2, newdata =test_v2[-3], type = "response")

fitted.results_v2 <- ifelse(fitted.results_v2 > 0.5,1,0)

fitted.results_v2 <- as.factor(fitted.results_v2)
caret::confusionMatrix(data = fitted.results_v2, reference = test_v2$is_open)

# Find the best model with the best cost parameter via 10-fold cross-validations
tryTypes=c(0:7)
tryCosts=c(0.0001,0.001,0.01,0.1,1)
bestCost=NA
bestAcc=0
bestType=NA
for(ty in tryTypes){
  for(co in tryCosts){
    acc=LiblineaR(data=scaled_train_v2,target=train_v2$is_open,type=ty,cost=co,bias=1,cross=5,verbose=FALSE)
    cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
    if(acc>bestAcc){
      bestCost=co
      bestAcc=acc
      bestType=ty
    }
  }
}

scaled_train_v2=scale(train_v2[-3],center=TRUE,scale=TRUE)
scaled_test_v2 = scale(train_v2[-3], center = TRUE, scale = TRUE)


model_liblinear_v2 <- LiblineaR(data=scaled_train_v2,target=train_v2$is_open,cost=.0001,bias=1,verbose=FALSE)

#prediction ##check again
predict(model_liblinear_v2, newdata = scaled_test_v2)

p=  predict(model,scaled_test,proba=TRUE,decisionValues=TRUE)

#confusion matrix

confusionMatrix(p$predictions, test$is_open)

####################xgboost

library(xgboost)

train_xgb <- train_v2[,-3]
test_xgb <- test_v2[,-3]

#train_xgb <- tst[,-3]
#test_xgb <- testsu[,-3]

train_xgb <- as.matrix(train_xgb)
test_xgb <- as.matrix(test_xgb)

#train_v2$is_open <- as.numeric(train_v2$is_open)
#test_v2$is_open <- as.numeric(test_v2$is_open)

#train_v2$is_open <- factor(train_v2$is_open, levels = c(0,1), labels = c(0,1))

#tst$is_open <- factor(tst$is_open, levels = c(0,1), labels = c(0,1))



dtrain <- xgb.DMatrix(data = train_xgb, label = train_v2$is_open)
dtest <- xgb.DMatrix(data = test_xgb, label= test_v2$is_open)

#tst <- train_v2
#testsu <- test_v2
#tst$is_open <- as.numeric(tst$is_open)
##
train_v2$is_open <- as.numeric(train_v2$is_open)
test_v2$is_open <- as.numeric(test_v2$is_open)

train_v2$is_open <- ifelse(train_v2$is_open == 1,0,1)
test_v2$is_open <- ifelse(test_v2$is_open == 1,0,1)

#tst$is_open <- ifelse(tst$is_open == 1,0,1)
#test_v2$is_open <- ifelse(test_v2$is_open == 1,0,1)

####################### train a model using our training data  ###############

model_xgboost <- xgboost(data = dtrain, # the data   
                 nround = 1000, # max number of boosting iterations
                 objective = "multi:softmax",num_class = 2, max_depth=5,eta = 0.12,silent =1,
                 eval_metric ="merror", min_child_weight =1, subsample = 0.5,colsample_bytree = 0.7)  # the objective function

params <- list(booster = "gbtree", objective = "multi:softmax",num_class = 2,eval_metric ="merror", eta=0.12,silent =1, gamma=0, max_depth=6, min_child_weight=1, subsample=0.5, colsample_bytree=0.7)

xgb_cv <- xgb.cv( params = params, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
##best iteration = 490

xgb_final <- xgboost(data = dtrain,objective = "multi:softmax",num_class = 2,eval_metric ="merror", eta=0.12,silent =1, gamma=0, max_depth=6, min_child_weight=1, subsample=0.5, colsample_bytree=0.7,  nrounds = 490, print_every_n = 10, eval_metric = "merror")


#model prediction 1

pred_xgb <- predict(xgb_final, dtest)



#chnge to factor for confusion matrix
pred_real_fact_cv <- factor(test_v2$is_open, levels = c(0,1), labels = c(0,1) )
pred_factor_cv <- factor(pred_xgb, levels =  c(0,1), labels = c(0,1))

#evaluate cv with xgb
caret::confusionMatrix(pred_factor_cv,pred_real_fact_cv)
#same 68% accuracy


#predict 2
pred_xgb <- predict(model_xgboost, dtest)

pred_real_fact <- factor(test_v2$is_open, levels = c(0,1), labels = c(0,1) )
pred_factor <- factor(pred_xgb, levels =  c(0,1), labels = c(0,1))

caret::confusionMatrix(pred_factor,pred_real_fact)
#accuracy : 68 %


#######################################################################
####### 17 april ##########

#inlcude more features from business_attributes
#temp3 <- plyr::join(business_yelp_merged_model_v2, temp_attr, type = "left")

temp_4 <- inner_join(business_yelp_merged, temp_attr, by = "business_id")
save(business_yelp_merged_attributes, file = "business_yelp_merged_attributes.Rdata")

business_yelp_merged_attributes[is.na(business_yelp_merged_attributes)] <- 0


temp_attr <- business_attributes_yelp
cols_attr <- c(2:82)   
temp_attr[,cols_attr] = apply(temp_attr[,cols_attr], 2, function(x) (as.character(x)));

# na to 0

temp_attr[,cols_attr] = apply(temp_attr[,cols_attr], 2, function(x) ifelse(x == "True",1,0));

save(temp_attr, file ="temp_attr.Rdata")

business_yelp_merged_model_v4 <-temp_4[,c(5,10,11,12,14,15,16,17,18:98)]

save(business_yelp_merged_model_v4, file = "business_yelp_merged_model_v4.Rdata")




#smote again as imbalance

data_SMOTE_3 <- business_yelp_merged_model_v4

data_SMOTE_3$is_open <- as.factor(data_SMOTE_3$is_open)

SMOTED_business_yelp_v3 <-DMwR::SMOTE(is_open ~.,data_SMOTE_3)

table(SMOTED_business_yelp_v2$is_open) #imbalanace better now with smote

save(SMOTED_business_yelp_v2, file ="SMOTED_business_yelp_v2.Rdata")
load("SMOTED_business_yelp_v3.Rdata")

#make train and test
library(caret)
sample3 <- createDataPartition(SMOTED_business_yelp_v3$is_open, p = .70, list = FALSE)
train_v3 <- SMOTED_business_yelp_v3[sample3,]
test_v3 <- SMOTED_business_yelp_v3[-sample3,]

library(xgboost)

#remove o/p varibale
train_xgb_last <- train_v3[,-c(1,4)]

test_xgb_last <- test_v3[,-c(1,4)]
test_xgb_last <- test_v3[,-1]
#make matrix
train_xgb_last <- as.matrix(train_xgb_last)
test_xgb_last <- as.matrix(test_xgb_last)

#change o/p to 0 1
train_v3$is_open <- as.numeric(train_v3$is_open)
test_v3$is_open <- as.numeric(test_v3$is_open)

train_v3$is_open <- ifelse(train_v3$is_open == 1,0,1)
test_v3$is_open <- ifelse(test_v3$is_open == 1,0,1)

#train

dtrain_last <- xgb.DMatrix(data = train_xgb_last, label = train_v3$is_open)
dtest_last <- xgb.DMatrix(data = test_xgb_last, label= test_v3$is_open)

params <- list(booster = "gbtree", objective = "multi:softmax",num_class = 2,eval_metric ="merror", eta=0.12,silent =1, gamma=0, max_depth=6, min_child_weight=1, subsample=0.5, colsample_bytree=0.7)

xgb_cv_last <- xgb.cv( params = params, data = dtrain_last, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early.stop.round = 20, maximize = F)
##best iteration = 193


model_xgboost <- xgboost(data = dtrain_last, # the data   
                         nround = 193, # max number of boosting iterations
                         objective = "multi:softmax",num_class = 2, max_depth=5,eta = 0.12,silent =1,
                         eval_metric ="merror", min_child_weight =1, subsample = 0.5,colsample_bytree = 0.7)


xgb_cv_last$best_iteration #193 rd iteration

print(xgb_cv_last, verbose=TRUE)

#model prediction 1

pred_xgb <- predict(model_xgboost, dtest_last)



#chnge to factor for confusion matrix
pred_real_fact_cv <- factor(test_v3$is_open, levels = c(0,1), labels = c(0,1))
pred_factor_cv <- factor(pred_xgb, levels =  c(0,1), labels = c(0,1))

#evaluate cv with xgb
caret::confusionMatrix(pred_factor_cv,pred_real_fact_cv)

importance <- xgb.importance(feature_names = NULL, model = model_xgboost)
print(xgb.plot.importance(importance_matrix = importance, top_n = 15))



#86 performance










