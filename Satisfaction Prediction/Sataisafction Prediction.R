setwd("C:/Users/sjaikumar/Downloads/Digging into data related/yelp_dataset_challenge_academic_dataset")
library(grid)
library(cwhmisc)
library(e1071)
library(rpart)
library(caret)
library(mlbench)
library(stringr)
library(Metrics)

library(kernlab)
library(caTools)
library(data.table)
library(ggplot2)


## Load User Dataset
user <- read.csv("yelp_academic_dataset_user.csv")
## Load Business Dataset
business <- read.csv("yelp_academic_dataset_business.csv")
##business_original <- business
##business <- business_original
reviews <-read.csv("yelp_academic_dataset_review.csv")

## Extract data only for Restaurant type
business$categories <- apply(business, 1, function(x) { grepl("restaurant",tolower(x[76]))}) 
business <- business[business$categories== TRUE,]

# Get rid of irrelevant variables
business <- business[, -grep("attributes.Hair+",colnames(business))]
business <- business[, -grep("attributes.Music+",colnames(business))]
business <- business[, !colnames(business) %in% c("attributes.BYOB","attributes.By.Appointment.Only","categories","attributes.BYOB.Corkage","full_address","attributes.Ages.Allowed","open","type","attributes.Coat.Check"
                                                  ,"attributes.Accepts.Insurance","neighborhoods","attributes.Dietary.Restrictions.vegan","attributes.Dietary.Restrictions.vegetarian")]
business$stars <- factor(business$stars)
reviews <- reviews[, c("user_id", "review_id", "business_id", "stars")]
names(reviews)[names(reviews)=="stars"] <- "review_stars"
user$friends <- (str_count(user$friends, "'")/2)
user$compliments_score <- rowSums(user[,grep("compliments+", names(user))], na.rm = TRUE)
user$votes_score <- rowSums(user[,grep("votes+", names(user))], na.rm = TRUE)
user <- user[, c("review_count", "average_stars", "user_id", "fans", "friends", "compliments_score", "votes_score")] 
names(user)[names(user)=="average_stars"] <- "average_stars_of_user"

# Calculate the good_for_score 
l <- grep("attributes.Good+", colnames(business))
for(i in l){
  business[,i] <- as.numeric(business[,i])
}

business$good_for_score <- rowSums(business[,grep("attributes.Good+", names(business))], na.rm = TRUE)

# Calculate the park_score
l <- grep("attributes.Parking+", colnames(business)) # index of columns
for(i in l){
  business[,i] <- as.numeric(business[,i])
}
business$park_score <- rowSums(business[,grep("attributes.Parking+", names(business))], na.rm = TRUE) 

#Merging the dataset by Business_id
reviewsBusiness <- merge(reviews, business, by="business_id")
total <- merge(reviewsBusiness, user, by="user_id")
total$review_stars_c <-ifelse(total$review_stars>3.5,"Satisfied","Unsatisfied")
total$review_stars_c <-as.factor(total$review_stars_c)

names(total)[names(total)=="review_count.x"] <- "review_count_business"
names(total)[names(total)=="review_count.y"] <- "review_count_user"

# Record performance of starts ("Positive" "Negative") 
record_performance_binary <- function(df, name, predV, test) {
  pred.table <- table(pred = predV, true=test$review_stars_c)
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(pred.table)$diag)))
  return(df)
}


# Prediction using svm model
predict_svm <- function(model,test){
  p_svm <- predict(model, test)
  return(p_svm)
}
 
# Stratified Sampling: based on the distribution of the target variable on different classes.
# Preserve the distribution of the outcome in the training and test sets
set.seed(107)
 #75% of data goes into training
inTrain <- createDataPartition(y = total$review_stars_c, p = .75,list = FALSE)
trainset <- total[inTrain,]
testset  <- total[-inTrain,]

###########################################DECISION TREES##############################

decision_tree_performance <- function(table_name, var_name, model_name, data_name) {
pred_tree <- predict(model_name, data_name)
  
  predictions <- data.table(cbind(data_name$review_stars_c,pred_tree))
  predictions[, predict := ifelse(Satisfied > Unsatisfied, 1, 2)]
  
  accuracy<-confusionMatrix(predictions$V1, predictions$predict)
  accr<-accuracy$overall['Accuracy']
  result_perc <- cbind(var_name,accr)
  table_name <- rbind(table_name, result_perc)
  return(table_name)
}

resultset <- data.frame("Decision_Tree_Model" = character(), "Accuracy" = numeric())

resultset <- decision_tree_performance(resultset, "park_score+review_count_business+average_stars_of_user+good_for_score+fans+friends", rpart(review_stars_c ~ park_score+review_count_business+average_stars_of_user+good_for_score+fans+friends, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "review_count_user+average_stars_of_user+fans+friends", rpart(review_stars_c ~ review_count_user+average_stars_of_user+fans+friends, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "average_stars_of_user+fans", rpart(review_stars_c ~ average_stars_of_user+fans, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "review_count_business+average_stars_of_user+fans+friends", rpart(review_stars_c ~ review_count_business+average_stars_of_user+fans+friends, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "review_count_user+review_count_business+average_stars_of_user+fans+friends", rpart(review_stars_c ~ review_count_user+review_count_business+average_stars_of_user+fans+friends, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends", rpart(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "stars+average_stars_of_user+fans+friends", rpart(review_stars_c ~ stars+average_stars_of_user+fans+friends, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "stars+average_stars_of_user+fans+friends+compliments_score+votes_score", rpart(review_stars_c ~ stars+average_stars_of_user+fans+friends+compliments_score+votes_score, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol", rpart(review_stars_c ~ attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Alcohol", rpart(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Alcohol, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi+attributes.Has.TV", rpart(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi+attributes.Has.TV, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)
resultset <- decision_tree_performance(resultset, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi", rpart(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi, data=trainset, control = rpart.control(minsplit = 20, minbucket = 6)), testset)


#########################LOGISTIC REGRESSION##################################################


#Storing data into new variable  ALL TESTS


trainset$review_stars_numeric <-ifelse(trainset$review_stars_c=="Satisfied",1,0)
testset$review_stars_numeric <-ifelse(testset$review_stars_c=="Satisfied",1,0) 
results_logistic <- data.frame("Logistic_Model" = character(), "Accuracy" = numeric())
test_glm <- data.table(testset)
#
#1
glm1 <- glm(review_stars_numeric ~ as.vector(park_score) +as.vector(review_count_user) + as.vector(average_stars_of_user) + as.vector(good_for_score) + as.vector(fans) + as.vector(friends) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('park_score+review_count_user+average_stars_of_user+good_for_score+fans+friends'), Accuracy=c(1-misClassificError_glm_1)))

#2
glm1 <- glm(review_stars_numeric ~ as.vector(review_count_user) + as.vector(average_stars_of_user) + as.vector(fans) + as.vector(friends) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('review_count_user+average_stars_of_user+fans+friends'), Accuracy=c(1-misClassificError_glm_1)))



#3
glm1 <- glm(review_stars_numeric ~ as.vector(average_stars_of_user) + as.vector(fans) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('average_stars_of_user+fans'), Accuracy=c(1-misClassificError_glm_1)))



#4
glm1 <- glm(review_stars_numeric ~ as.vector(review_count_business) + as.vector(average_stars_of_user)  + as.vector(fans) + as.vector(friends) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('review_count_business+average_stars_of_user+fans+friends'), Accuracy=c(1-misClassificError_glm_1)))



#5
glm1 <- glm(review_stars_numeric ~  +as.vector(review_count_user) + as.vector(review_count_business) + as.vector(average_stars_of_user) +  as.vector(fans) + as.vector(friends) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('review_count_user+review_count_business+average_stars_of_user+fans+friends'), Accuracy=c(1-misClassificError_glm_1)))



#6
glm1 <- glm(review_stars_numeric ~ as.vector(stars) +as.vector(review_count_user) + as.vector(average_stars_of_user) + as.vector(review_count_business) + as.vector(fans) + as.vector(friends) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('stars+review_count_user+review_count_business+average_stars_of_user+fans+friends'), Accuracy=c(1-misClassificError_glm_1)))



#7
glm1 <- glm(review_stars_numeric ~ as.vector(stars)  + as.vector(average_stars_of_user)  + as.vector(fans) + as.vector(friends) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('stars+average_stars_of_user+fans+friends'), Accuracy=c(1-misClassificError_glm_1)))



#8
glm1 <- glm(review_stars_numeric ~ as.vector(stars) + as.vector(average_stars_of_user)  + as.vector(fans) + as.vector(friends) + as.vector(compliments_score) + as.vector(votes_score) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('stars+average_stars_of_user+fans+friends+compliments_score+votes_score'), Accuracy=c(1-misClassificError_glm_1)))



#9
glm1 <- glm(review_stars_numeric ~ as.vector(attributes.Noise.Level) +as.vector(attributes.Wi.Fi) + as.vector(attributes.Alcohol) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol'), Accuracy=c(1-misClassificError_glm_1)))



#10
glm1 <- glm(review_stars_numeric ~ as.vector(stars) +as.vector(review_count_user) + as.vector(review_count_business) + as.vector(average_stars_of_user) + as.vector(fans) + as.vector(friends) + as.vector(attributes.Noise.Level) + as.vector(attributes.Alcohol) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Noise.Level+attributes.Alcohol'), Accuracy=c(1-misClassificError_glm_1)))



#11
glm1 <- glm(review_stars_numeric ~ as.vector(stars) +as.vector(review_count_user) + as.vector(average_stars_of_user) + as.vector(review_count_business) + as.vector(fans) + as.vector(friends) + as.vector(attributes.Wi.Fi)+ as.vector(attributes.Has.TV) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi+attributes.Has.TV'), Accuracy=c(1-misClassificError_glm_1)))



#12
glm1 <- glm(review_stars_numeric ~ as.vector(stars) +as.vector(review_count_user) + as.vector(average_stars_of_user) + as.vector(review_count_business) + as.vector(fans) + as.vector(friends) + as.vector(attributes.Wi.Fi) , family = binomial(link = 'logit'), data = trainset)
f.results_glm_1 <- predict(glm1, test_glm, type = 'response')
f.results_glm_1 <- ifelse(f.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(f.results_glm_1 != test_glm$review_stars_numeric)
results_logistic <- rbind(results_logistic, data.frame(Logistic_Model=c('stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi'), Accuracy=c(1-misClassificError_glm_1)))


#Reducing the amount of data in Total in order to facilitate SVM Run
newtotal <- total[1:5000,]

# Stratified Sampling: based on the distribution of the target variable on different classes.
# Preserve the distribution of the outcome in the training and test sets
set.seed(107)
#75% of data goes into training
inTrain <- createDataPartition(y = newtotal$review_stars_c, p = .75,list = FALSE) 

trainset <- newtotal[inTrain,]
testset  <- newtotal[-inTrain,]


#######################################################SVM MODEL##################################################


# Predict feedback (satisfied, unsatisfied) that a user gives to a particular restaurant
mfc_baseline_binary <- sum(testset$review_stars > 3.5) / nrow(testset)
results_svm_binary <- data.frame(model=c("MFC"), score=c(mfc_baseline_binary))

predBinary <- predict_svm(svm(review_stars_c ~ park_score+review_count_user+average_stars_of_user+good_for_score+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "park_score+review_count_user+average_stars_of_user+good_for_score+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ review_count_user+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "review_count_user+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ average_stars_of_user+fans, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "average_stars_of_user+fans", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ review_count_business+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "review_count_business+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ review_count_user+review_count_business+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "review_count_user+review_count_business+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+average_stars_of_user+fans+friends, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+average_stars_of_user+fans+friends", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+average_stars_of_user+fans+friends+compliments_score+votes_score, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+average_stars_of_user+fans+friends+compliments_score+votes_score", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "attributes.Noise.Level+attributes.Wi.Fi+attributes.Alcohol", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi", predBinary, testset)
predBinary <- predict_svm(svm(review_stars_c ~ stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi, data=trainset), testset)
results_svm_binary <- record_performance_binary(results_svm_binary, "stars+review_count_user+review_count_business+average_stars_of_user+fans+friends+attributes.Wi.Fi", predBinary, testset)

results_svm_binary