setwd("C:/Users/sjaikumar/Downloads/Digging into data related/yelp_dataset_challenge_academic_dataset")
library(grid)
library(cwhmisc)
library(e1071)
library(rpart)
library(caret)
library(mlbench)
library(stringr)
library(Metrics)
library(dplyr)


user <- read.csv("yelp_academic_dataset_user.csv")
user_original <- user
business <- read.csv("yelp_academic_dataset_business.csv")
business_original <- business
#business <- business_original
reviews1 <-read.csv("chunk1.csv")
reviews2 <-read.csv("chunk2.csv")
reviews3 <-read.csv("chunk3.csv")
reviews4 <-read.csv("chunk4.csv")

reviews <- rbind(reviews1,reviews2,reviews3,reviews4)

business$categories <- apply(business, 1, function(x) { grepl("restaurant",tolower(x[76]))}) 
business <- business[business$categories== TRUE,]

# Get rid of irrelevant variables
business <- business[, -grep("attributes.Hair+",colnames(business))]
business <- business[, -grep("attributes.Music+",colnames(business))]
business <- business[, !colnames(business) %in% c("attributes.BYOB","attributes.By.Appointment.Only","categories","attributes.BYOB.Corkage","full_address","attributes.Ages.Allowed","open","type","attributes.Coat.Check"
                                                  ,"attributes.Accepts.Insurance","neighborhoods","attributes.Dietary.Restrictions.vegan","attributes.Dietary.Restrictions.vegetarian")]

reviewsBusiness <- merge(reviews, business, by="business_id")
total <- merge(reviewsBusiness, user, by="user_id")


df_state4 <- business %>% group_by(stars) %>% summarize(avg_stars = mean(stars.x))
df_states <- business %>% group_by(state) %>% summarize(avg_stars = mean(stars), count=n()) %>% arrange(desc(count))

ggplot(data=df_states, aes(state, avg_stars)) +  geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(avg_stars, 2)), hjust=1, color="white") + ggtitle("Average Stars recieved by Restaurant in a State")



hist(reviewsBusiness$stars.x,breaks=40)
#hist(reviewsBusiness$stars.x,freq=FALSE,xlab=reviewsBusiness$stars.x,main="Distribution of Ratings",col="lightgreen",xlim=c(15,35),ylim=c(0,0.20))


names(reviewsBusiness)

###################plotting on neighborhoods################

df_hood <- business %>% group_by(neighborhoods) %>% summarize(avg_stars = mean(stars), count=n()) %>% arrange(desc(count))
ggplot(data=df_hood, aes(neighborhoods, avg_stars)) +  geom_bar(stat="identity")


ggplot(data=df_hood, aes(neighborhoods, avg_stars)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(avg_stars, 2)), hjust=1, color="white") + ggtitle("Average Stars recieved by Restaurant in a Neighborhood")


#####################plotting on histograms###################

qplot(business$review_count,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram showing Distribution of Review Count", 
      xlab = "Review Count",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(20,738))

qplot(reviewsBusiness$stars.x,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram showing Distribution of Review Stars", 
      xlab = "Review Count",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,10))
