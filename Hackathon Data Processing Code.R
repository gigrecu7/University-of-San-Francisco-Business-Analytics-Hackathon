#Importing the data
price_data <- read.csv('price_data.csv')
tweets_data <- read.csv('Tweets_Data.csv')
merged_data <- read.csv('merged.csv')

#Looking at the data, trying to determine if there is data missing
library(questionr)
library(dplyr)
library(lubridate)
freq.na(price_data)
freq.na(tweets_data)
freq.na(merged_data)

#Look at the structures of the two main tables
str(tweets_data)
str(price_data)

#Clean the data

#Turning the tweets data into something workable
#Working tweets data now includes 10 variables, rather than 25
working_tweets_data <- select(tweets_data, screen_name, utc_date, source,
                              favorites, retweets, replies, quotes, tweet_type,
                              text, Sentiment)
#Scrambling to find why there are 4 levels in sentiment factors
#If anyone from the BA club reads this code, there's no way that's a coincidence
#of one lowercase n. 
table(working_tweets_data$Sentiment)

#Deleting the single lowercase n
which(rownames(working_tweets_data$Sentiment) == "n")
working_tweets_data <- working_tweets_data[-8065,]

#Working price data removes the duplicates from Dogecoin
#Also removes the name variable, reduces redundancy
working_price_data <- price_data[-c(31:40), -(1:3)]

#Force certain attributes to become factors, ie something that we can use
#Check the structure of the working tweets data
str(working_tweets_data)
#Force source, tweet type and sentiment to be factors instead of characters
working_tweets_data$source <- as.factor(working_tweets_data$source)
working_tweets_data$tweet_type <- as.factor(working_tweets_data$tweet_type)
working_tweets_data$Sentiment <- as.factor(working_tweets_data$Sentiment)
# Convert utc from characters into an actual date
working_tweets_data$utc_date <- ymd_hms(working_tweets_data$utc_date)
str(working_tweets_data)

#Check structure of working price data
str(working_price_data)
#Change symbols to factor and date from character to date
working_price_data$Symbol <- as.factor(working_price_data$Symbol)
working_price_data$Date <- as.Date(working_price_data$Date)
str(working_price_data)

#Creating our own clean csv's to use
write.csv(working_tweets_data,"/Users/iliegrecu/HackathonData/
          working_tweets_data.csv", row.names =  TRUE)
write.csv(working_price_data,"/Users/iliegrecu/HackathonData/
          working_price_data.csv", row.names =  TRUE)

NEWDATA <- merge(working_price_data, working_tweets_data)

NEWDATA <- rename(NEWDATA, Date_ymd_hms = utc_date )

#Creating a Natural Language Processor
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(working_tweets_data$text))

corpus = tm_map(corpus, content_transformer(tolower)) #Makes everything lowecase
corpus = tm_map(corpus, removeNumbers) # Removes numbers from the review. 
corpus = tm_map(corpus, removePunctuation) #Remove punctuation from the review.
corpus = tm_map(corpus, removeWords, stopwords()) #Removes stopwords (filler words)
corpus = tm_map(corpus, stemDocument) # Convert everything down into the stem of the word
corpus = tm_map(corpus, stripWhitespace) #Getting rid of all the extra spaces

dtm = DocumentTermMatrix(corpus) #Converting this corpus into a matrix
dtm = removeSparseTerms(dtm, .994) #All the columns with only 0s and 1s will go away
dataset = as.data.frame(as.matrix(dtm)) #Corpus matrix into data frame

#Looking for words similar to "hold"
df =  data.frame(working_tweets_data$utc_date, dataset$hold, dataset$holdthelin, dataset$buyandhold)
write.csv(df, "/Users/iliegrecu/HackathonData/
          df.csv", row.names = TRUE)

#Looking for words related to Elon Musk
elon_df = data.frame(working_tweets_data$utc_date, dataset$elon, dataset$elondog, dataset$elonmusk,
                     dataset$musk)
write.csv(elon_df, "/Users/iliegrecu/HackathonData/
          elon_df.csv", row.names = TRUE)

#Looking for calls to action
buy_df = data.frame(working_tweets_data$utc_date, dataset$buy, dataset$now, dataset$trade)
write.csv(buy_df, "/Users/iliegrecu/HackathonData/
          buy_df.csv", row.names = TRUE)

#Screen names v

##Find the top 30 words used in the tweets
top30 <- sort(colSums(dataset), decreasing = TRUE)[1:30]
View(top30)
write.csv(top30,"/Users/iliegrecu/HackathonData/
          top30.csv", row.names =  TRUE)
##Are there any specific words that are affecting the price?
##The high low will be interesting to look at, correlate it with the words
##Max it at around 7 graphs
##The graphs should be very interactive (filters, sliders, motion charts)
##Make it consistent "Jazz it up a little bit" -Shivani
