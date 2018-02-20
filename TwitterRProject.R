#install these libraries
rm(list=ls())
installed.packages("twitteR")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm")
install.packages("SnowballC")
install.packages("RSentiment")
install.packages("igraph")
install.packages()
library(dplyr)
library(plyr)
library(twitteR)
library(ROAuth)
library(wordcloud)
library(tm)
library(igraph)
library(ggplot2)

consumer_key <- "qh8wFY1TRaEAmKwAWMazOpFFH"
consumer_secret <- "7Z2R4i1LbbmanLWYuRUsOuV1j2swwhG8EzS0KOeUBlR70VRFSG"
access_token <- "793829198112501760-nTlsY2PhCrA315q9BaZsnMJZgNtvl91"
access_secret <- "iGuCgwSCAG9XvE9yqCZEPwKVC8OnT01bbhgi6bigUMF7i"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#This searches twitter for the string containing 'programming' and returns
#the 500 most recent tweets
tweets <- searchTwitter('programming', lang = "en", n = 500, resultType="recent")

(n.tweet<-length(tweets))#gives the number of tweets
#converting the tweets file in to a dataframe
tweets.df <- twListToDF(tweets)
tweets.df

summary(tweets.df)#summary of the data

View(tweets.df)#see the twitter dataframe in the new window

#details with respect to each variable in the twitter dataset
names(tweets.df)
dim(tweets.df)#Gives the dimentions of the matrix holding the tweets
str(tweets.df)
attributes(tweets.df)

df <- do.call("rbind", lapply(tweets, as.data.frame))#take tweets and call into another dataframe
#names(df)
#head(df,3)
counts=table(df$text)
#barplot(counts)

#we then transform these tweets into a vector to begin using the data
tweets_text <- sapply(tweets, function(x) x$getText())

#create a corpus of the tweets which is from the tm library so we can preform
#edit operations on the text in the tweets next
tweet_corpus <- Corpus(VectorSource(tweets_text))


#First we remove the punctuation because we dont need it
#make all of the words lowercase to make it easier
#stop unwanted 'english' words ex(and, is, then, if, etc.)
#remove numbers
#remove the now frequent whitespace because a lot of stuff was removed
#remove words that are the same as search criteria
#remove url 
tweet_corpus <- tm_map(tweet_corpus, removePunctuation)
tweet_corpus#show the contents of the corpus
tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower))
tweet_corpus <- tm_map(tweet_corpus, removeWords, stopwords("english"))
tweet_corpus <- tm_map(tweet_corpus, removeNumbers)
tweet_corpus <- tm_map(tweet_corpus, stripWhitespace)
tweet_corpus <- tm_map(tweet_corpus, removeWords, c("programming","program"))
removeURL<-function(x) gsub("http[[:alnum:]]*","",x)
tweet_corpus<-tm_map(tweet_corpus,removeURL)
tweet_stopwords<-c(stopwords("english"),"available","via","and")
tweet_stopwords<-setdiff(tweet_stopwords,c("r","big"))
tweet_corpus<-tm_map(tweet_corpus,removeWords,tweet_stopwords)
tweet_corpuscopy<-tweet_corpus

#turn the vector corpus of tweets into and editable text document containing the matrix
tdm<-TermDocumentMatrix(tweet_corpus,control = list(wordlenths=c(1,Inf)))
tdm

idm<-which(dimnames(tdm)$terms == "r")
(frequency.terms<-findFreqTerms(tdm,lowfreq = 10))#show terms with a freq higher than 10
term.freq<-rowSums(as.matrix(tdm))
term.freq<-subset(term.freq,term.freq>=10)
df<-data.frame(term =names(term.freq),freq=term.freq)

#sort the words in decending order
#show rough view and count
v <- sort(term.freq,decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d

#histogram for the words which are most frequently used in the tweets
#criteria we used here is#if the word appears atleast 10 times in the tweets data
#the word is characterised as a frequent word.
ggplot(df, aes(x=term,y=freq))+geom_bar(stat = "identity")+xlab("terms")+ylab("count")+coord_flip()


#create list of words with their relation to 'javascript' as a ratio
word.co <- findAssocs(dtm, "javascript", corlimit=0.0)
head(word.co, 20)

topWords <- word.co$javascript[1:10]
barplot(topWords, main="Top Tweets About Programming", 
        xlab="The Words")

#this function now creates the word cloud from the new stripped down tweets above
#we pass in the tweet vector
#make the words in a random order
#use only a max of 100 words (dont wwant it too big)
#scale the size of the wordcloud
#add colors (this color changes as you get further from the center which means less use of those words)
wordcloud(tweet_corpus, random.order = F, max.words = 100, scale = c(3,0.5), colors = brewer.pal(8,'Dark2')) 

#now this is the beginning of taking a user from twitter and creating a bubble graph
#of their followers and who they follow
#get our twitter user
start <- getUser("@CCICareer")
#lookup their friends and followers and add them to the variables
theFriends <- lookupUsers(start$getFriendIDs())
theFollowers <- lookupUsers(start$getFollowerIDs())

#create a vector for friends and followers
#we make the vector 20 friends and followers long (change the number 20 to change size of bubble graph)
#pass in name parameter to get the friend/follower names only
friends <- sapply(theFriends[1:20], name)
followers <- sapply(theFollowers[1:20], name)

#Create a 'relations' variable to merge the two data frames of friends and followers
#Follower=friends creates the vertices of @CCIcareers 20 friends and makes arrows to them
#User=followers, Follower=@CCICareer Creates the vertices of the 20 people @CCICareer follows and makes arrows to them
relations <- merge(data.frame(User = '@CCICareer', Follower=friends), data.frame
                   (User = followers, Follower='@CCICareer'), all=TRUE)

#create the graph of the relations variable and make the arrows directed
g <- graph.data.frame(relations, directed = TRUE)

#label the vertices with the followers and friends names
V(g)$label <- V(g)$name

#plot graph
#NOTICE this loads in a differend window unlike the wordcloud which loads in the plots tab
#TO CHANGE THE VERTICES COLOR, CLICK THE SELECT BUTTON IN THE TOP LEFT CORNER
#OF THE GRAPHS WINDOW AND HIT 'SELECT ALL VERTICES', THEN RIGHT CLICK A VERTEX AND
#HIT CHANGE COLOR
tkplot(g)


frequent_words_plot <- function(tweet_corpus,count)
{
  corp <- Corpus(VectorSource(tweet_corpus))
  corp <- tm_map(corp,removeWords,c(stopwords('english'),stopwords('SMART')))
  tdm <- TermDocumentMatrix(corp) 
  
  freq.terms <- findFreqTerms(tdm,lowfreq = count)
  term.freq <- rowSums(as.matrix(tdm))
  term.freq <- subset(term.freq, term.freq >= count)
  df <- data.frame(term = names(term.freq), freq = term.freq)
  ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()
}
dtm_tweets<-DocumentTermMatrix(VCorpus(VectorSource(tweet_corpus[[1]]$content)))
freq_tweets <- colSums(as.matrix(dtm_tweets))

