# Midterm Project By Charlie Yoo and Karan Gupta


#Loading all the packages using pacman.
pacman:: p_load(magrittr, lubridate, tidyverse, forecast, zoo, tm, rtweet, mongolite, sqldf)

#Using Charlie's APi Key we'll extract the twitter data
appname = "charlieyoo4"
api_key = "###"
api_secret = "###"
access_token = "###"
access_token_secret = "###"
token = create_token(
  app = appname,
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret)


setwd("~/Desktop/ISA 414/Midterm Project")

#b)Reading in the positive and negative words
positive_words <- readLines ("positive-words.txt")
negative_words <- readLines ("negative-words.txt")

#Setting up a variable to keep track of the sentiment score
all_tweets = data.frame("Tweet" = NULL, "Pos" = NULL, "Neg" = NULL, "Score" = NULL) #making data frame for first pass to put data in 
final = data.frame()

#Initializing the variable which will be used in the loop. Infinite loop, click stop when done using program.
i = 1

finalscore = data.frame("Tweet" = NULL, "Pos" = NULL, "Neg" = NULL, "Score" = NULL) #making data frame for first pass to put data in 
final = data.frame()
i = 1
while(i<=3){
  x = nrow(final) ##For when to tweet no new tweets
  temp = search_tweets(q = 'ISA414YG',
                       n = 3,
                       token = token)
  final = rbind(final, temp)
  final = distinct(final)
      y = data.frame("Text" = final$text)
      text_data <- iconv(y$Text, "latin1", "ASCII", sub="")
      vecData   <- VectorSource (text_data)
      myCorpus  <- VCorpus (vecData)
      
      removeLinks <- function (x) {
        gsub ("http.+?[[:blank:]]","",x)
      }
      
      
      #d) Doing all the cleaning and pre-processing below
      
      myCorpus <- tm_map(myCorpus, content_transformer(removeLinks))
      myCorpus <- tm_map(myCorpus, removePunctuation)
      myCorpus <- tm_map(myCorpus, removeNumbers)
      myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
      myCorpus <- tm_map(myCorpus, stripWhitespace)
      myCorpus <- tm_map(myCorpus, content_transformer(tolower))
      for(j in 1:length(myCorpus)){
        terms = unlist(strsplit(myCorpus[[j]]$content, " "))
        pos_matches <- sum (terms %in% positive_words)
        neg_matches <- sum (terms %in% negative_words)
        score <- pos_matches - neg_matches
        Pos = ifelse(score>0, 1, 0)
        Neg = ifelse(score<0, 1, 0)
        tempscore = data.frame("Tweet" = myCorpus[[j]]$content, "Pos" = Pos, "Neg" = Neg, "Score" = score)
        finalscore = rbind(finalscore, tempscore)
        finalscore = distinct(finalscore)
      }
      print(paste("Your Score is:", sum(finalscore$Score)))
      ifelse(sum(finalscore$Score)<=0,print("Score getting low"), print("Lookin Good"))
      ##generate sentiment score from x
      #Creating a new Connection to the database a)
      con <-mongo(collection = "TwitterScoresTest", db= "db", url="mongodb+srv://USER:PASSWORDcluster0.amio5.mongodb.net/myFirstDatabase?retryWrites=true&w=majority")
      con$insert(finalscore)
      Sys.sleep(10)
      y = nrow(final)
      ifelse(x==y, print("No New Tweets"), "")
      ##PRESS STOP WHEN COMPLETED
    }




