setwd("###")
source("###/WalmartScrape.R") # change ### to folder path containing WalmartScrape
pacman:: p_load(magrittr, tidyverse, tidytext, ggplot2, lubridate, wordcloud, RColorBrewer, sentimentr, rvest)
reviews_all = data.frame()
BlankPg = c()
pgnum = 25 ##set to number of pages on desired prodcut page
for(i in 1:pgnum){
  url = paste0("https://www.walmart.com/reviews/product/284021788?page=",i,"&sort=submission-desc")
  temp = WalmartScrape(url)
  reviews_all = rbind(reviews_all, temp)
  Sys.sleep(5)
  if(nrow(temp)==0){
    TempBlankPg = i
    BlankPg = c(BlankPg, TempBlankPg)
  }
  else(NA)
}

# create ID for reviews
review_df <- reviews_all %>%
  mutate(id = row_number())

lexicon1 = lexicon::hash_sentiment_jockers_rinker

sent_df <- review_df %>%
  get_sentences() %>%
  sentiment_by(by = c('id', 'author', 'date', 'stars', 'text'), polarity_dt = lexicon1)



p <- ggplot(sent_df, aes(x = stars, y = ave_sentiment, color = factor(stars), group = stars)) +
  geom_boxplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_text(aes(5.2, -0.05, label = "Neutral Sentiment", vjust = 0), size = 3, color = "red") +
  guides(color = guide_legend(title="Star Rating")) +
  ylab("Avg. Sentiment") +
  xlab("Review Star Rating") +
  ggtitle("Sentiment of Huggies Walmart Reviews, by Star Rating") 
p





# sentiment over time
sent_ts <- sent_df %>%
  mutate(
    date = as.Date(date, format = "%d-%B-%y"),
    dir = sign(ave_sentiment)
  ) %>%
  group_by(date) %>%
  summarise(
    avg_sent = mean(ave_sentiment)
  ) %>%
  ungroup()

# plot
p <- ggplot(sent_ts, aes(x = date, y = avg_sent))+
  geom_smooth()+
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_text(aes(max(date) - 5, -0.02, label = "Neutral Sentiment", vjust = 0), size = 3, color = "red") +
  ylab("Avg. Sentiment") +
  xlab("Review Date") +
  ggtitle("Sentiment of Huggies Walmart Reviews, Over Time 2021 (APR - JUL)")
p



####Peter's stuff
#############################################
#############################################
### MASTER FILE: R CODE FOR TEXT ANALYSES ###
#############################################
#############################################

###############################
###############################
## PREPARE TEXT FOR ANALYSIS ##
###############################
###############################

#for five star reveiws later
# reviews_all$stars %<>% as.numeric()
# FiveStars = subset(reviews_all, reviews_all$stars==5)


co = reviews_all
# Step 3: Overview and preprocess the data
str(co)
co$text <- as.character(co$text) # set 'text' column as a character string 
co$lineID <- 1:nrow(co) # create an ID based on the line of the text in 'co'
co$nchar <- nchar(co$text) # number of characters in 'text'
co$nwords <- sapply(strsplit(co$text, " "), length) # number of words in 'text'

# Select variables in 'co' for text analysis 
library(dplyr)
sv <- select(co, text, lineID, nwords, stars)

# Clean the text data, as necessary
sv$text <- gsub("@\\W+","", sv$text) # remove mentions (good when analyzing tweets)
sv$text <- gsub("@\\w+","", sv$text) # (\w = any word character; \W = non-word characters)
sv$text <- gsub("[[:punct:]]","",sv$text) # remove punctuations
sv$text <- gsub("[[:digit:]]","",sv$text) # remove numbers, if applicable
sv$text <- gsub("http\\W+","",sv$text) # remove URLs
sv$text <- gsub("http\\w+","",sv$text)
sv$text <- tolower(sv$text) # all lower case

######################################
######################################
## TOKENIZATION AND NGRAM FREQUENCY ##
######################################
######################################

library(tidytext) # required for unnest_tokens() function and stop_words list
library(tidyr) # required for separate() function

# set stopwords (e.g., 'a', 'the', it) to be removed
# 'stop_words' is pullsed from the 'tidytext' package
# 'stop_words' data contains 3 lexicons: onix (404), SMART (571), snowball (174)
# use 'snowball' for removing minimal stopwords; use 'SMART' for removing most stopwrods 
sw <- subset(stop_words, lexicon == "SMART") 

##################
# Unigram (word) #
##################

sv_unigram <- sv %>%
  unnest_tokens(input = text, output = word, token = "words") # tokenize from text to word

sv_unigram <- sv_unigram %>%
  anti_join(sw, by = "word") # remove stopwords 'sw'

# remove specific words, if necessary
rw <- c("?","it?", "don?","you?","amp","dont", "i?","uf", "diaper", "baby") # insert words to be removed here
sv_unigram <- sv_unigram %>%
  filter(!word %in% rw) # remove words contained in 'rw'

unigram_freq <- sv_unigram %>%
  count(word, sort = TRUE)
unigram_freq

##########
# Bigram #
##########

sv_bigram <- sv %>% 
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) # tokenize from text to bigram

bigram_sep <- sv_bigram %>% # separate bigrams to words (but keeping order of words)
  separate(bigram, c("word1","word2"), sep = " ")

bigram_fil <- bigram_sep %>% # filter out bigrams containing words in stopwords 'sw'
  filter(!word1 %in% sw$word) %>%
  filter(!word2 %in% sw$word) %>%
  filter(!is.na(word1))

bigram_freq <- bigram_fil %>%
  count(word1, word2, sort = TRUE)

bigram_freq

##############################
# Combining unigram, bigram  #
##############################

bigram_freq2 <- data.frame("word" = paste(bigram_freq$word1,bigram_freq$word2),
                           "n" = bigram_freq$n)

ngram_freq <- rbind(top_n(unigram_freq, n = 100),
                    top_n(bigram_freq2, n = 50))

ngram_freq <- ngram_freq[order(-ngram_freq$n),] # descending order based on 'n'
ngram_freq

#############################
# VISUALIZE N-GRAM FREQUENCY #
#############################

library(ggplot2)
library(wordcloud)
library(RColorBrewer)

#####################
# Unigram frequency #
#####################

unigram_freq %>% 
  top_n(n=30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

wordcloud(word = unigram_freq$word, 
          freq = unigram_freq$n,
          max.words = 100,
          random.order = FALSE, 
          rot.per = 0.35,
          scale = c(4,0.5),
          colors = brewer.pal(8,"Dark2")
)

####################
# Bigram frequency #
####################

bigram_freq %>% 
  top_n(n=20) %>%
  mutate(word = reorder(paste(word1,word2), n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

wordcloud(word = paste(bigram_freq$word1, bigram_freq$word2), 
          freq = bigram_freq$n,
          max.words = 50,
          random.order = FALSE, 
          rot.per = 0.35,
          scale = c(2,0.5),
          colors = brewer.pal(8,"Dark2")
)

###################
# Ngram frequency #
###################

wordcloud(word = ngram_freq$word, 
          freq = ngram_freq$n,
          max.words = 100,
          random.order = FALSE, 
          rot.per = 0.35,
          scale = c(4,0.5),
          colors = brewer.pal(8,"Dark2")
)

##############################################
##############################################
## WORD-PAIR ASSOCIATION - WITHIN DOCUMENTS ##
##############################################
##############################################

# key input here is 'sv_unigram'
head(sv_unigram,5)

library(widyr) # for pairwise_count() function
library(ggraph)
library(igraph) # for manipulating and analzying networks

# count words co-occuring within sections
word_pairs <- sv_unigram %>%
  pairwise_count(item = word, feature = lineID, sort = TRUE)
word_pairs

# pairwise correlation
# phi coefficeint (pc) - a common measure for binary correlation
# pc - how much more likely it is that either both word X and Y appear, or neitehr do, than one appears without the other

# we need to filter for at least relatively common words first
word_cors <- sv_unigram %>%
  group_by(word) %>%
  filter(n() >= 8) %>%  # EDIT THIS VALUE DEPENDING ON DENSITY OF NETWORK GRAPH; higher number to make graph less dense
  pairwise_cor(word, lineID, sort = TRUE)
word_cors

word_cor_brand <- word_cors %>%
  filter(item1 == "fit") # shows words highly correlated with 'fit'
word_cor_brand

set.seed(2019)
word_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

########################
########################
## SENTIMENT ANALYSES ##
########################
########################

library(tidyr)

sentiment <- sv_unigram %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(lineID, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # set negative and positive sentiment in separate columns
  mutate(sent_sum = positive - negative) # calculates the net sentiment (positive - negative)


sv_sent <- merge(sv, sentiment, by = "lineID")
sv_sent$sent_score <- sv_sent$sent_sum / sv_sent$nwords

# overall sentiment score of text
mean(sv_sent$sent_score) 

# separate positive and negative text into different dataframes
sv_sent_pos <- subset(sv_sent, sent_score > 0)
sv_sent_neg <- subset(sv_sent, sent_score < 0)

# proportion positive and negative text
nrow(sv_sent_pos) / nrow(sv_sent) # proportion of text that are positive
nrow(sv_sent_neg) / nrow(sv_sent) # proportion of text that are negative

# visualize most common positive and negative words about 'realdonaldtrump'
library(reshape2)
sv_unigram %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"), max.words = 90)

# Most frequent terms mentioned in positive and negative texts
sv_sent_pos %>%
  unnest_tokens(input = text, output = word, token = "words") %>%
  anti_join(sw, by = "word") %>%
  filter(!word %in% rw) %>%
  count(word, sort = TRUE) %>%
  top_n(n=30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "dark green") +
  xlab(NULL) +
  coord_flip()

sv_sent_neg %>%
  unnest_tokens(input = text, output = word, token = "words") %>%
  anti_join(sw, by = "word") %>%
  filter(!word %in% rw) %>%
  count(word, sort = TRUE) %>%
  top_n(n=30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "red") +
  xlab(NULL) +
  coord_flip()

#####################
#####################
## TF-IDF ANALYSIS ##
#####################
#####################

# tf-idf = term frequency inverse document frequency 
# tf-idf analysis is used to determine words specific to a group of text
# e.g., words specific to positive and negative tweets/reviews
# e.g., words specific to one brand over antoher

##TEST
sv_sent_pos = subset(sv, sv$stars>=4)
sv_sent_neg = subset(sv, sv$stars<=2)

# first, create a new column to identify the two groups of text
sv_sent_pos$groups <- "Positive Reviews"
sv_sent_neg$groups <- "Negative Reviews"

# then, combine the two groups of dataframes
sv_groups <- rbind(sv_sent_pos,sv_sent_neg)

# tokenize from text to words 
sv_groups_unigram <- sv_groups %>%
  unnest_tokens(input = text, output = word, token = "words")

# conduct tf-idf analysis
tf_idf <- sv_groups_unigram %>% 
  count(groups, word, sort = TRUE) %>% 
  bind_tf_idf(word, groups, n) %>%
  arrange(-tf_idf) %>%
  group_by(groups) %>%
  top_n(20) %>% 
  ungroup 

tf_idf %>%
  group_by(groups) %>%
  top_n(20) %>% 
  ungroup %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = groups)) +
  geom_col(alpha = 0.8, show.legend=FALSE) +  
  facet_wrap(~groups, scales = "free") +
  coord_flip() +  
  theme(strip.text=element_text(size=9)) +
  labs(x = NULL, y = "tf-idf",
       title = "Words specific to Positive and Negative Reviews" ,
       subtitle = "Reviews Of 'Lil Snugglers'")



##My sentiment Analysis
sv_sent_pos = data.frame()
sv_sent_neg = data.frame()

for(i in 1:10){
  url = paste0("https://www.walmart.com/reviews/product/688194180?page=",i,"&sort=rating-desc")
  temp = WalmartScrape(url)
  sv_sent_pos = rbind(sv_sent_pos, temp)
  Sys.sleep(5)
  if(nrow(temp)==0){
    TempBlankPg = i
    BlankPg = c(BlankPg, TempBlankPg)
  }
  else(NA)
}

for(i in 1:10){
  url = paste0("https://www.walmart.com/reviews/product/688194180?sort=rating-asc&page=",i)
  temp = WalmartScrape(url)
  sv_sent_neg = rbind(sv_sent_neg, temp)
  Sys.sleep(5)
  if(nrow(temp)==0){
    TempBlankPg = i
    BlankPg = c(BlankPg, TempBlankPg)
  }
  else(NA)
}




##Positive review analysis

##UNIGRAM POS##

sv_unigram_pos <- sv_sent_pos %>%
  unnest_tokens(input = text, output = word, token = "words") # tokenize from text to word

sv_unigram_pos <- sv_unigram_pos %>%
  anti_join(sw, by = "word") # remove stopwords 'sw'

# remove specific words, if necessary
rw <- c("?","it?", "don?","you?","amp","dont", "i?","uf", "diaper", "baby") # insert words to be removed here
sv_unigram_pos <- sv_unigram_pos %>%
  filter(!word %in% rw) # remove words contained in 'rw'

unigram_freq_pos <- sv_unigram_pos %>%
  count(word, sort = TRUE)
unigram_freq_pos

##BIGRAM POS##
sv_bigram_pos <- sv_sent_pos %>% 
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) # tokenize from text to bigram

bigram_sep_pos <- sv_bigram_pos %>% # separate bigrams to words (but keeping order of words)
  separate(bigram, c("word1","word2"), sep = " ")

bigram_fil_pos <- bigram_sep_pos %>% # filter out bigrams containing words in stopwords 'sw'
  filter(!word1 %in% sw$word) %>%
  filter(!word2 %in% sw$word) %>%
  filter(!is.na(word1))

bigram_freq_pos <- bigram_fil_pos %>%
  count(word1, word2, sort = TRUE)

bigram_freq_pos





#Negative Review Analysis

##UNIGRAM##
sv_unigram_neg <- sv_sent_neg %>%
  unnest_tokens(input = text, output = word, token = "words") # tokenize from text to word

sv_unigram_neg <- sv_unigram_neg %>%
  anti_join(sw, by = "word") # remove stopwords 'sw'

# remove specific words, if necessary
rw <- c("?","it?", "don?","you?","amp","dont", "i?","uf", "diaper", "baby") # insert words to be removed here
sv_unigram_neg <- sv_unigram_neg %>%
  filter(!word %in% rw) # remove words contained in 'rw'

neg_unigram_freq <- sv_unigram_neg %>%
  count(word, sort = TRUE)
neg_unigram_freq


##BIGRAM##
sv_bigram_neg <- sv_sent_neg %>% 
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) # tokenize from text to bigram

bigram_sep_neg <- sv_bigram_neg %>% # separate bigrams to words (but keeping order of words)
  separate(bigram, c("word1","word2"), sep = " ")

bigram_fil_neg <- bigram_sep_neg %>% # filter out bigrams containing words in stopwords 'sw'
  filter(!word1 %in% sw$word) %>%
  filter(!word2 %in% sw$word) %>%
  filter(!is.na(word1))

bigram_freq_neg <- bigram_fil_neg %>%
  count(word1, word2, sort = TRUE)

bigram_freq_neg

x = grep("lion king", sv_sent_neg$text, ignore.case = T) 
lionking = sv_sent_neg
lionking = lionking%>%
  mutate(rowid = row_number())
lionking$YN = ifelse(lionking$rowid %in% x, 1, 0)
lionking = subset(lionking, lionking$YN ==1)

?gregexpr

#############
#############
## THE END ##
#############
#############


