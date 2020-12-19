# install.packages("Hmisc")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("pastecs")
# install.packages("outliers")
# install.packages("corrplot") 
# install.packages("rcompanion") 
# install.packages("nortest")
# install.packages("ggplot2")
# install.packages("sqldf")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("xlsx")
# install.packages("naniar")
# install.packages("caret")
# install.packages("fastDummies")

# -------------Sentiments Analysis/Text Mining Packages-------------

# install.packages("tidytext")
# install.packages("lexicon")
# install.packages("textdata")
# install.packages("tidyverse")
# install.packages("syuzhet")
# install.packages("plotly")
# install.packages("wordcloud")
# install.packages("plotrix")
# install.packages("dendextend")
# install.packages("ggthemes")
# install.packages("RWeka")
# install.packages("reshape2")
# install.packages("quanteda")

# ---------------Text Classification Packages--------------------------

# install.packages("irlba")
# install.packages("e1071")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("SnowballC")
# install.packages("RColorBrewer")
# install.packages("biclust")
# install.packages("igraph")
# install.packages("fpc")
# install.packages("Rcampdf")
# install.packages("glmnet")
# install.packages("LiblineaR")


#-----------------------------------------------------------------------

library(Hmisc)
library(dplyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(tidyr)
library(pastecs)
library(outliers)
library(corrplot)
library(rcompanion)
library(nortest)
library(ggplot2)
library(sqldf)
library(xlsx)
library(naniar)
library(caret)
library(MASS)
library(fastDummies)
library(tidytext)
library(lexicon)
library(textdata)
library(tidyverse)
library(syuzhet)
library(plotly)
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)
library(irlba)
library(e1071)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(SnowballC)
library(RColorBrewer)
library(biclust)
library(igraph)
library(fpc)
# library(Rcamp
library(glmnet)
library(LiblineaR)

yelp <- read.csv("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Text Mining(Classification) - Yelp Reviews\\yelp.csv")
head(yelp,2)

NROW(yelp)
NCOL(yelp)

summary(yelp)

describe(yelp)

str(yelp)

yelp$business_id <- as.character(yelp$business_id)
yelp$date <- as.Date(yelp$date)
yelp$review_id <- as.character(yelp$review_id)
yelp$text <- as.character(yelp$text)
yelp$user_id <- as.character(yelp$user_id)
yelp$stars <- as.factor(yelp$stars)

y1 <- yelp %>% 
dplyr::select(stars, business_id) %>%
group_by(stars) %>%
summarise(Reviews = n()) 

y1

ggplot(y1, aes(x=stars, y=Reviews))  + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="No. of Reviews per Rating", 
       caption="source: Yelp") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) 

y2 <- sqldf("select stars,sum(cool) Cool, sum(useful) Useful, sum(funny) Funny
                   from yelp
                   group by stars
                   order by stars asc")
y2

ggplot(y2, aes(x=stars, y=Cool))  + 
  geom_bar(stat="identity", width=0.5, fill="darkblue") + 
  labs(title="Star Ratings commented by others as COOL", 
       subtitle="Useful Reviews", 
       caption="source: Yelp") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) 

ggplot(y2, aes(x=stars, y=Useful))  + 
  geom_bar(stat="identity", width=0.5, fill="darkblue") + 
  labs(title="Star Ratings commented by others as USEFUL", 
       subtitle="Useful Reviews", 
       caption="source: Yelp") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) 

ggplot(y2, aes(x=stars, y=Funny))  + 
  geom_bar(stat="identity", width=0.5, fill="darkblue") + 
  labs(title="Star Ratings commented by others as FUNNY", 
       subtitle="Funny Reviews", 
       caption="source: Yelp") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) 

y3 <- sqldf("select strftime('%Y', date * 3600 * 24, 'unixepoch') year, stars
                   from yelp
                   group by date  
                   order by date asc")
head(y3,5)

ggplot(y3, aes(x = year, y = stars, fill=stars)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Year", y = "Star Ratings")
 

y4 <- sqldf("select strftime('%Y', date * 3600 * 24, 'unixepoch') year, sum(cool) cool, sum(funny) funny, sum(useful) useful
                   from yelp
                   group by year  
                   order by year asc")
y4

ggplot(y4, aes(x = year, y = cool)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Year", y = "Cool Ratings") 

ggplot(y4, aes(x = year, y = useful)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Year", y = "Useful Ratings") 

ggplot(y4, aes(x = year, y = funny)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Year", y = "Funny Ratings") 

review_words <- yelp %>%
  dplyr::select(review_id, business_id, stars, text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

head(review_words,5)

# The bing lexicon model classifies the sentiment into a binary category of negative or positive. 

pos_sent <- get_sentiments("bing") %>% filter(sentiment == "positive")
neg_sent <- get_sentiments("bing") %>% filter(sentiment == "negative")

head(pos_sent,5)
head(neg_sent,5)

# Do an inner join with the lexicon for positive sentiment

reviews_positive <- review_words %>%
  inner_join(pos_sent, by = "word") %>%
  group_by(review_id, stars)

head(reviews_positive,5)

# Do an inner join with the lexicon for negative sentiment

reviews_negative <- review_words %>%
  inner_join(neg_sent, by = "word") %>%
  group_by(review_id, stars)

head(reviews_negative,5)

# Combine the postive and negative reviews into 1 dataframe

all_reviews <- data.frame(rbind(reviews_positive , reviews_negative))
head(all_reviews)

y5 <- sqldf("select stars,sentiment from all_reviews")

# The bar plot shows the relation between star ratings and sentiment scores.
# As expected ratings of 4 and 5 show  high positive sentiment.
# Ratings of 1 and 2 have almost equal amount of postive and negative sentiment.

ggplot(y5, aes(stars, sentiment, fill = sentiment)) +
 geom_bar(stat = "identity", show.legend = TRUE) +
 facet_wrap(~stars, ncol = 5, scales = "free_x")

bing_word_counts <- all_reviews %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

head(bing_word_counts,5)

# We can view this visually to assess the top n words for each sentiment.

bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n), n, fill = sentiment)) +
          geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment", x = NULL) +
          coord_flip() 

# Once the data was converted into one-row-per-term-per-document, then AFFIN lexicon was used to find the value of the word choice. 
# More details about AFFIN lexicon can be found at AFFIN.
# This might not work from Jupyter notebook. Try it on R-Studio and come back to Jupyter to continue work

FINN <- tidytext::get_sentiments("afinn")
head(FINN)

reviews_sentiment <- review_words %>%
  inner_join(FINN, by = "word") %>%
  group_by(review_id, stars) %>%
  summarize(sentiment = mean(value))

head(reviews_sentiment,5)

# The box plot shows the relation between average ratings and sentiment scores.
# Higher ratings of 4 and 5 have higher mean sentiment score.

ggplot(reviews_sentiment, aes(stars, sentiment, group = stars)) +
  geom_boxplot(col="green") +
  ylab("Average Sentiment Score")

review_words_counted <- review_words %>%
  count(review_id, business_id, stars, word) %>%
  ungroup()

head(review_words_counted,5)

# Change datatype of Stars to Numeric for mean analysis
review_words_counted$stars <- as.numeric(review_words_counted$stars)
yelp$stars <- as.numeric(yelp$stars)

word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(businesses = n_distinct(business_id),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(stars)) %>%
  ungroup()

head(word_summaries,10)

word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 200, businesses >= 10)

head(word_summaries_filtered,5)

word_summaries_filtered %>%
  arrange(desc(average_stars)) %>% head(5)

word_summaries_filtered %>%
  arrange(average_stars) %>% head(5)

ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(yelp$stars), color = "red", lty = 2, lwd=1) +
  xlab("# of reviews") +
  ylab("Average Stars")

words_afinn <- word_summaries_filtered %>%
  inner_join(FINN,by = "word")

head(words_afinn,5)

ggplot(words_afinn, aes(value, average_stars, group = value)) +
  geom_boxplot(col="red") +
  xlab("AFINN score of word") +
  ylab("Average stars of reviews with this word")

nrc <- tidytext::get_sentiments("nrc")
head(nrc)

reviews_sentiment_nrc <- review_words %>% 
  inner_join(nrc,by="word")

head(reviews_sentiment_nrc,5)

reviews_sentiment_nrc %>% 
count(word,sentiment,sort=TRUE) %>%
group_by(sentiment)%>%top_n(n=10) %>% 
ungroup() %>%
  ggplot(aes(x=reorder(word,n), y=n,fill=sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment,scales="free") + 
  coord_flip()

# Function for text cleaning

f_clean_text <- function (x) {
  
 # clean_text = sapply(x, function(x) x$getText())
  # remove retweet entities
  clean_text = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  # remove at people
  clean_text = gsub('@\\w+', '', clean_text)
  # remove punctuation
  clean_text = gsub('[[:punct:]]', '', clean_text)
  # remove numbers
  clean_text = gsub('[[:digit:]]', '', clean_text)
  # remove html links
  clean_text = gsub('http\\w+', '', clean_text)
  # remove unnecessary spaces
  clean_text = gsub('[ \t]{2,}', '', clean_text)
  clean_text = gsub('^\\s+|\\s+$', '', clean_text)
  # remove emojis or special characters
  clean_text = gsub('<.*>', '', enc2native(clean_text))
  clean_text = tolower(clean_text)
                      
  clean_text
}

yelp$text <- f_clean_text(yelp$text)

# Get sentiments using the 3 different lexicons

bing <- get_sentiment(yelp$text, method="bing")
afinn <- get_sentiment(yelp$text, method="afinn")
nrc <- get_sentiment(yelp$text, method="nrc")

sentiments <- data.frame(bing, afinn, nrc, yelp$date)

head(sentiments,5)

# All 3 are mostly located above 0 so positive sentiment. Overlap between BING and NRC and FINN is also mostly positive.
# They look pretty consistent/correlated.

plot_ly(sentiments, x=~yelp.date, y=~bing, type="scatter", mode="lines", name="bing") %>%
  add_trace(y=~afinn, mode="markers", name="afinn") %>%
  add_trace(y=~nrc, mode="markers", name="nrc") %>%
  layout(title="Comparison of sentiments of Yelp reviews",
         yaxis=list(title="score"), xaxis=list(title="date"))

# get the emotions using the NRC dictionary

emotions <- get_nrc_sentiment(yelp$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments

plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for reviews on Yelp")

yelp <- read.csv("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Text Mining(Classification) - Yelp Reviews\\yelp.csv")
head(yelp,1)

# Make a vector source and a corpus
corpus_review <- Corpus(VectorSource(yelp$text))
corpus_review

#Convert to lower case — this way, if there are 2 words “Dress” and “dress”, it will be converted to a single entry “dress”
corpus_review <- tm_map(corpus_review, tolower)

#Remove punctuation
corpus_review <- tm_map(corpus_review, removePunctuation)

#Remove stopwords: “stopwords” is a very important concept to understand while doing text mining. When we write, the text generally consists of a large number of prepositions, pronouns, conjunctions etc. These words need to be removed before we analyse the text. Otherwise, stopwords will appear in all the frequently used words list and will not give the correct picture of the core words used in the text.
#There is a list of common stopwords used in English which we can view with this command: stopwords(“en”)
stopwords("en")

#Remove stopwords
corpus_review <- tm_map(corpus_review, removeWords, stopwords("english"))

# We might also want to remove custom stopwords based on the context of the text mining. These are words specific to the dataset that may not add value to the text.
# Remove context specific stop words
corpus_review <- tm_map(corpus_review, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))

# In linguistics, stemming is the process of reducing inflected (or derived) words to their word stem, base or root form-generally a written word form.
#The SnowballC package is used for document stemming. For example “complicated”, “complication” and “complicate” will be reduced to “complicat” after stemming. 
#This is again to ensure that the same word is not repeated as multiple versions in the DTM and TDM and we only have the root of the word represented in the DTM and TDM.

## Stem document
corpus_review <- tm_map(corpus_review, stemDocument)
##Viewing the corpus content
corpus_review[[8]][1]

# Find the 20 most frequent terms: term_count
term_count <- freq_terms(corpus_review, 20)
# Plot 20 most frequent terms
plot(term_count)

review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)

review_dtm
review_tdm

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# View the top 10 most common words
review_term_freq[1:10]

# Plot a barchart of the 20 most common words
barplot(review_term_freq[1:20], col = "steel blue", las = 2)

review_word_freq <- data.frame(term = names(review_term_freq),
  num = review_term_freq)

#The word cloud can also receive a set of colors or a color palette as input to distinguish between the more and the lesser frequent words in the cloud.

wordcloud(review_word_freq$term, review_word_freq$num,
  max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))

# Corpus for Yes ratings (ratings of 3,4 and 5)
yelp_yes <- subset(yelp$text, yelp$stars %in% c(3,4,5))
corpus_review_yes <-  Corpus(VectorSource(yelp_yes))

# Corpus for No ratings (ratings of 1 and 2)
yelp_no <- subset(yelp$text, yelp$stars %in% c(1,2))
corpus_review_no <-  Corpus(VectorSource(yelp_no))




#Convert to lower case — this way, if there are 2 words “Dress” and “dress”, it will be converted to a single entry “dress”
corpus_review_yes <- tm_map(corpus_review_yes, tolower)
#Remove punctuation
corpus_review_yes <- tm_map(corpus_review_yes, removePunctuation)
#Remove stopwords: “stopwords” is a very important concept to understand while doing text mining. When we write, the text generally consists of a large number of prepositions, pronouns, conjunctions etc. These words need to be removed before we analyse the text. Otherwise, stopwords will appear in all the frequently used words list and will not give the correct picture of the core words used in the text.
#There is a list of common stopwords used in English which we can view with this command: stopwords(“en”)
##stopwords("en")
#Remove stopwords
corpus_review_yes <- tm_map(corpus_review_yes, removeWords, stopwords("english"))
# We might also want to remove custom stopwords based on the context of the text mining. These are words specific to the dataset that may not add value to the text.
# Remove context specific stop words
corpus_review_yes <- tm_map(corpus_review_yes, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))
## Stem document
corpus_review_yes <- tm_map(corpus_review_yes, stemDocument)


##Viewing the corpus content

corpus_review_yes[[8]][1]

#Convert to lower case — this way, if there are 2 words “Dress” and “dress”, it will be converted to a single entry “dress”
corpus_review_no <- tm_map(corpus_review_no, tolower)
#Remove punctuation
corpus_review_no <- tm_map(corpus_review_no, removePunctuation)
#Remove stopwords: “stopwords” is a very important concept to understand while doing text mining. When we write, the text generally consists of a large number of prepositions, pronouns, conjunctions etc. These words need to be removed before we analyse the text. Otherwise, stopwords will appear in all the frequently used words list and will not give the correct picture of the core words used in the text.
#There is a list of common stopwords used in English which we can view with this command: stopwords(“en”)
##stopwords("en")
#Remove stopwords
corpus_review_no <- tm_map(corpus_review_no, removeWords, stopwords("english"))
# We might also want to remove custom stopwords based on the context of the text mining. These are words specific to the dataset that may not add value to the text.
# Remove context specific stop words
corpus_review_no <- tm_map(corpus_review_no, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))
## Stem document
corpus_review_no <- tm_map(corpus_review_no, stemDocument)


##Viewing the corpus content

corpus_review_no[[11]][1]

# Find the 20 most frequent Yes terms: term_count
term_count_yes <- freq_terms(corpus_review_yes, 30)
# Plot 20 most frequent terms
plot(term_count_yes)

# Find the 20 most frequent No terms: term_count
term_count_no <- freq_terms(corpus_review_no, 30)
# Plot 20 most frequent terms
plot(term_count_no)

review_dtm_yes <- DocumentTermMatrix(corpus_review_yes)
review_tdm_yes <- TermDocumentMatrix(corpus_review_yes)

review_dtm_no <- DocumentTermMatrix(corpus_review_no)
review_tdm_no <- TermDocumentMatrix(corpus_review_no)

# Convert TDM to matrix
review_m_yes <- as.matrix(review_tdm_yes)
# Sum rows and frequency data frame
review_term_freq_yes <- rowSums(review_m_yes)
# Sort term_frequency in descending order
review_term_freq_yes <- sort(review_term_freq_yes, decreasing = T)
# View the top 10 most common words
review_term_freq_yes[1:10]

# Convert TDM to matrix
review_m_no <- as.matrix(review_tdm_no)
# Sum rows and frequency data frame
review_term_freq_no <- rowSums(review_m_no)
# Sort term_frequency in descending order
review_term_freq_no <- sort(review_term_freq_no, decreasing = T)
# View the top 10 most common words
review_term_freq_no[1:10]

# Plot a barchart of the 20 most common words
barplot(review_term_freq_yes[1:20], col = "dark green", las = 2)

# Plot a barchart of the 20 most common words
barplot(review_term_freq_no[1:20], col = "dark red", las = 2)

review_word_freq_yes <- data.frame(term = names(review_term_freq_yes),
  num = review_term_freq_yes)

# Create a wordcloud for the Yes values in word_freqs
wordcloud(review_word_freq_yes$term, review_word_freq_yes$num,
  max.words = 75, colors = "dark green")

review_word_freq_no <- data.frame(term = names(review_term_freq_no),
  num = review_term_freq_no)

# Create a wordcloud for the No values in word_freqs
wordcloud(review_word_freq_no$term, review_word_freq_no$num,
  max.words = 75, colors = "dark red")

## Combine both corpora: all reviews
all_yes <- paste(corpus_review_yes, collapse = "")
all_no <- paste(corpus_review_no, collapse = "")
all_combine <- c(all_yes, all_no)
## Creating corpus for combination
corpus_review_all=Corpus(VectorSource(all_combine)) 
## Pre-processing corpus - all
#Convert to lower-case
corpus_review_all=tm_map(corpus_review_all, tolower)
#Remove punctuation
corpus_review_all=tm_map(corpus_review_all, removePunctuation)
#Remove stopwords
corpus_review_all=tm_map(corpus_review_all, removeWords, stopwords("english"))
corpus_review_all=tm_map(corpus_review_all, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress","just","i"))
#Stem document
corpus_review_all=tm_map(corpus_review_all, stemDocument)
review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m=as.matrix(review_tdm_all)
colnames(all_m)=c("Yes","No")
#Sum rows and frequency data frame
review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term=names(review_term_freq_all), num = review_term_freq_all)


comparison.cloud(all_m,
                 colors = c("green", "red"),
                 max.words = 50)

commonality.cloud(all_m, 
                  colors = "steelblue1",
                  max.words = 50)

# Identify terms shared by both documents
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)

# calculate common words and difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)

# Make a Pyramid plot
top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))

pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 2000,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("Yes",
                            "Words",
                            "No")
             )

review_tdm2 <- removeSparseTerms(review_tdm, sparse = 0.85)
hc <- hclust(d = dist(review_tdm2, method = "euclidean"), method = "complete")

# Plot a dendrogram
plot(hc)

# Create associations
associations <- findAssocs(review_tdm, "food", 0.10)

# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]

# Plot the associations_df values 
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3) + 
  ggtitle("Word Associations to 'food'") + 
  theme_gdocs()

yelp$text <- as.character(yelp$text)
review_bigram <- tokens(yelp$text) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_ngrams(n = 2) %>%
    dfm()

topfeatures(review_bigram)

review_trigram <- tokens(yelp$text) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_ngrams(n = 3) %>%
    dfm()

topfeatures(review_trigram)

# Tokenize descriptions
reviewtokens=tokens(yelp$text,what="word",
remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=TRUE)

# Lowercase the tokens
reviewtokens=tokens_tolower(reviewtokens)

# remove stop words and unnecessary words
rmwords <- c("dress", "etc", "also", "xxs", "xs", "s")
reviewtokens=tokens_select(reviewtokens, stopwords(),selection = "remove")
reviewtokens=tokens_remove(reviewtokens,rmwords)

# Stemming tokens
reviewtokens=tokens_wordstem(reviewtokens,language = "english")
reviewtokens=tokens_ngrams(reviewtokens,n=1:2)

# Creating a bag of words
reviewtokensdfm=dfm(reviewtokens,tolower = FALSE)

# Remove sparsity
reviewSparse <- convert(reviewtokensdfm, "tm")
tm::removeSparseTerms(reviewSparse, 0.7)

# Create the dfm
dfm_trim(reviewtokensdfm, min_docfreq = 0.3)
x=dfm_trim(reviewtokensdfm, sparsity = 0.98)

## Setup a dataframe with features
df=convert(x,to="data.frame")

##Add the Y variable yelp$stars
reviewtokensdf <- cbind(yelp$stars,df)

## Cleanup names
names(reviewtokensdf)[names(reviewtokensdf) == "yelp$stars"] <- "stars"
head(reviewtokensdf)

## Remove the original review$document column
reviewtokensdf <- reviewtokensdf[,-c(2)]
reviewtokensdf$stars <- factor(reviewtokensdf$stars)
head(reviewtokensdf)

reviewtokensdf$stars <- factor(reviewtokensdf$stars)

train_obs <- sample(nrow(reviewtokensdf), 0.80 * nrow(reviewtokensdf), replace = FALSE)
test_obs <- sample(nrow(reviewtokensdf), 0.20 * nrow(reviewtokensdf), replace = FALSE)

train_reviewtokensdf <- reviewtokensdf[train_obs,]
test_reviewtokensdf <- reviewtokensdf[test_obs,]

nrow(reviewtokensdf)
nrow(train_reviewtokensdf)
nrow(test_reviewtokensdf)

## Build the CART model
tree <- rpart(formula = stars ~ ., data = train_reviewtokensdf, method="class",control = rpart.control(minsplit = 200,  minbucket = 30, cp = 0.0001))
printcp(tree)
plotcp(tree)

##Prune down the tree
bestcp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
bestcp
ptree=prune(tree,cp=bestcp)

rpart.plot(ptree,cex = 0.6)
prp(ptree, faclen = 0, cex = 0.5, extra = 2)

#Build the prediction 
res_reviewtokensdf <- predict(tree, newdata = test_reviewtokensdf, type="class")
head(res_reviewtokensdf)

check_accuracy1 <- as.data.frame(cbind(predicted = res_reviewtokensdf, actual = test_reviewtokensdf$stars))
confMat <- table(check_accuracy1$actual, check_accuracy1$predicted)
accuracy <- sum(diag(confMat))/sum(confMat)*100
cat("% Accuracy of Decision Tree model: ", accuracy)

write.csv(check_accuracy1, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Text Mining(Classification) - Yelp Reviews\\DT_predictions.csv")
saveRDS(tree, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Text Mining(Classification) - Yelp Reviews\\DT_model.rda")

# Rename 'next' column because it is a reserved keyword in R
colnames(reviewtokensdf)[colnames(reviewtokensdf)=="next"] <- "NEXT"

colnames(train_reviewtokensdf)[colnames(train_reviewtokensdf)=="next"] <- "NEXT"
colnames(test_reviewtokensdf)[colnames(test_reviewtokensdf)=="next"] <- "NEXT"

reviewRF <- randomForest(stars ~ ., data=train_reviewtokensdf)
varImpPlot(reviewRF, cex=.7)

#Build the prediction 
res_reviewtokensdf <- predict(reviewRF, newdata = test_reviewtokensdf, type="response")
head(res_reviewtokensdf)

check_accuracy2 <- as.data.frame(cbind(predicted = res_reviewtokensdf, actual = test_reviewtokensdf$stars))
confMat <- table(check_accuracy2$actual, check_accuracy2$predicted)
accuracy <- sum(diag(confMat))/sum(confMat)*100
cat("% Accuracy of RandomForest model: ", accuracy)

write.csv(check_accuracy2, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Text Mining(Classification) - Yelp Reviews\\RF_predictions.csv")
saveRDS(reviewRF, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Text Mining(Classification) - Yelp Reviews\\RF_model.rda")

svm_model <- train(stars ~ ., data = train_reviewtokensdf, method = 'svmLinear3')

plot(svm_model)

#Build the prediction 
res_reviewtokensdf <- predict(svm_model, newdata = test_reviewtokensdf)
head(res_reviewtokensdf)

check_accuracy3 <- as.data.frame(cbind(predicted = res_reviewtokensdf, actual = test_reviewtokensdf$stars))
confMat <- table(check_accuracy3$actual, check_accuracy3$predicted)
accuracy <- sum(diag(confMat))/sum(confMat)*100
cat("% Accuracy of SVM with Linear Kernel model: ", accuracy)

write.csv(check_accuracy3, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Text Mining(Classification) - Yelp Reviews\\SVM_predictions.csv")
saveRDS(svm_model, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Text Mining(Classification) - Yelp Reviews\\SVM_model.rda")
