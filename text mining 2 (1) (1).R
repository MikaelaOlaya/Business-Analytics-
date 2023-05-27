# topic modeling 
install.packages("tm") # text mining 
install.packages("topicmodels") 
install.packages("slam")
install.packages("tidy")
install.packages("tidyverse")
install.packages("dplyr")

library(tm)
library(topicmodels)
library(slam)
library(tidyr)
library(tidytext)
library(tidyverse)
library(dplyr)


data <- read.csv("tweets_about_sprint.csv")
data <- data[1:1000,] 
data$tweet_id <- seq(nrow(data))
summary(data$tweet_id)

# corpus 
corpus <- Corpus(VectorSource(data$tweet), readerControl = list(language = "en"))

# dtm 
tweet_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE, stemming = TRUE)) 
dim(tweet_dtm)
tweet_dtm$dimnames

# LDA 
# optimal k 
install.packages("ldatuning")
library(ldatuning)

result <- FindTopicsNumber(
  tweet_dtm, 
  topics = seq(from = 2, to = 20 , by = 2), 
  metrics = c("Arun2010", "CaoJuan2009", "Deveaud2014", "Griffiths2004"), 
  method = "Gibbs", 
  control = list(seed = 77)
)

FindTopicsNumber_plot(result)

lda <- LDA(tweet_dtm, k = 10)
lda

lda_td <- tidy(lda)
lda_td


top_terms <- lda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

ap_document <- tidy(lda, matrix= "gamma")

tidy_tweet <- data %>%
  unnest_tokens(word, tweet)
install.packages("stopwords")
install.packages("SnowballC")
library(stopwords)
library(SnowballC)

wordstoremove <- data.frame("word" = c("sprint", "https", "http", "sprintcare", "rt", "get", "amp"))


tidy_tweet <- tidy_tweet %>%
  anti_join(stop_words)%>%
  anti_join(wordstoremove)%>%
  mutate(nchar = nchar(word))%>%
  filter(nchar > 2 & nchar < 12)%>%
  mutate(word= wordStem(word), languages = "english")

#tf - idf 
tweet_words <- tidy_tweet %>%
  count(tweet_id, word, sort= TRUE) %>%
  ungroup()

head(tweet_words)

tweet_words <- tweet_words %>%
  bind_tf_idf(word, tweet_id, n)
view(tweet_words)

ggplot(tweet_words, aes(tf_idf)) +
  geom_histogram()
summary(tweet_words$tf_idf)

tweet_words <- tweet_words %>%
  filter(tf_idf > 0.4577)

#dtm 
tweet_dtm <- tweet_words %>%
  cast_dtm(tweet_id, word, n)

lda <- LDA(tweet_dtm, k = 10)
lda_td2 <- tidy(lda)


top_terms <- lda_td2 %>%
  group_by(topic)%>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill= factor(topic))) +
  geom_col(show.legend= F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

