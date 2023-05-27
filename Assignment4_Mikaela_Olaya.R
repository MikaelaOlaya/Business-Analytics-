library(data.table)
library(tidytext)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(tm)
library(topicmodels)
library(slam)
library(tidyr)

data <- read.csv("psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

#3.1
colnames(data)

#3.2
dim(data)

#4 & 4.1
library(dplyr)
library(tidytext)

text_df = tibble(line = 1:8360, text = data$q_content)
tidy_text = text_df %>%  
  unnest_tokens(word, text)
data(stop_words)
tidy_text <- tidy_text %>% 
  anti_join(stop_words)
tidy_text %>%  
  count(word, sort= TRUE)


#4.2
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 2000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat =  "identity") + 
  xlab(NULL) + 
  coord_flip()

#4.3
#short answer 

#4.4
install.packages("SnowballC", repos = "https://cran.r-project.org")
install.packages("SnowballC")
library(SnowballC)
tidy_text <- tidy_text %>%
  mutate(word = wordStem(word))
data("stop_words")
tidy_text <- tidy_text %>% 
  anti_join(stop_words, by = "word")
tidy_text %>% 
  count(word, sort = TRUE)
  
#4.4.2
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 4000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat =  "identity") + 
  xlab(NULL) + 
  coord_flip()

#4.4.3
install.packages("wordcloud")
install.packages("reshape2")

library(wordcloud)
library(reshape2)

tidy_text %>% 
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 200))

#4.4.4
tidy_text %>% 
  inner_join(get_sentiments("bing")) %>% 
  dplyr::count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 200)

#5.1
library(dplyr)
library(tidytext)

text_df = tibble(line = 1:8360, text = data$answers)
tidy_text = text_df %>%  
  unnest_tokens(word, text)
data(stop_words)
tidy_text <- tidy_text %>% 
  anti_join(stop_words)
tidy_text %>%  
  count(word, sort= TRUE)

#5.2
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 4000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat =  "identity") + 
  xlab(NULL) + 
  coord_flip()

#5.3
install.packages("SnowballC", repos = "https://cran.r-project.org")
install.packages("SnowballC")
library(SnowballC)

tidy_text <- tidy_text %>%
  mutate(word = wordStem(word))
data("stop_words")
tidy_text <- tidy_text %>% 
  anti_join(stop_words, by = "word")
tidy_text %>% 
  count(word, sort = TRUE)

#5.3.2
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 6000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat =  "identity") + 
  xlab(NULL) + 
  coord_flip()

#5.3.3
install.packages("wordcloud")
install.packages("reshape2")

library(wordcloud)
library(reshape2)
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 200))

#5.3.4
tidy_text %>% 
  inner_join(get_sentiments("bing")) %>% 
  dplyr::count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)

#6
library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.

#6.1
install.packages("ldatuning")
library(ldatuning)
lda
lda_td <- tidy(lda)
lda_td
top_terms <- lda_td %>%
  group_by(topic)%>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill= factor(topic))) +
  geom_col(show.legend= F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

#6.2
#short answer

#6.3.1
lda <- LDA(dtm.new, k = 2)
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

#6.3.2
lda <- LDA(dtm.new, k = 3)
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

#6.3.3
lda <- LDA(dtm.new, k = 4)
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

#6.3.4
lda <- LDA(dtm.new, k = 10)
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

#6.3.5
#short answer 

#7
data <- data[1:1000,]
corpus2 <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm2 <- DocumentTermMatrix(corpus2, control = list(stopwords = TRUE,
                                                   minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  
                                                   stemDocument = TRUE))

#7.1
rowTotals2 <- apply(dtm , 1, sum)
dtm.new2   <- dtm[rowTotals> 0, ]
lda2 <- LDA(dtm.new2, k = 10)
lda_td2 <- tidy(lda2)
lda_td2
top_terms2 <- lda_td2 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms2 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.2
#short answer 

#7.3.1
lda_k2 <- LDA(dtm.new2, k = 2)
lda_td_k2 <- tidy(lda_k2)
top_terms2 <- lda_td_k2 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms2 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.3.2
lda_k8 <- LDA(dtm.new2, k = 8)
lda_td_k8 <- tidy(lda_k8)
top_terms8 <- lda_td_k8 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms8 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.3.3
lda_k11 <- LDA(dtm.new2, k = 11)
lda_td_k11 <- tidy(lda_k11)
top_terms11 <- lda_td_k11 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_terms11 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.3.4
lda_k14 <- LDA(dtm.new2, k = 14)
lda_td_k14 <- tidy(lda_k14)
top_termsk14 <- lda_td_k14 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

top_termsk14 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.3.5
#results recommendation

#8
#recommendation

