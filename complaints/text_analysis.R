library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(ggplot2)
rm(list = ls())

setwd("C:/Users/Rooooohan/Documents/rprojects/complaints")

df<- read.csv('Consumer_Complaints.csv',stringsAsFactors = FALSE)
head(df)
# Data Cleanup
# Select relevant column and remove NA values
df_clean <- df %>% 
  select(Consumer.complaint.narrative) %>% 
  filter(!is.na(Consumer.complaint.narrative))

# Convert text to lowercase, remove punctuation, and tokenize
tidy_text <- df_clean %>% 
  unnest_tokens(word, Consumer.complaint.narrative) %>% 
  mutate(word = str_to_lower(word)) %>% 
  anti_join(stop_words)

# Sentiment Analysis using Bing
bing_sentiment <- tidy_text %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment)

# Plot Bing Sentiment Analysis
bing_sentiment %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Sentiment Analysis using Bing")

# Sentiment Analysis using NRC
nrc_sentiment <- tidy_text %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)

# Plot NRC Sentiment Analysis
nrc_sentiment %>% 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Sentiment Analysis using NRC")

# Word Cloud
word_freq <- tidy_text %>% count(word, sort = TRUE)
wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 100, colors = brewer.pal(8, "Dark2"))

# Complaints by Product
complaints_by_product <- df %>% 
  count(Product, sort = TRUE) %>%
  top_n(10)

# Plot Complaints by Product
complaints_by_product %>% 
  ggplot(aes(x = reorder(Product, n), y = n, fill = Product)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top 10 Complaints by Product")
