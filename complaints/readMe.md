# Text analysis
## Customer Complaints

First, we load all the libraries:
```r
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(ggplot2)
library(scales)
```

After loading the library we load the data into r studio
```r
setwd("C:/Users/Rooooohan/Documents/rprojects/complaints")

df<- read.csv('Consumer_Complaints.csv',stringsAsFactors = FALSE)
```

Now to remove any empty data or null data
```r
df_clean <- df %>% 
  select(Consumer.complaint.narrative) %>% 
  filter(!is.na(Consumer.complaint.narrative))
```

Next we will convert all the words to lower text, remove punctuation and tokenized
and also clean up sensitive data that are marked with 'XXXX'. converted to lower case so that it is easy to count when looking for
a total number of common words.
```r
tidy_text <- df_clean %>% 
  unnest_tokens(word, Consumer.complaint.narrative) %>% 
  mutate(word = str_to_lower(word)) %>% 
  anti_join(stop_words)
tidy_text <- tidy_text %>% filter(word != "xxxx")
```

The first graph is about all the common words that are in text which have count of more than 50000 i chose suck a big number as I was 
getting a lot of words that was not showing in the graph properly so had to limit the count number so it easy to analyze.
```r
tidy_text %>%
  count(word, sort = TRUE)

tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 50000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
```

<div align = "center">
<img src = "https://github.com/rohaanfarrukh/data-332/blob/main/complaints/chart/common%20words.png" width = "450")>
</div>

Sentiment Analysis using Bing and plotting the analysis
```r
bing_sentiment <- tidy_text %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment)

bing_sentiment %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Sentiment Analysis using Bing")+
  scale_y_continuous(labels = comma)
```
<div align = "center">
<img src = "https://github.com/rohaanfarrukh/data-332/blob/main/complaints/chart/bing.png" width = "450")>
</div>

Sentiment Analysis using NRC and plotting the analysis
```r
nrc_sentiment <- tidy_text %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)

nrc_sentiment %>% 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Sentiment Analysis using NRC")
```

<div align = "center">
<img src = "https://github.com/rohaanfarrukh/data-332/blob/main/complaints/chart/NRC.png" width = "450")>
</div>



the other chart that I use is world cloud
```r
word_freq <- tidy_text %>% count(word, sort = TRUE)
wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 100, colors = brewer.pal(8, "Dark2"))
```
<div align = "center">
<img src = "https://github.com/rohaanfarrukh/data-332/blob/main/complaints/chart/cloud.png" width = "450")>
</div>

