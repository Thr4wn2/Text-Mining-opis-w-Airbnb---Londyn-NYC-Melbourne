library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(ggplot2)

londyn <- read.csv(gzfile("london.csv.gz"), stringsAsFactors = FALSE, encoding = "UTF-8")
nyc <- read.csv(gzfile("nyc.csv.gz"), stringsAsFactors = FALSE, encoding = "UTF-8")
melbourne <- read.csv(gzfile("melbourne.csv.gz"), stringsAsFactors = FALSE, encoding = "UTF-8")

#Wyciągamy próbkę 1000 opisów ze względu na wielkość plików

df_londyn <- data.frame(city = "London", text = londyn$description) %>% drop_na() %>% sample_n(1000)
df_nyc <- data.frame(city = "NYC", text = nyc$description) %>% drop_na() %>% sample_n(1000)
df_melbourne <- data.frame(city = "Melbourne", text = melbourne$description) %>% drop_na() %>% sample_n(1000)

data <- rbind(df_londyn, df_nyc, df_melbourne)

corpus <- VCorpus(VectorSource(data$text))

corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

corpus <- tm_map(corpus, toSpace, "<br/>") # Specyficzne dla Airbnb (usuwa entery html)
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)

# Usunięcie słów, które nic nie wnoszą do Airbnb
corpus <- tm_map(corpus, removeWords, c("apartment", "room", "bed", "bedroom", "place", "can", "will", "london", "york", "melbourne"))
corpus <- tm_map(corpus, stripWhitespace)