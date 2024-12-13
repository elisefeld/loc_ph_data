library(tidyverse)
library(tidytext)
library(stopwords)

tokenize_words <- function(data, text_column) {
  words <- data |>
    mutate(row_id = row_number()) |>
    unnest_tokens(word, text_column, token = "words") |>
    filter(str_detect(word, "[a-z]"))  #filter out Hebrew characters and numbers
  
  return(words)
}



tokenize_ngrams <- function(data, text_column) {
  ngram <- data |>
    unnest_tokens(bigram, text_column, token = "ngrams", n = 2) |>
    filter(bigram != "NA")
  
  return(ngram)
}



remove_stopwords <- function(data) {
  smart_stopwords <- get_stopwords(source = "smart")
  supported_languages <- stopwords_getlanguages(source = "stopwords-iso")
  
  # Removing English Stop Words
  data <- data |>
    anti_join(smart_stopwords, by = "word")
  
  # Removing Non-English Stop Words
  data <- data |>
    filter(data$iso_code %in% supported_languages & !is.na(data$iso_code))
    
  for (iso_code in unique(data$iso_code)) {
    stopwords <- get_stopwords(language = iso_code, source = "stopwords-iso")
    
    data <- data |>
      anti_join(stopwords, by = "word")
  }
  return(data)
}
  

analyze_bing_sentiment <- function(data) {
  bing_sentiments <- get_sentiments("bing")
  bing <- data |>
    inner_join(bing_sentiments, by = "word")
  
  bing_avg <- bing |>
    mutate(sentiment_value = ifelse(sentiment == "positive", 1, -1)) |>
    group_by(index) |>
    summarize(avg_sentiment = mean(sentiment_value, na.rm = TRUE))
  
  return(list(bing, bing_avg))
}

get_ngrams <- function() {
  ngram_filter <- ngram |>
    separate(bigram, c("word1", "word2"), sep = " ") |>
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) |>
    count(word1, word2, sort = TRUE)
  
  ngram_filter_graph <- ngram_filter |>
    filter(n > 2) |>
    graph_from_data_frame()
}


