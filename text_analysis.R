library(tidyverse)
library(tidytext)
library(stopwords)
library(igraph)
library(janitor)

tokenize_words <- function(data, text_column, type) {
  data  <- data |>
    mutate(row_id = row_number()) 
  
    if (type == "ngrams") {
      tokenized <- data |>
        unnest_tokens(word, {{ text_column }}, token = type, n = 2) # tokenize groups of 2
    } 
    if (type == "words") {
      tokenized <- data |>
        unnest_tokens(word, {{ text_column }}, token = type) # tokenize single words
    } 
  
  tokenized <- tokenized |>
    filter(str_detect(word, "[a-z]")) |>  #filter out Hebrew characters and numbers
    filter(!is.na(word))
    
  return(tokenized)
}


remove_stopwords <- function(data) {
  smart_stopwords <- get_stopwords(source = "smart")
  supported_languages <- stopwords_getlanguages(source = "stopwords-iso")
  
  # Removing English Stop Words
  data <- data |>
    anti_join(smart_stopwords, by = "word") 
  
  english <- data |>
    filter(!(data$iso_code %in% supported_languages) | is.na(data$iso_code))
  
  # Removing Non-English Stop Words
  other_langs <- data |>
    filter(data$iso_code %in% supported_languages & !is.na(data$iso_code))
    
  for (iso_code in unique(other_langs$iso_code)) {
    stopwords <- get_stopwords(language = iso_code, source = "stopwords-iso")
    
    other_langs <- other_langs |>
      anti_join(stopwords, by = "word")
    
    data <- bind_rows(english, other_langs)
  }
  return(data)
}
  

analyze_sentiment <- function(data, type) {
  
  sentiments <- get_sentiments(type)
  
  sentiment <- data |>
    inner_join(sentiments, by = "word") # add sentiments to data
    
  if (type == "bing") {
    # Average Bing Sentiment by Paper
    average <- sentiment |>
      mutate(sentiment_value = ifelse(sentiment == "positive", 1, -1)) |>
      group_by(index) |>
      summarize(avg_sentiment = mean(sentiment_value, na.rm = TRUE))
    
    
    # Average Bing Sentiment by Date
    date <- sentiment |>
      group_by(index, date) |>
      mutate(sentiment_value = ifelse(sentiment == "positive", 1, -1)) |>
      summarize(avg_sentiment = mean(sentiment_value, na.rm = TRUE), .groups = "drop") |>
      arrange(date)
  }
  
  if (type == "afinn") {
    # Average Afinn Sentiment by Paper
    average <- sentiment |>
      group_by(index) |>
      summarize(avg_sentiment = mean(value, na.rm = TRUE))
    
    # Average Afinn Sentiment by Date
    date <- sentiment |>
      group_by(index, date) |>
      summarize(avg_sentiment = mean(value, na.rm = TRUE), .groups = "drop") |>
      arrange(date)
  }
  
    summary <- date |>
      group_by(date) |>
      summarize(avg_sentiment = mean(avg_sentiment, na.rm = TRUE), .groups = "drop")

  
  return(list(sentiment, average, summary))
}



get_ngrams <- function(data) {
  stop_words <- get_stopwords(source = "smart")
  
  ngram_filter <- data |> 
    separate(word, c("word1", "word2"), sep = " ") |> # separate words into two columns
    filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) |>
    count(word1, word2, sort = TRUE) # count the number of times each ngram appears
  
  ngram_filter_graph <- ngram_filter |>
    filter(n > 2) |>
    graph_from_data_frame()
  
  return(ngram_filter_graph)
}

get_keywords_list <- function() {
  Nazi_keywords <- data.frame(word = c("internment",
                                       "camp",
                                       "relocation",
                                       "concentration",
                                       "outsider",
                                       "nazi",
                                       "nazis",
                                       "hitler",
                                       "ghetto",
                                       "swastika",
                                       "holocaust",
                                       "extermination",
                                       "gestapo",
                                       "fuhrer"))
  
  Roma_keywords <- data.frame(word = c("roma",
                                       "gypsy",
                                       "romani",
                                       "sinti",
                                       "roma-american",
                                       "gypsy-american",
                                       "sinti-american",
                                       "romani-american"))
  
  
  
  German_keywords <- data.frame(word = c("german",
                                         "germans",
                                         "german-american",
                                         "aryan",
                                         "kraut",
                                         "hun",
                                         "heinie",
                                         "jerry",
                                         "squarehead",
                                         "boche"))
  
  Jewish_keywords <- data.frame(word = c("jews",
                                         "jewish",
                                         "jew",
                                         "hebrew",
                                         "semite",
                                         "israelite",
                                         "yiddish",
                                         "anti-semitism"))
  
  AAPI_keywords <- data.frame(word = c("japanese",
                                       "japanese-american",
                                       "japan",
                                       "jap",
                                       "issei",
                                       "nisei",
                                       "sansei",
                                       "yonsei",
                                       "chinaman",
                                       "oriental",
                                       "coolie",
                                       "yellowman",
                                       "nipponese",
                                       "nip",
                                       "kanaka",
                                       "hawaii",
                                       "hawaiian",
                                       "polynesian"))
  
  Miscellaneous_keywords <- data.frame(word = c("traitor", 
                                                "spy",
                                                "saboteur",
                                                "alien",
                                                "facist",
                                                "quisling",
                                                "enemy"))
  
  keywords <- bind_rows(list(Nazi_keywords = Nazi_keywords,
                              German_keywords = German_keywords,
                              Roma_keywords = Roma_keywords,
                              Jewish_keywords = Jewish_keywords,
                              AAPI_keywords = AAPI_keywords,
                              Miscellaneous_keywords = Miscellaneous_keywords),
                         .id = "id") # combine all keywords with an id column) # combine all keywords with an id column
  return(keywords)
}

get_keywords <- function(data, key_words_list) {
  keywords_data <- data |>
    inner_join(key_words_list, by = "word") # add keywords to data
  
  keywords_data <- keywords_data |>
    mutate(id = str_remove(id, "_keywords"))
  
  return(keywords_data)
  
}