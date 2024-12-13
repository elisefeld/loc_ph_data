library(tidyverse)
library(tidytext)
library(stopwords)

tokenize_words <- function(data = pearl_data, text_column = "") {
  words <- data |>
    mutate(row_id = row_number()) |>
    unnest_tokens(word, text_column, token = "words") |>
    filter(str_detect(word, "[a-z]"))  #filter out Hebrew characters and numbers
  
  return(words)
}



tokenize_ngrams <- function(data = pearl_data, text_column = "") {
  ngram <- data |>
    unnest_tokens(bigram, text_column, token = "ngrams", n = 2) |>
    filter(bigram != "NA")
  
  return(ngram)
}



remove_stopwords <- function(data = words) {
  words_clean <- data |>
  anti_join(smart_stopwords) |>
    group_by(iso_code) |>
    nest() |>
    mutate(cleaned_data = map2(data, iso_code, ~ {
      stopwords <- get_stopwords(iso_code = .y, source = "stopwords-iso")
      .x |>
        anti_join(stopwords, by = "word") |>
        count(word, sort = TRUE)
    })
    ) |>
    unnest(cleaned_data)

  return(words_clean)
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


