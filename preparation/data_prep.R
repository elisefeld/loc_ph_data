library(tidyverse)

iso_to_language <- data.frame( 
  language = c("english",
               "spanish",
               "finnish",
               "yiddish",
               "german",
               "czech",
               "croatian",
               "polish",
               "italian",
               "slovenian",
               "hungarian",
               "romanian",
               "japanese"),
  iso_code = c("en",
               "es",
               "fi",
               "yi",
               "de",
               "cs",
               "hr",
               "pl",
               "it",
               "sl",
               "hu",
               "ro",
               "ja")
)

prepare_data <- function(data) {
  raw_data <- readRDS(data)
  clean_data <- raw_data |>
    select(index,
           date,
           title,
           partof_title,
           description,
           url,
           image_url,
           language,
           location_state,
           location_county,
           location_city,
           contributor,
           publication_frequency,
           subject,
           subject_ethnicity) |>
    mutate(across(where(is.list) & !all_of("subject"), # remove list columns
                  ~ sapply(.x, function(val) paste(val, collapse = ', ')))) |>
    mutate(language = str_remove(language, "[, ]?english[, ]?"), # remove english from the language column
           language = str_remove(language, ","), 
           language = str_trim(language)) |>
    rename(newspaper_title = partof_title,
           text_data = description,
           state = location_state,
           city = location_city,
           ethnicity = subject_ethnicity) |>
    mutate(across(everything(), ~ ifelse(. == "", NA, .))) |> # replace empty strings with NA
    mutate(across(everything(), ~ ifelse(. == "NULL", NA, .))) |> # replace NULL with NA
    mutate(as_date = ymd(date)) |> # convert date to date format
    mutate(before_pearl_harbor = ifelse(as_date < "1941-12-07", "Before Pearl Harbor", "After Pearl Harbor"), # create a new column for before or after Pearl Harbor
          newspaper = str_remove(newspaper_title, " \\(.*"), # remove anything after parentheses from the newspaper title
          newspaper = str_trim(newspaper)) 
  
  clean_data <- clean_data |>
    left_join(iso_to_language, by = "language") # join in the iso codes
  
  clean_data <<- clean_data
  saveRDS(clean_data, "clean_data.rds")
}


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
