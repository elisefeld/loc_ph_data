library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)


base_url <- "https://www.loc.gov/collections/chronicling-america/"
final_results <- data.frame()

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


get_json_data <- function(params) { 
  req <- request(base_url) |> # 
    req_throttle(rate = 20/10) |> # do not exceed LOC rate limit 
    req_url_query(!!!params) # add query parameters
  
  response <- req |>
    req_perform() |> 
    resp_body_string() |> # return utf-8 string
    fromJSON() # convert from JSON
  
  return(response)
}



parse_json_data <- function(response) {
  data <- flatten(response$results)
  has_more <- !is.null(response$pagination["next"][[1]]) # check if there are more pages
  print(paste0(response$pagination$to, "/", response$pagination$of)) # print the status of the request
  print(paste0("Has more pages: ", has_more)) # print if there are more pages
  mylist <- list(data, has_more) 
  return(mylist)
}



get_paginated_data <- function(display_level = "page",
                               start_date = "",
                               end_date = "",
                               location_state = "",
                               search_type = "advanced",
                               sp = "1",
                               fa = "",
                               front_pages_only = "true"){
  final_results <- data.frame()
  page <- 1
  has_more <- TRUE
  
  while (has_more) { # loop over all pages until there are none left
    print(paste0("Getting page ", page, "..."))
    
    # Set the query parameters
    params <-  list(dl = display_level,
                    start_date = start_date,
                    end_date = end_date,
                    number_page = page,
                    searchType = search_type,
                    front_pages_only = front_pages_only,
                    sp = page,
                    fa = fa,
                    fo = "json",
                    at = "results,pagination")
    
    response <- get_json_data(params)
    mylist <- parse_json_data(response)
    
    parsed <- mylist[[1]]
    has_more <- mylist[[2]]
    
    final_results <- bind_rows(final_results, parsed)
    
    page <- page + 1 # proceed to next page
  }
  print(paste0("Number of results: ", nrow(final_results))) 
  final_results <<- final_results 
}


# I recognize this function appears redundant considering you can input a range directly,
# however when requesting a large range at once we experienced issues with the API
get_all_years <- function(start_year = "", end_year = "") {
  
  all_data <- list()
  
  for (year in start_year:end_year) {
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-12-31")
    
    year_data <- get_paginated_data(start_date = start_date,
                                    end_date = end_date,
                                    fa = "language:english")
    data_list[[as.character(year)]] <- data_year
    
  }
  raw_data <- bind_rows(data_list)
  
  saveRDS(raw_data, "year_data.rds") # save the data to an r file 
  # (can't be saved to a csv because of the nested structure)
  
  return(raw_data)
}



prepare_data <- function(data = "raw_data.rds") {
  raw_data <- readRDS(data)
  pearl_data <- raw_data |>
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
    filter(as_date >= "1941-09-07" & as_date <= "1942-03-07") |> # filter by date range
    mutate(before_pearl_harbor = ifelse(as_date < "1941-12-07", "Before Pearl Harbor", "After Pearl Harbor"), # create a new column for before or after Pearl Harbor
          newspaper = str_remove(newspaper_title, " \\(.*"), # remove anything after parentheses from the newspaper title
          newspaper = str_trim(newspaper)) 
  
  pearl_data <- pearl_data |>
    left_join(iso_to_language, by = "language") # join in the iso codes
  
  return(pearl_data)
}


