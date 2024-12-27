library(httr2)
library(jsonlite)

base_url <- "https://www.loc.gov/collections/chronicling-america/"
final_results <- data.frame()


# function to get JSON data from the API
get_json_data <- function(parameters) { 
  api_request <- request(base_url) |> 
    req_throttle(rate = 20/10) |> # do not exceed LOC rate limit 
    req_url_query(!!!parameters) # add query parameters
  
  api_response <- api_request |> 
    req_perform() |> # perform the request
    resp_body_string() |> # return utf-8 string
    fromJSON() # convert from JSON
    results <- api_response$results
    pagination <- api_response$pagination
  return(list(results, pagination))
}

parse_json_data <- function(api_response) {
  api_results <- api_response[[1]]
  api_pagination <- api_response[[2]]
  
  has_more <- !is.null(api_pagination["next"][[1]]) # check if there are more pages
  
  print(paste0(api_pagination$to, "/", api_pagination$of)) # print the status of the request
  print(paste0("Has more pages: ", has_more)) # print if there are more pages
  
  api_result <- list(api_results, has_more) 
  return(api_result)
}


get_paginated_data <- function(display_level = "page",
                               start_date = "",
                               end_date = "",
                               location_state = "",
                               search_type = "advanced",
                               sp = "1", # page number
                               fa = "",
                               front_pages_only = "true") {
  newspaper_data <- data.frame()
  page <- 1
  has_more <- TRUE
  
  # loop over all pages until there are none left
  while (has_more) { 
    print(paste0("Getting page ", page, "..."))
    
    # Set the query parameters
    parameters <-  list(dl = display_level,
                    start_date = start_date,
                    end_date = end_date,
                    number_page = page,
                    searchType = search_type,
                    front_pages_only = front_pages_only,
                    sp = page,
                    fa = fa,
                    fo = "json",
                    at = "results,pagination")
    
    api_response <- get_json_data(parameters)
    api_results <- parse_json_data(api_response)
    
    
    parsed_data <- api_results[[1]]
    has_more <- api_results[[2]]
    
    
    newspaper_data <- bind_rows(newspaper_data, parsed_data)
    
    page <- page + 1 # proceed to next page
  }
  print(paste0("Number of results: ", nrow(newspaper_data))) 
  
  saveRDS(newspaper_data, paste0(start_date, "_to_", end_date, "_", "newspapers.rds"))
  # save the data to an r file (can't be saved to a csv because of the nested structure)
  return(newspaper_data)
}


# I recognize this function appears redundant considering you can input a range directly,
# however when requesting a large range at once (1939-1945) we experienced issues with the API
# we didn't end up using the entire dataset though because it was too large.
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
  
  saveRDS(raw_data, "raw_data.rds") # save the data to an r file 
  # (can't be saved to a csv because of the nested structure)
}