# Pearl Harbor Newspaper Content Analysis

Overview 
----------------
For our final project, we analyzed the text from the front page of newspapers across the United States three months before and after the bombing of Pearl Harbor. Our Shiny application provides an overview of different relationships between the newspapers' words, publishers, dates, and sentiments. We sourced our data from the Library of Congress Chronicling America project, which contains historical American newspapers from 1756 to 1963. The data was accessed through the Chronicling America API using the httr2 and jsonlite packages.

Instructions
----------------
To run the project:
  1. Clone the repo into RStudio
  2. Navigate to "flexdashboard.Rmd"
  3. Select "Run Document"

This will run the Shiny application locally.


Data 
----------------
The data used for the analysis is stored in the Data folder. 
All data files were filtered to include English results only. 
"pearl_data.rds" contains data for dates between September 7th, 1941 and March 7th, 1941. 
"data_prep.R" and "text_analysis.R" contain functions used to prepare and analyze the data.
