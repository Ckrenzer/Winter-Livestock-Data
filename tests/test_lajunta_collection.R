# File: test_lajunta_collection.R
# Author: Connor Krenzer
# Contact: Ckrenzer.info@gmail.com
# Date: 6/5/2022
# Description:
#   Performs tests on the data to ensure the scraper's integrity,
#   writes information to a log.


# Isolating the tests to ensure they do not conflict
# with another section of the program.
local({
  # The file should have the data from the most recent market report.
  lajunta <- readr::read_csv("data/ljmr.csv", show_col_types = FALSE)
  
  
  # Error Checking --------------------------------------------------------------
  # Failure to pass a check prevents changes to the repo from being pushed.
  # Used instead of an error to allow all tests to execute.
  failure <- function(msg, df = tibble()){
    assign(x = "appended_new_data", value = FALSE, pos = .GlobalEnv)
    message("\n")
    if(nrow(df) > 0) print(df)
    message(msg)
  }
  
  # Finding market reports using the most recent market report instead of the permalink.
  has_permalinks <- any(lajunta$URL == "http://www.winterlivestock.com/lajunta.php")
  if(has_permalinks){
    failure("A URL used in the file is not a permalink!")
  }
  
  # Check for missing values
  rows_with_missing_values <- dplyr::filter(lajunta,
                                            dplyr::if_any(.cols = everything(),
                                                          .fns = is.na)
  )
  if(length(rows_with_missing_values) > 0){
    failure(msg = "Missing values were identified in the data.",
            df = rows_with_missing_values)
  }
  
  # Checks for duplicate values -- no two URLs should have the same date
  dates_with_duplicate_urls <- lajunta %>% 
    dplyr::distinct(URL, Date) %>% 
    dplyr::count(URL, Date) %>% 
    dplyr::filter(n > 1)
  if(nrow(dates_with_duplicate_urls) > 0){
    failure(msg = "No two URLs should have the same date. The scraper missed a case!",
            df = dates_with_duplicate_urls)
  }
  
  
  # Writing Log -----------------------------------------------------------------
  # Write useful versions of these metrics to a file...?
  
  # Counts of all types
  count(lajunta, Type) %>% 
    arrange(desc(n)) %>% 
    as.data.frame()
  
  # How many sales were there on a given day?
  lajunta %>% 
    count(Date)
  
  # Taking a look at the newest data
  tail(lajunta)
  
  # finds counts of all reproductive statuses--there should be five: bull, cow, steer, heifer, and NA (if there are missing values).
  count(lajunta, Reprod) %>% 
    arrange(desc(n)) %>% 
    as.data.frame()
})