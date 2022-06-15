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
  # Setup ---------------------------------------------------------------------
  start_width <- getOption("width")
  options(width = 1000L)
  # The file should have the data from the most recent market report.
  lajunta <- readr::read_csv("data/ljmr.csv", show_col_types = FALSE)
  
  
  # Error Checking ------------------------------------------------------------
  # Failure to pass a check prevents changes to the repo from being pushed.
  # Used instead of an error to allow all tests to execute.
  write_log <- function(msg, append = TRUE) cat(msg, file = out, append = append)
  failure <- function(msg, df = NULL){
    assign(x = "appended_new_data", value = FALSE, pos = .GlobalEnv)
    write_log(paste0("\n", msg), append = TRUE)
    if(!is.null(df)){
      write_log("")
      capture.output(df, file = out, append = TRUE)
    }
  }
  
  write_log("COLLECTION ERRORS:\n", append = FALSE)
  
  # Finding market reports using the most recent market report instead of the permalink.
  has_permalinks <- any(lajunta$URL %in% c("http://www.winterlivestock.com/lajunta.php",
                                           "http://www.winterlivestock.com/lajunta.php?reportID=#marketreport"))
  if(has_permalinks){
    failure("A URL used in the file is a temporary link!\n")
  }
  
  # Check for missing values
  rows_with_missing_values <- lajunta %>% 
    dplyr::filter(if_any(.cols = everything(), .fns = is.na))
  if(length(rows_with_missing_values) > 0){
    failure(msg = "Missing values were identified in the data.\n",
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
  
  
  # Reset Options -------------------------------------------------------------
  message("Log Updated.")
  options(width = start_width)
})
