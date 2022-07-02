# Packages --------------------------------------------------------------------
if(!require(RSelenium)) install.packages("RSelenium"); library(RSelenium)
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(htmltools)) install.packages("htmltools"); library(htmltools)
if(!require(htmlwidgets)) install.packages("htmlwidgets"); library(htmlwidgets)

if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
if(!require(readr)) install.packages("readr"); library(readr)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(lubridate)) install.packages("lubridate"); library(lubridate)
if(!require(clock)) install.packages("clock"); library(clock)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

if(!require(git2r)) install.packages("git2r"); library(git2r)
if(!require(gert)) install.packages("gert"); library(gert)

# The collection() function
path_functions <- "scripts/collection/lajunta/functions"
source(str_glue("{path_functions}/collection.R"))


# Get URL ---------------------------------------------------------------------
# Start a browser
browser <- rsDriver(port = 4545L,
                    browser = "firefox",
                    version = "latest",
                    geckover = "latest",
                    verbose = FALSE)

# Browser client object
remote_driver <- browser[["client"]]

# Open the La Junta webpage
remote_driver$navigate("http://www.winterlivestock.com/lajunta.php#marketreport")

# the xpath of the current market report
xpath_value <- "/html/body/div/div[3]/div[7]/div/div/table/tbody/tr/td[1]/a"
# Click on the link to the most current market report
remote_driver$findElements(using = "xpath", value = xpath_value)[[1]]$clickElement()

# Let the page load in...
Sys.sleep(3.2)
# Extract the current URL
urls <- remote_driver$getCurrentUrl()[[1]]
# Give the program a few seconds to complete the task
Sys.sleep(3.2)

# Close the browser, close the server, and kill all java instances
remote_driver$closeall()
browser$server$stop()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)


# Append New Data -------------------------------------------------------------
# Try to append market report data to the csv,
# saving the logical value returned indicating success
# or failure of the market report addition
appended_new_data <- collection(urls = urls, prevent_use_of_previous_urls = TRUE)


# Run tests -------------------------------------------------------------------
# Performs checks, preventing automatic commits by setting appended_new_data
# to FALSE when issues are found, and writes to the log.
# Only runs when new data is added to the csv file.
if(appended_new_data){
  # If the current working directory is not the project's root directory,
  # assume the script was called from rscript.exe and the working directory
  # is in scripts/collection/lajunta/.
  # Move back to the parent directory if this is the case.
  reponame <- "Winter-Livestock-Data"
  rootdir <- normalizePath("/", winslash = "/")
  while(basename(getwd()) != reponame & getwd() != rootdir) setwd(dirname(getwd()))
  
  # Write to the log and print the results to the R console
  # (overwrites previous log).
  out <- "log/lajunta.txt"
  source("tests/test_lajunta_collection.R")
  cat(read_lines(out), sep = "\n")
}


# UPLOAD ----------------------------------------------------------------------
# UPLOAD NOTES:
# Commit and push changes to the repo. Change the user if you wish to use this method.
#
# You have to provide the path to this repo as a string to `repository_path`.
if(Sys.info()["effective_user"] == "crkre" & appended_new_data){
  git2r::commit(repo = getwd(), message = "Weekly Market Update", all = TRUE, session = TRUE)
  gert::git_push(repo = getwd(), verbose = FALSE)
}
