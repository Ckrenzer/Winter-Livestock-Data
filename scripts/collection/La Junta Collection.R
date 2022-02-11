# Packages ------------------------------------------------------------------------------
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

if(!require(git2r)) install.packages("git2r"); library(git2r)
if(!require(gert)) install.packages("gert"); library(gert)

# The collection() function
source("scripts/collection/functions/collection().R")



# Market Report -------------------------------------------------------------------------
# NOTES:
# This section gets the current URL and sends it to the collection() function.


# BEGIN:
# Starting a browser
browser <- rsDriver(port = 4545L,
                    browser = "firefox",
                    version = "latest",
                    geckover = "latest",
                    verbose = FALSE)

# Assigning the browser client to an object
remote_driver <- browser[["client"]]

# Navigate to the La Junta webpage
remote_driver$navigate("http://www.winterlivestock.com/lajunta.php#marketreport")

# the xpath of the current market report
xpath_value <- "/html/body/div/div[3]/div[7]/div/div/table/tbody/tr/td[1]/a"
# Click on the link to the most current market report
remote_driver$findElements(using = "xpath", value = xpath_value)[[1]]$clickElement()

# Getting the current URL
urls <- remote_driver$getCurrentUrl()[[1]]

# Closing the browser
remote_driver$closeall()

# Try to append market report data to the csv,
# saving the logical value returned indicating success
# or failure of the market report addition
appended_new_data <- collection(urls = urls, prevent_use_of_previous_urls = TRUE)



# UPLOAD --------------------------------------------------------------------------------
# UPLOAD NOTES:
# Commit and push our changes to the repo. Uncomment this section if you wish
# to use it. You will have to provide the path to this repo as a string to `repository_path`.


# UPLOAD:
# We only try to commit and push if we changed the repo and have access
#if(appended_new_data){
#  git2r::commit(repo = repository_path, message = "Weekly Market Update", all = TRUE, session = TRUE)
#  gert::git_push(repo = repository_path, verbose = FALSE)
#}
