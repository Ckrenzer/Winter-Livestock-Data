# Packages ------------------------------------------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(RSelenium, rvest, htmltools, htmlwidgets,
               stringr, tidyr, readr, dplyr, lubridate, clock,
               git2r)

# The collection() function
source("Collection/functions/collection().R")

# When getwd() is called from the task scheduler, it will only recognize the
# path of the script (as opposed to the project). Therefore, we want to move
# to the parent directory of Collection/
repository_path <- str_extract(getwd(), ".*(?=/Collection)")
setwd(repository_path)



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
# Commit and push our changes to the repo.
# The Personal Access Token (PAT) needs updating every 90 days.
# Follow these instructions:
# https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token
# Basically, this means generating a new PAT and overwriting PAT.txt with the new code.


# UPLOAD:
# We only try to commit and push if the repo was changed
if(appended_new_data){
  GITHUB_PAT <- read_lines("PAT.txt")
  git2r::commit(repo = repository_path, message = "Weekly Market Update", all = TRUE, session = TRUE)
  git2r::push(object = repository_path, credentials = cred_token())
}