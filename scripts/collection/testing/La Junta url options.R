# Play around with this script if you need inspiration on ways to
# scrape the website.


# Packages ------------------------------------------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(rvest, stringr, tidyr, readr, dplyr, lubridate, clock, htmltools, htmlwidgets)

# The collection() function.
source("Collection/functions/collection().R")


# URL CONFIGURATIONS --------------------------------------------------------------------
# This parses together different market report IDs and stores them in a vector
# (useful for when you do not know the URLs beforehand).
#
# The market reports seem to start around reportID == 6700.
# The max URL at the time of writing is around 14000. 
# Data begins in January 2016.
urls <- paste0("http://www.winterlivestock.com/lajunta.php?reportID=", 6680:14000, "#marketreport")


# The most recent market report, without the permalink.
# NOTE: the URL column will not contain the market report number!!!
urls <- "http://www.winterlivestock.com/lajunta.php"


# This contains all the La Junta market report URLs for easy access
# (recommended if you plan to re-collect the data from scratch--just
# do not forget the possibility that some market reports could have
# been missed--if you really think it necessary to do so).
# Be sure to set `prevent_use_of_previous_urls` to FALSE in collection().
urls <- read_lines("Collection/La Junta URLs.txt")
urls <- read_lines("https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/Collection/La%20Junta%20URLs.txt")
#collection(urls = urls, prevent_use_of_previous_urls = FALSE)


# The last-used market report URL (outdated--but the number is the only thing that changes).
urls <- "http://www.winterlivestock.com/lajunta.php?reportID=13783#marketreport"
#collection(urls = urls)