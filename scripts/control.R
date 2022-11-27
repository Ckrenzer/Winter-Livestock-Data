# File: control.R
# Author: Connor Krenzer (ckrenzer.info@gmail.com)
# Date: 3/12/2021
# Description:
#   Orchestrates the collection process.


{# Setup -----------------------------------------------------------------------

    installed_packages <- rownames(installed.packages())
    if(!"RSelenium" %in% installed_packages) install.packages("RSelenium"); library(RSelenium)
    if(!"data.table" %in% installed_packages) install.packages("data.table"); library(data.table)
    if(!"stringr" %in% installed_packages) install.packages("stringr"); library(stringr)
    if(!"tidyr" %in% installed_packages) install.packages("tidyr"); library(tidyr)
    if(!"lubridate" %in% installed_packages) install.packages("lubridate"); library(lubridate)
    if(!"clock" %in% installed_packages) install.packages("clock"); library(clock)

    source("scripts/prep.R")

}

{# Get URL ---------------------------------------------------------------------

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
    system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)

}

{# Load Market Reports ---------------------------------------------------------

    urls <- c(urls,
              paste0("http://www.winterlivestock.com/lajunta.php?reportID=",
                     readLines("data-info/reports/wl_reportIDs.txt"),
                     "#marketreport"))
    lajunta <- raw_data_extraction(urls = urls)
    raw_data_validation(saleslist = lajunta, urls = urls)

    # Next check that the reportID hasn't been used yet
    # Then come up with a way to create the date (preserve the original date column--make a new one!)
    # Do the same for the market field
    # Then determine if the market-date has appeared in the data already

}

