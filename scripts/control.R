# File: control.R
# Date: 3/12/2021
# Description:
#   Orchestrates the collection process.


{# Setup -----------------------------------------------------------------------

    installed_packages <- rownames(installed.packages())
    if(!"data.table" %in% installed_packages) install.packages("data.table"); library(data.table)
    if(!"stringr" %in% installed_packages) install.packages("stringr"); library(stringr)
    if(!"Rcpp" %in% installed_packages) install.packages("Rcpp"); library(Rcpp)
    if(!"TAF" %in% installed_packages) install.packages("TAF");

    source("scripts/prep.R")

    urls <- local({
        webpage <- readLines("https://www.winterlivestock.com/lajunta.php")
        reportID <- str_subset(webpage, "Current Report")
        if(length(reportID) == 0){
            stop("Current Report not found!")
        } else if(length(reportID) > 1){
            stop("More than one match found for Current Report!")
        }
        reportID <- str_extract(reportID, "\\d+")
        str_glue("https://www.winterlivestock.com/lajunta.php?reportID={reportID}#marketreport")
    })

}

{# Load Market Reports ---------------------------------------------------------

    urls <- c(str_c("https://www.winterlivestock.com/lajunta.php?reportID=",
                    readLines("data-info/reports/wl_reportIDs.txt"),
                    "#marketreport"),
              urls)
    permitted_markets <- readLines("data-info/reports/valid_markets.txt")

    lajunta <- raw_extraction(urls = urls)
    raw_validation(saleslist = lajunta, urls = urls)
    lajunta <- refine_date(saleslist = lajunta)
    lajunta <- refine_market(saleslist = lajunta)
    ready_to_save <- refine_validation(saleslist = lajunta, valid_markets = permitted_markets)
    if(ready_to_save){
        lajunta <- rbindlist(lajunta)
        fwrite(lajunta, "data-info/wl_raw.csv.gz", compress = "gzip")
        lajunta <- lajunta[, .(market, date, buyer, quantity, type, reprod, weight, price, reportid)]
    }
    lajunta <- clean_attributes(lajunta)
    ready_to_save <- clean_validation(lajunta)
    if(ready_to_save){
        fwrite(lajunta, "data-info/wl_market_reports.csv")
    }

}

