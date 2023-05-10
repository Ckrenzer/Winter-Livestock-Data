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
    permitted_markets <- readLines("data-info/valid_markets.txt")
    past_reportIDs <- as.integer(readLines("data-info/reports/wl_reportIDs.txt"))

}

{# Load Market Reports ---------------------------------------------------------

    reportIDs <- identify_possible_reportIDs(previous_reportIDs = past_reportIDs)
    lajunta <- raw_extraction(reportIDs = reportIDs, previous_reportIDs = past_reportIDs)
    reportIDs <- as.integer(readLines("data-info/reports/wl_reportIDs.txt"))
    raw_validation(saleslist = lajunta, reportIDs = reportIDs)
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

