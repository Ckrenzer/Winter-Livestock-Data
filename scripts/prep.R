# File: prep.R        Consider a better name for this script
# Date: 11/19/2022
# Description:
#   Holds functions used to collect and tidy market reports.


# Parses the HTML from the URL and stores the sale information
# in a list of data frames: one data frame for each URL.
# Preserves all information from the original text.
raw_data_extraction <- function(urls){
    str2df <- function(x) as.data.table(matrix(x, ncol = length(x)))
    urls <- unique(urls)

    salesinfo <- structure(vector("list", length(urls)), names = urls)
    for(url in urls){

        # Load, pre-process the text with awk
        reportID <- str_extract(url, "(?<=reportID\\=)\\d+")
        reportfile <- str_glue("data-info/reports/wl_reportID{reportID}.html")
        if(!file.exists(reportfile)){
            # Pay the server tax
            Sys.sleep(runif(1, 15, 25))
            shell(str_glue("curl {url} -o {reportfile}"),
                  mustWork = TRUE)
            writeLines(reportID, "data-info/reports/wl_reportIDs.txt")
        }
        lajunta <- shell(str_glue("gawk -f scripts/lajunta.awk -v url={url} {reportfile}"),
                         intern = TRUE,
                         mustWork = TRUE)


        # Get the data into workable format
        obs <- str_split(lajunta, "\\ *(DATE|MARKET|URL):\\ *")
        obs <- lapply(obs, str2df)
        obs <- rbindlist(obs)
        setnames(obs, c("sale", "date", "market", "url"))
        lowercase_cols <- c("sale", "market")
        obs[, (lowercase_cols) := lapply(.SD, str_to_lower), .SDcols = lowercase_cols]

        # Put the reportID in its own field
        obs[, reportid := reportID]

        # Organize fields into columns
        # (extracting from the end of the string, the price, until
        # the beginning, the buyer's name, because buyer names have
        # far less structure than the rest of the string)
        patt <- "[\\d\\.]+$"
        obs[, price := str_extract(sale, patt)]
        obs[, sale := str_remove(sale, patt)]

        patt <- "\\d+\\s*$"
        obs[, weight := str_extract(sale, patt)]
        obs[, sale := str_remove(sale, patt)]

        patt <- "(?<=\\d)[^\\d]+$"
        obs[, type := str_extract(sale, patt)]
        obs[, sale := str_remove(sale, patt)]

        patt <- "\\d+\\s*$"
        obs[, quantity := str_extract(sale, patt)]
        obs[, sale := str_remove(sale, patt)]

        # All that's left should be the buyer
        setnames(obs, "sale", "buyer")
        obs[, (colnames(obs)) := lapply(.SD, str_trim), .SDcols = colnames(obs)]


        # Get reproductive typing
        patt <- "[^[:space:]]+$"
        obs[, reprod := str_extract(type, patt)]
        obs[, type := str_trim(str_remove(type, patt))]


        setcolorder(obs, c("market", "date", "buyer", "quantity", "weight",
                           "price", "type", "reprod", "url", "reportid"))
        salesinfo[[url]] <- obs[]; rm(obs)
    }
    salesinfo
}

# Ensures the values from the scraper came through properly.
# These checks must pass before continuing with the process.
# No news is good news.
raw_data_validation <- function(saleslist = NULL, urls = NULL){
    reportIDs_file <- "data-info/reports/wl_reportIDs.txt"
    salescols <- c("market", "date", "buyer", "quantity", "weight",
                   "price", "type", "reprod", "url", "reportid")

    stopifnot(!is.null(saleslist), !is.null(urls), file.exists(reportIDs_file))
    reportIDs_stored <- sort(readLines(reportIDs_file))
    reportIDs <- sort(unique(str_extract(urls, "(?<=reportID\\=)\\d+")))

    # Ensure the data was stored properly
    if(length(reportIDs_stored) != length(unique(reportIDs_stored))){
        report_counts <- table(reportIDs_stored)
        stop(sprintf("The following Report IDs have been used more than once:\n%s",
                     paste(names(report_counts[report_counts > 1]), collapse = "\n")))
    }
    unrecorded_IDs <- reportIDs %in% reportIDs_stored
    if(!all(unrecorded_IDs)){
        stop(sprintf("The following Report IDs were used to collect data but not recorded:\n%s",
                     paste(urls[unrecorded_IDs], collapse = "\n")))
    }
    if(!is.list(saleslist)){
        stop("`saleslist` is not a list!")
    }
    if(length(saleslist) != length(urls)){
        stop("The length of `saleslist` does not match the number of URLs!")
    }
    if(!all(sort(names(saleslist)) == sort(urls))){
        stop("The names of `saleslist` are not the values of `urls`!")
    }

    for(url in urls){
        # Ensure each element of the list is a data frame with the required fields
        if(!is.data.frame(saleslist[[url]])){
            stop(sprintf("`saleslist`[[\"%s\"]] is not a data.frame!", url))
        }
        if(nrow(saleslist[[url]]) == 0){
            stop(sprintf("`saleslist`[[\"%s\"]] does not have any observations!", url))
        }
        dfcols <- colnames(saleslist[[url]])
        cols_in_dfcols_not_in_salescols <- setdiff(dfcols, salescols)
        if(length(cols_in_dfcols_not_in_salescols) > 0){
            stop(sprintf("`saleslist`[[\"%s\"]] contains extra columns:\n%s",
                         url,
                         paste(cols_in_dfcols_not_in_salescols, collapse = "\n")))
        }
        cols_in_salescols_not_in_dfcols <- setdiff(salescols, dfcols)
        if(length(cols_in_salescols_not_in_dfcols) > 0){
            stop(sprintf("`saleslist`[[\"%s\"]] is missing these columns:\n%s",
                         url,
                         paste(cols_in_salescols_not_in_dfcols, collapse = "\n")))
        }

        # Ensure the values in each field came through properly
        check_missingness <- function(df, field){
            if(any(is.na(df[[field]]) | df[[field]] == "")){
                stop(sprintf("`saleslist`[[\"%s\"]] has NAs or an empty string in the %s field!", url, field))
            }
        }
        check_digitsonly <- function(df, field){
            if(any(str_detect(df[[field]], "^[^ \\d.]$"))){
                stop(sprintf("`saleslist`[[\"%s\"]] has non-digits in the %s field!", url, field))
            }
        }
        check_nonumbers <- function(df, field){
            if(any(str_detect(df[[field]], "\\d"))){
                stop(sprintf("`saleslist`[[\"%s\"]] has digits in the %s field!", url, field))
            }
        }
        for(col in dfcols) check_missingness(df = saleslist[[url]], field = col)
        for(col in c("quantity", "weight", "price")) check_digitsonly(df = saleslist[[url]], field = col)
        for(col in c("type", "reprod")) check_nonumbers(df = saleslist[[url]], field = col)
        if(any(saleslist[[url]][, str_detect(type, "\\d")])){
            stop(sprintf("`saleslist`[[\"%s\"]] has numbers in the type field!", url))
        }
        if(!all(saleslist[[url]][["url"]] == url)){
            stop(sprintf("The url field of `saleslist`[[\"%s\"]] is not equal to \"%s\"!", url, url))
        }
    }
    suppressWarnings(file.remove(reportIDs_file))
    writeLines(reportIDs_stored, reportIDs_file)
}

# Extract or impute dates
refine_date <- function(saleslist){}
# Extract or impute market
refine_market <- function(saleslist){}
# date-reportID-market distinctness check
refine_duplicaterecords <- function(saleslist){}
# Clean attributes
refine_attributes <- function(saleslist){}

