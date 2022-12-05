# File: prep.R
# Date: 11/19/2022
# Description:
#   Holds functions used to collect and tidy market reports.


# Parses the HTML from the URL and stores the sale information
# in a list of data frames: one data frame for each URL.
# Preserves all information from the original text.
raw_extraction <- function(urls){
    str2df <- function(x) as.data.table(matrix(x, ncol = length(x)))
    cppFunction('CharacterVector locf(CharacterVector x){
                String missingval = "";
                String fillval = missingval;
                CharacterVector fillvec = clone(x);
                for(int i = 0; i < x.size(); i++){
                    if(fillvec[i] != missingval){
                        fillval = fillvec[i];
                    } else {
                        fillvec[i] = fillval;
                    }
                }
                return fillvec;
    }')

    urls <- unique(urls)
    salesinfo <- structure(vector("list", length(urls)), names = urls)
    for(url in urls){

        # Load, pre-process the text with awk
        reportID <- str_extract(url, "(?<=reportID\\=)\\d+")
        reportfile <- str_glue("data-info/reports/wl_reportID{reportID}.html")
        if(!file.exists(reportfile)){
            # Pay the server tax
            Sys.sleep(runif(1, 15, 25))
            shell(str_glue("curl {url} -o {reportfile}"), mustWork = TRUE)
            shell(str_glue("dos2unix {reportfile}"), mustWork = TRUE)
            cat(reportID, "data-info/reports/wl_reportIDs.txt", sep = "\n", append = TRUE)
        }
        lajunta <- shell(str_glue("gawk -f scripts/lajunta.awk -v url={url} {reportfile}"),
                         intern = TRUE,
                         mustWork = TRUE)


        # Get the data into workable format
        obs <- str_split(lajunta, "\\ *(DATE|MARKET|URL):\\ *")
        obs <- lapply(obs, str2df)
        obs <- rbindlist(obs)
        setnames(obs, c("saletext", "datetext", "markettext", "url"))
        lowercase_cols <- c("saletext", "datetext", "markettext")
        obs[, (lowercase_cols) := lapply(.SD, str_to_lower), .SDcols = lowercase_cols]

        # Put the reportID in its own field
        obs[, reportid := reportID]

        # Remove characters after the price in the sale line
        # (lines end with the price, so removing everything after
        # the last digit should do the trick)
        obs[, sale := str_remove(saletext, "[^0-9]+$")]

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
        obs[, buyer := locf(buyer)]


        # Get reproductive typing
        patt <- "[^[:space:]]+$"
        obs[, reprod := str_extract(type, patt)]
        obs[, type := str_trim(str_remove(type, patt))]


        setcolorder(obs, c("markettext", "datetext", "saletext", "buyer", "quantity", "weight",
                           "price", "type", "reprod", "url", "reportid"))
        salesinfo[[url]] <- obs[]; rm(obs)
    }
    salesinfo
}

# Ensures the values from the scraper came through properly.
# These checks must pass before continuing with the process.
# No news is good news.
raw_validation <- function(saleslist = NULL, urls = NULL){
    reportIDs_file <- "data-info/reports/wl_reportIDs.txt"
    salescols <- c("markettext", "datetext", "saletext", "buyer", "quantity", "weight",
                   "price", "type", "reprod", "url", "reportid")

    stopifnot(!is.null(saleslist), !is.null(urls), file.exists(reportIDs_file))
    reportIDs_stored <- as.character(sort(as.integer(readLines(reportIDs_file))))
    reportIDs_asis <- unique(as.integer(str_extract(urls, "(?<=reportID\\=)\\d+")))
    reportIDs <- as.character(sort(reportIDs_asis))
    reportIDs_asis <- as.character(reportIDs_asis)
    if(any(is.na(reportIDs_stored),
           is.na(reportIDs),
           !is.character(reportIDs_stored),
           !is.character(reportIDs))){
        stop("The reportIDs came through as NAs or were not strings!")
    }
    if(!all(reportIDs == reportIDs_asis)){
        stop("The process requires the list to be sorted by report ID.\n",
             "It's likely that the reportIDs file was modified or removed ",
             "outside of the standard scripts!")
    }

    # Ensure the data was stored properly
    if(length(reportIDs_stored) != length(unique(reportIDs_stored))){
        report_counts <- table(reportIDs_stored)
        stop(sprintf("The following Report IDs have been used more than once:\n%s",
                     paste(names(report_counts[report_counts > 1]), collapse = "\n")))
    }
    recorded_IDs <- reportIDs %in% reportIDs_stored
    if(!all(recorded_IDs)){
        stop(sprintf("The following Report IDs were used to collect data but not recorded:\n%s",
                     paste(urls[!recorded_IDs], collapse = "\n")))
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
    writeLines(reportIDs, reportIDs_file)
}

# Extract dates, imputing the year when necessary
refine_date <- function(saleslist){
    extract_date <- function(report_df){
        mnames <- str_to_lower(str_c(month.name, collapse = "|"))
        report_df[, date := datetext]

        # Remove characters before the month name
        report_df[, date := str_remove(date, str_glue("^.*(?=({mnames}))"))]
        # Remove characters after the first period
        report_df[, date := str_remove(date, "\\..*$")]
        # Remove characters after the year
        report_df[, date := str_replace(date, "((?<=[0-9]{4})).*$", "\\1")]

        # Remove date-formatting characters
        report_df[, date := str_remove_all(date, "(?<=\\d)(st|nd|rd|th)|,")]
        # Remove the day of the month for Mondays on multi-day sales
        report_df[, date := str_remove(date, "\\d+\\ +(&|and)")]
        # Remove extra whitespace
        report_df[, date := str_squish(date)]

        # The date should now have [month, day, (optional) year] format
        # Remove anything after this pattern
        report_df[, date := str_extract(date, str_c("^(", mnames, ")\\ \\d{1,2}(\\ \\d{4})?"))]

        report_df[]
    }
    impute_year <- function(saleslist_){
        previous_date <- saleslist_[[1]][1, as.Date(date, "%B %d %Y")]

        for(report_df in saleslist_){
            current_reportID <- report_df[1, reportid]
            current_date <- report_df[1, date]
            missing_year <- str_detect(current_date, "\\d{4}$", negate = TRUE)
            if(missing_year){
                impute_date <- previous_date + 7
                if(wday(impute_date) != 3){
                    stop(sprintf("The imputed date for reportID %s is not a Tuesday!", current_reportID))
                }
                current_date <- str_c(current_date, year(impute_date), sep = " ")
                current_date <- as.Date(current_date, "%B %d %Y")
                if(is.na(current_date)){
                    stop(sprintf("The date for reportID %s is NA!", current_reportID))
                }
                if(current_date - 7 != previous_date){
                    stop(sprintf("The date for reportID %s ", current_reportID),
                         "is not 7 days from the previous date after the ",
                         "year imputation!")
                }
            } else {
                current_date <- as.Date(current_date, "%B %d %Y")
            }
            report_df[, date := as.character(current_date)][]
            previous_date <- current_date
        }

        invisible(saleslist_)
    }
    saleslist <- lapply(saleslist, extract_date)
    impute_year(saleslist)

    saleslist
}

# Extract the market
refine_market <- function(saleslist){
    for(report_df in saleslist){
        report_df[, market := str_extract(markettext, "la\\s?junta")]
        report_df[, market := str_remove_all(market, fixed(" "))]
        report_df[]
    }
    saleslist
}

# Ensure the values created in the refine
# section came through as intended
# Check for one report ID per market-date combo
# No news is good news.
refine_validation <- function(saleslist, valid_markets){
    check_missingness <- function(var, val, reportID_){
        val <- as.character(val)
        if(is.na(val) | val == ""){
            stop(sprintf("The value in the '%s' field (reportID %s) ", var, reportID_),
                 "has NAs or an empty string!")
        }
    }
    # Check for invalid values
    local({
        for(report_df in saleslist){
            market <- report_df[1, market]
            date <- report_df[1, date]
            reportID <- report_df[1, reportid]

            check_missingness("market", market, reportID)
            check_missingness("date", date, reportID)
            if(!str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$")){
                stop(sprintf("The date '%s' (reportID %s) ", date, reportID),
                     "is not represented in YYYY-mm-dd format!")
            }
            if(!market %in% valid_markets){
                stop(sprintf("The market '%s' (reportID %s) ", market, reportID),
                     "is not in valid_markets.txt!")
            }
        }
    })

    df2str <- function(df){
        str_c(capture.output(df), collapse = "\n")
    }
    # Check for market-date distinctness
    reportkey <- lapply(saleslist, function(report_df) report_df[1, .(key = str_c(market, "_", date), reportid)])
    reportkey <- rbindlist(reportkey)
    reportkey <- reportkey[, .(list_index = .I), .(key, reportid)]
    dups <- reportkey[, .N, .(key)][N > 1, key]
    if(length(dups) > 0){
        stop(sprintf("The following reportIDs are duplicates of one another:\n%s",
                     df2str(reportkey[key %in% dups])),
            "\nPick one and remove the other (update wl_reportIDs.txt).")
    }

    # Return TRUE if all tests pass
    TRUE
}

# Clean attributes
clean_attributes <- function(salesdf){
    message("clean_attributes() has not been implemented.")
    salesdf
}

# Verify that the cleaning process went according to plan
clean_validation <- function(salesdf){
    message("clean_validation() has not been implemented.")
    FALSE
}

