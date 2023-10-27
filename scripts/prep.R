# File: prep.R
# Date: 11/19/2022
# Description:
#   Holds functions used to collect and tidy market reports.


# Convert a reportID to a URL
to_url <- function(reportIDs){
    str_c("https://www.winterlivestock.com/lajunta.php?reportID=", reportIDs, "#marketreport")
}

# Identify possible urls to check for market reports
identify_possible_reportIDs <- function(previous_reportIDs){
    newest_reportID <- tryCatch({
        webpage <- suppressWarnings(readLines("https://www.winterlivestock.com/lajunta.php"))
        reportID <- str_subset(webpage, "Current Report")
        if(length(reportID) == 0L){
            stop("Current Report not found!")
        } else if(length(reportID) > 1L){
            stop("More than one match found for Current Report!")
        }
        newest_reportID <- as.integer(str_extract(reportID, "\\d+"))
    }, error = function(cnd){
        warning("Could not identify newest report. Only using historical reportIDs.", immediate. = TRUE)
        max(as.integer(str_extract(previous_reportIDs, "\\d+")))
    })
    if(newest_reportID == max(previous_reportIDs)){
        warning("The newest report ID has already been recorded.", immediate. = TRUE)
        return(previous_reportIDs)
    }
    c(previous_reportIDs, seq(max(previous_reportIDs) + 1L, newest_reportID))
}

# Ensure all files are found on disk
download_reports <- function(reportIDs){
    report_files <- sprintf("data-info/reports/wl_reportID%s.html", reportIDs)
    file_not_found <- !file.exists(report_files)
    report_files <- report_files[file_not_found]
    urls <- to_url(reportIDs[file_not_found])
    original_timeout <- getOption("timeout")
    options(timeout = 10L)
    on.exit(options(timeout = original_timeout), add = TRUE)
    for(i in seq_along(report_files)){
        reportfile <- report_files[i]
        url <- urls[i]
        attempt <- 1L
        # Handle timeouts
        while(attempt == 1L || is(dl_status, "try-error")){
            attempt <- attempt + 1L
            # Pay the server tax
            Sys.sleep(runif(1, 15, 25))
            dl_status <- try(download.file(url = url, destfile = reportfile, method = "curl"))
            if(attempt > 5L) stop("the download failures are likely not caused by timeouts!")
        }
        TAF::dos2unix(reportfile)
    }
}

# Useful for printing problematic data
# in data frames in error messages
df2str <- function(df){
    str_c(capture.output(df), collapse = "\n")
}

# last observation carried forward
locf <- function(x, missingval = ""){
    hasval <- `if`(is.na(missingval), is.na(x), x != missingval)
    c(x[hasval][1L], x[hasval])[cumsum(hasval) + 1L]
}

# Parses the HTML from the URL and stores the sale information
# in a list of data frames: one data frame per URL.
raw_extraction <- function(reportIDs, previous_reportIDs){
    str2df <- function(x) as.data.table(matrix(x, ncol = length(x)))
    reportIDs <- as.character(reportIDs)
    report_info <- Map(list,
                       reportID = reportIDs,
                       filename = sprintf("data-info/reports/wl_reportID%s.html", reportIDs),
                       url = to_url(reportIDs))

    cores <- max(parallel::detectCores() - 1L, 1L)
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    cat("", file = "data-info/logs/extraction.txt", sep = "", append = FALSE)
    salesinfo <- foreach(report = report_info,
                         .packages = c("data.table", "stringr"),
                         .export = "locf",
                         .inorder = FALSE,
                         .errorhandling = "pass", # must use "pass" because of the .final call
                         .final = function(x) setNames(x, reportIDs)) %dopar% {

        reportID <- report[["reportID"]]
        reportfile <- report[["filename"]]
        url <- report[["url"]]

        # Load, pre-process text with awk
        lajunta <- system(str_glue("gawk -f scripts/lajunta.awk -v url={url} {reportfile}"),
                          ignore.stderr = TRUE,
                          intern = TRUE)
        if(length(lajunta) == 0L){
            msg <- sprintf("The preprocessor didn't identify any data for the following url: %s\n", url)
            cat(msg, file = "data-info/logs/extraction.txt", append = TRUE)
            return(NULL)
        }

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
        return(obs)
    }

    salesinfo <- salesinfo[lengths(salesinfo) > 0L]
    processed_reportIDs <- names(salesinfo)
    new_reportIDs <- setdiff(processed_reportIDs, previous_reportIDs)
    if(length(new_reportIDs) > 0){
        # wl_reportIDs.txt should only contain information for used market reports
        cat(new_reportIDs, file = "data-info/reports/wl_reportIDs.txt", sep = "\n", append = TRUE)
    }
    salesinfo
}

# Ensures the values from the scraper came through properly.
# These checks must pass before continuing with the process.
# No news is good news.
raw_validation <- function(saleslist = NULL, reportIDs = NULL){
    reportIDs_file <- "data-info/reports/wl_reportIDs.txt"
    salescols <- c("markettext", "datetext", "saletext", "buyer", "quantity",
                   "weight", "price", "type", "reprod", "url", "reportid")
    urls <- to_url(reportIDs)

    stopifnot(!is.null(saleslist), !is.null(reportIDs), file.exists(reportIDs_file))
    reportIDs_stored <- str_sort(readLines(reportIDs_file), numeric = TRUE)
    reportIDs_asis <- as.character(reportIDs)
    reportIDs <- str_sort(reportIDs_asis, numeric = TRUE)
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
        stop(sprintf("The following reports were used to collect data but not are recorded:\n%s",
                     paste(urls[!recorded_IDs], collapse = "\n")))
    }
    if(!is.list(saleslist)){
        stop("`saleslist` is not a list!")
    }
    if(length(saleslist) != length(reportIDs_asis)){
        stop("The length of `saleslist` does not match the number of reports!")
    }
    if(!all(sort(names(saleslist)) == sort(reportIDs))){
        stop("The names of `saleslist` are not the values of `reportIDs`!")
    }

    for(reportID in reportIDs){
        salestable <- saleslist[[reportID]]
        # Ensure each element of the list is a data frame with the required fields
        if(!is.data.frame(salestable)){
            stop(sprintf("`saleslist`[[\"%s\"]] is not a data.frame!", reportID))
        }
        if(nrow(salestable) == 0){
            stop(sprintf("`saleslist`[[\"%s\"]] does not have any observations!", reportID))
        }
        dfcols <- colnames(salestable)
        cols_in_dfcols_not_in_salescols <- setdiff(dfcols, salescols)
        if(length(cols_in_dfcols_not_in_salescols) > 0){
            stop(sprintf("`saleslist`[[\"%s\"]] contains extra columns:\n%s",
                         reportID,
                         paste(cols_in_dfcols_not_in_salescols, collapse = "\n")))
        }
        cols_in_salescols_not_in_dfcols <- setdiff(salescols, dfcols)
        if(length(cols_in_salescols_not_in_dfcols) > 0){
            stop(sprintf("`saleslist`[[\"%s\"]] is missing these columns:\n%s",
                         reportID,
                         paste(cols_in_salescols_not_in_dfcols, collapse = "\n")))
        }

        # Ensure the values in each field came through properly
        check_missingness <- function(df, field){
            if(any(is.na(df[[field]]) | df[[field]] == "")){
                stop(sprintf("`saleslist`[[\"%s\"]] has NAs or an empty string in the %s field!", reportID, field))
            }
        }
        check_digitsonly <- function(df, field){
            if(any(str_detect(df[[field]], "^[^ \\d.]$"))){
                stop(sprintf("`saleslist`[[\"%s\"]] has non-digits in the %s field!", reportID, field))
            }
        }
        check_nonumbers <- function(df, field){
            if(any(str_detect(df[[field]], "\\d"))){
                stop(sprintf("`saleslist`[[\"%s\"]] has digits in the %s field!", reportID, field))
            }
        }
        for(col in dfcols) check_missingness(df = salestable, field = col)
        for(col in c("quantity", "weight", "price")) check_digitsonly(df = salestable, field = col)
        for(col in c("type", "reprod")) check_nonumbers(df = salestable, field = col)
        if(any(salestable[, str_detect(type, "\\d")])){
            stop(sprintf("`saleslist`[[\"%s\"]] has numbers in the type field!", reportID))
        }
        if(!all(salestable[["url"]] == to_url(reportID))){
            stop(sprintf("The url field of `saleslist`[[\"%s\"]] is not equal to \"%s\"!", reportID, to_url(reportID)))
        }
    }
    writeLines(reportIDs, reportIDs_file)
}

# Extract dates, imputing the year when necessary
refine_date <- function(saleslist){
    extract_date <- function(report_df){
        mnames <- str_to_lower(str_c(month.name, collapse = "|"))
        dateinfo <- report_df[1L, datetext]

        # Remove characters before the month name
        dateinfo <- str_remove(dateinfo, str_glue("^.*(?=({mnames}))"))
        # Remove characters after the first period
        dateinfo <- str_remove(dateinfo, "\\..*$")
        # Remove characters after the year
        dateinfo <- str_replace(dateinfo, "((?<=[0-9]{4})).*$", "\\1")

        # Remove date-formatting characters
        dateinfo <- str_remove_all(dateinfo, "(?<=\\d)(st|nd|rd|th)|,")
        # Remove the day of the month for Mondays on multi-day sales
        dateinfo <- str_remove(dateinfo, "\\d+\\ +(&|and)")
        # Remove extra whitespace
        dateinfo <- str_squish(dateinfo)

        # Final formatting attempts
        date_delimiter_pattern <- " \\-/"
        patterns_to_test <- c(
                              # month dd yyyy, month-dd-yyyy, etc.
                              sprintf("^(%s)[%s]\\d{1,2}([%s]\\d{4})?", mnames, date_delimiter_pattern, date_delimiter_pattern),
                              # mm-dd-yyyy, mm/dd/yyyy, mm dd yyyy, dd-mm-yyyy, etc.
                              sprintf("\\d{1,2}[%s]\\d{2}([%s]\\d{4})?", date_delimiter_pattern, date_delimiter_pattern)
        )
        report_date <- vapply(patterns_to_test, str_extract, character(1L), string = dateinfo, USE.NAMES = FALSE)
        report_date <- report_date[!is.na(report_date)]
        multiple_successful_date_formats <- any(duplicated(report_date)[-1L])
        report_date <- str_replace_all(report_date[1L], "-|/", " ")
        if(multiple_successful_date_formats){
            warning(sprintf("Multiple successful date parsings. Choosing %s as the date for report ID %s.",
                            report_date, report_df[1L, reportid]),
                    immediate. = TRUE)
        }

        report_df[, date := report_date]
        report_df[]
    }
    impute_year <- function(saleslist_){
        previous_date <- saleslist_[[1L]][1L, as.Date(date, "%B %d %Y")]
        to_date <- function(datestring){
            formats_to_try <- c("%B %d %Y", "%m %d %Y")
            d <- as.Date(datestring, formats_to_try)
            d <- d[!is.na(d)]
            d[1L]
        }
        for(report_df in saleslist_){
            current_reportID <- report_df[1L, reportid]
            current_date <- report_df[1, date]
            missing_year <- str_detect(current_date, "\\d{4}$", negate = TRUE)
            if(missing_year){
                impute_date <- previous_date + 7L
                if(wday(impute_date) != 3L){
                    stop(sprintf("The imputed date for reportID %s is not a Tuesday!", current_reportID))
                }
                current_date <- str_c(current_date, year(impute_date), sep = " ")
                current_date <- to_date(current_date)
                if(is.na(current_date)){
                    stop(sprintf("The date for reportID %s is NA!", current_reportID))
                }
                if(current_date - 7 != previous_date){
                    stop(sprintf("The date for reportID %s ", current_reportID),
                         "is not 7 days from the previous date after the ",
                         "year imputation!")
                }
            } else {
                current_date <- to_date(current_date)
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
    salesdf <- copy(salesdf)

    # Standardize reprod to only use 'bull', 'cow', 'heifer', and 'steer'
    salesdf[, reprod := str_remove_all(reprod, "[^a-z]")]
    salesdf[reprod %in% c("bull", "bulls"), reprod := "bull"]
    salesdf[reprod %in% c("cow", "cows", "dcow", "bow"), reprod := "cow"]
    salesdf[reprod %in% c("hfrs", "hfr", "heifers", "hrfs", "hrs"), reprod := "heifer"]
    salesdf[reprod %in% c("strrs", "strs", "str", "steers", "steer"), reprod := "steer"]
    # Cases where reprod was not provided
    salesdf[reportid == 7308 & reprod == "pairs", reprod := "cow"]
    salesdf[reportid == 9929 & reprod == "sim", `:=`(type = "sim", reprod = "bull")]

    # Standardize typing (type1, type2, color1, color2)
    setnames(salesdf, "type", "type1")
    salesdf[, type1 := str_remove_all(type1, "[^a-z&\\-\\ ]")]
    salesdf[, type1 := str_remove(type1, "\\ +x$")]
    salesdf[, `:=`(color1 = "none", color2 = "none", type2 = "none")]
    salesdf[type1 %in% c("ang-char",
                         "ang-gel",
                         "ang-here",
                         "ang-lim",
                         "ang-maine",
                         "ang-sal",
                         "ang-saler",
                         "ang-stab",
                         "angus-char",
                         "angus-gel",
                         "angus-here",
                         "angus-lim",
                         "char-ang",
                         "char-angus",
                         "char-lim",
                         "gel-ang",
                         "gel-angus",
                         "gel-lim",
                         "gelb-ang",
                         "here-angus",
                         "hereford-angus",
                         "lim-ang",
                         "lim-angus",
                         "lim-char",
                         "lim-flex",
                         "maine-ang",
                         "maine-angus",
                         "sal-lim",
                         "saler-ang",
                         "sim-ang",
                         "sim-angus"),
                c("type1", "type2") := tstrsplit(type1, fixed("-"))]
    for(typecol in c("type1", "type2")){
        salesdf[get(typecol) == "ang", (typecol) := "angus"]
        salesdf[get(typecol) == "bal", (typecol) := "balancer"]
        salesdf[get(typecol) == "beefmstr", (typecol) := "beefmaster"]
        salesdf[get(typecol) == "brah", (typecol) := "brahman"]
        salesdf[get(typecol) %in% c("chr", "char"), (typecol) := "charolais"]
        salesdf[get(typecol) %in% c("here", "hererord"), (typecol) := "hereford"]
        salesdf[get(typecol) %in% c("gel", "gelvieh", "gelb"), (typecol) := "gelbvieh"]
        salesdf[get(typecol) %in% c("lim", "limo"), (typecol) := "limousin"]
        salesdf[get(typecol) == "maine", (typecol) := "maine_anjou"]
        salesdf[get(typecol) == "sal", (typecol) := "saler"]
        salesdf[get(typecol) == "sim", (typecol) := "simmental"]
        salesdf[get(typecol) == "stab", (typecol) := "stabilizer"]
    }
    salesdf[type1 == "flex" & type2 == "angus", type1 := "limousin"]
    salesdf[type1 == "limousin" & type2 == "flex", type2 := "angus"]
    salesdf[, type1 := str_replace_all(type1, "blk|balck", "black")]
    salesdf[, type1 := str_replace_all(type1, fixed("mot"), "motley")]
    salesdf[, type1 := str_replace_all(type1, fixed("wf"), "whiteface")]
    salesdf[, type1 := str_replace_all(type1, fixed("bwhiteface"), "black whiteface")]
    salesdf[, type1 := str_replace_all(type1, fixed("rwhiteface"), "red whiteface")]
    salesdf[type1 == "black & black whiteface",                type1 := "black whiteface"]
    salesdf[type1 == "black whiteface & whiteface",            type1 := "black whiteface"]
    salesdf[type1 == "whiteface & black whiteface",            type1 := "black whiteface"]
    salesdf[type1 == "black whiteface strs &",                 type1 := "black whiteface"]
    salesdf[type1 == "red & red whiteface",                    type1 := "red whiteface"]
    salesdf[type1 == "red whiteface & whiteface",              type1 := "red whiteface"]
    salesdf[type1 == "whiteface & x-bred",                     type1 := "whiteface"]
    salesdf[type1 == "red whiteface & whiteface",              type1 := "red whiteface"]
    salesdf[type1 == "black black whiteface",                  type1 := "black whiteface"]
    salesdf[type1 == "black black whiteface red face",         type1 := "black whiteface & red whiteface"]
    salesdf[type1 == "black whiteface red whiteface",          type1 := "black whiteface & red whiteface"]
    salesdf[type1 %in% c("black",
                         "brown",
                         "motley",
                         "red",
                         "roan",
                         "black whiteface",
                         "red whiteface",
                         "whiteface"),                         `:=`(color1 = type1, type1 = "none")]
    salesdf[type1 == "black & red",                            `:=`(color1 = "black", color2 = "red", type1 = "none")]
    salesdf[type1 == "red & black",                            `:=`(color1 = "red", color2 = "black", type1 = "none")]
    salesdf[type1 == "red whiteface & black",                  `:=`(color1 = "red whiteface", color2 = "black", type1 = "none")]
    salesdf[type1 == "black bred",                             `:=`(color1 = "black", type1 = "none")]
    salesdf[type1 == "black & red ang",                        `:=`(color1 = "black", color2 = "red", type1 = "angus")]
    salesdf[type1 == "black whiteface & red whiteface",        `:=`(color1 = "black whiteface", color2 = "red whiteface", type1 = "none")]
    salesdf[type1 == "black black whiteface red whiteface",    `:=`(color1 = "black whiteface", color2 = "red whiteface", type1 = "none")]
    salesdf[type1 == "black red char",                         `:=`(color1 = "black", color2 = "red", type1 = "charolais")]
    salesdf[type1 == "red & black ang",                        `:=`(color1 = "red", color2 = "black", type1 = "angus")]
    salesdf[type1 == "black & red angus",                      `:=`(color1 = "black", color2 = "red", type1 = "angus")]
    salesdf[type1 == "red & black angus",                      `:=`(color1 = "red", color2 = "black", type1 = "angus")]
    salesdf[type1 == "black red & char",                       `:=`(color1 = "black", color2 = "red", type1 = "charolais")]
    salesdf[type1 == "black & red limo",                       `:=`(color1 = "black", color2 = "red", type1 = "limousin")]
    salesdf[type1 == "black & red whiteface",                  `:=`(color1 = "black", color2 = "red whiteface", type1 = "none")]
    salesdf[type1 == "red black",                              `:=`(color1 = "red", color2 = "black", type1 = "none")]
    salesdf[type1 == "red whiteface & black whiteface",        `:=`(color1 = "red whiteface", color2 = "black whiteface", type1 = "none")]
    salesdf[type1 == "whiteface-ang",                          `:=`(color1 = "whiteface", type1 = "angus")]
    salesdf[type1 == "ang-whiteface",                          `:=`(color1 = "whiteface", type1 = "angus")]
    salesdf[type1 == "red ang",                                `:=`(color1 = "red", type1 = "angus")]
    salesdf[type1 == "black sim",                              `:=`(color1 = "black", type1 = "simmental")]
    salesdf[type1 == "black lim",                              `:=`(color1 = "black", type1 = "limousin")]
    salesdf[type1 == "red ang-whiteface",                      `:=`(color1 = "red", color2 = "whiteface", type1 = "angus")]
    salesdf[type1 == "irish black",                            `:=`(color1 = "irish black", type1 = "none")]
    salesdf[type1 == "sim x char",                             `:=`(type1 = "simmental", type2 = "charolais")]
    salesdf[type1 == "red lim",                                `:=`(color1 = "red", type1 = "limousin")]
    salesdf[type1 == "x-bred",                                 type1 := "none"]
    salesdf[type1 == "ang & red ang",                          `:=`(color1 = "red", type1 = "angus")]
    salesdf[type1 == "ang x strs &",                           type1 := "angus"]
    salesdf[type1 == "red limo",                               `:=`(color1 = "red", type1 = "limousin")]
    salesdf[type1 %in% c("brwn", "bwn"),                       `:=`(color1 = "brown", type1 = "none")]
    salesdf[type1 == "black ang",                              `:=`(color1 = "black", type1 = "angus")]
    salesdf[type1 == "char x & red",                           `:=`(color1 = "red", type1 = "charolais")]
    salesdf[type1 == "red & char",                             `:=`(color1 = "red", type1 = "charolais")]
    salesdf[type1 == "white park",                             type1 := "white_park"]
    salesdf[type1 == "red angus",                              `:=`(color1 = "red", type1 = "angus")]
    salesdf[type1 == "spot",                                   `:=`(color1 = "spot", type1 = "none")]
    salesdf[type1 == "santa gert",                             type1 := "santa_gertrudis"]
    salesdf[type1 == "black & char",                           `:=`(color1 = "black", type1 = "charolais")]
    salesdf[type1 == "black gel",                              `:=`(color1 = "black", type1 = "gelbvieh")]
    salesdf[type1 == "black maine",                            `:=`(color1 = "black", type1 = "maine_anjou")]
    salesdf[type1 == "red ang-char",                           `:=`(color1 = "red", type1 = "angus", type2 = "charolais")]
    salesdf[type1 == "black limo",                             `:=`(color1 = "black", type1 = "limousin")]
    salesdf[type1 == "whiteface & red whiteface",              `:=`(color1 = "red whiteface", type1 = "none")]
    salesdf[type1 == "red gel",                                `:=`(color1 = "red", type1 = "gelbvieh")]
    salesdf[type1 == "red brangus",                            `:=`(color1 = "red", type1 = "brangus")]
    salesdf[type1 == "black strs &",                           `:=`(color1 = "black", type1 = "none")]
    salesdf[type1 == "lh",                                     type1 := "longhorn"]
    salesdf[type1 == "hol",                                    type1 := "holstein"]
    salesdf[type1 == "char x lim",                             `:=`(type1 = "charolais", type2 = "limousin")]
    salesdf[type1 == "brang",                                  type1 := "brangus"]
    salesdf[type1 == "shthrn",                                 type1 := "shorthorn"]
    salesdf[type1 == "char-black",                             `:=`(color1 = "black", type1 = "charolais")]
    salesdf[type1 == "lim-whiteface",                          `:=`(color1 = "whiteface", type1 = "limousin")]
    salesdf[type1 == "black & whiteface",                      `:=`(color1 = "black", color2 = "whiteface", type1 = "none")]
    salesdf[type1 == "red & whiteface",                        `:=`(color1 = "red", color2 = "whiteface", type1 = "none")]
    salesdf[type1 == "mix",                                    type1 := "mixed"]
    salesdf[type1 == "ang x whiteface",                        `:=`(color1 = "whiteface", type1 = "angus")]
    salesdf[type1 == "black & whiteface",                      `:=`(color1 = "black", color2 = "whiteface", type1 = "none")]
    salesdf[type1 == "red & whiteface",                        `:=`(color1 = "red", color2 = "whiteface", type1 = "none")]
    salesdf[type1 == "gray",                                   `:=`(color1 = "grey", type1 = "none")]
    salesdf[type1 == "char & red",                             `:=`(color1 = "red", type1 = "charolais")]
    salesdf[type1 == "red sim-ang",                            `:=`(color1 = "red", type1 = "simmental", type2 = "angus")]
    salesdf[type1 == "char x ang",                             `:=`(type1 = "charolais", type2 = "angus")]
    salesdf[type1 == "lim x char",                             `:=`(type1 = "limousin", type2 = "charolais")]
    salesdf[type1 == "red sim-angus",                          `:=`(color1 = "red", type1 = "simmental", type2 = "angus")]
    salesdf[type1 == "lim x sim",                              `:=`(type1 = "limousin", type2 = "simmental")]
    salesdf[type1 == "black here",                             `:=`(color1 = "black", type1 = "hereford")]
    salesdf[type1 == "char & black",                           `:=`(color1 = "black", type1 = "charolais")]
    salesdf[type1 == "angus-red angus",                        `:=`(color1 = "red", type1 = "angus")]
    salesdf[type1 == "angus-whiteface",                        `:=`(color1 = "whiteface", type1 = "angus")]
    salesdf[type1 == "char-whiteface",                         `:=`(color1 = "whiteface", type1 = "charolais")]
    salesdf[type1 == "red & whiteface",                        `:=`(color1 = "red", color2 = "whiteface", type1 = "none")]
    salesdf[type1 == "lim x bulls &",                          `:=`(type1 = "limousin")]
    salesdf[type1 == "black longhorn",                         `:=`(color1 = "black", type1 = "longhorn")]
    salesdf[type1 == "x bred",                                 type1 := "none"]
    salesdf[type1 == "whiteface-angus",                        `:=`(color1 = "whiteface", type1 = "angus")]
    salesdf[type1 == "angus & char",                           `:=`(type1 = "angus", type2 = "charolais")]
    salesdf[type1 == "red angus-whiteface",                    `:=`(color1 = "red", color2 = "whiteface", type1 = "angus")]
    salesdf[type1 == "red sim",                                `:=`(color1 = "red", type1 = "simmental")]
    salesdf[type1 == "rd angus",                               `:=`(color1 = "red", type1 = "angus")]
    salesdf[type1 == "black gelbvieh",                         `:=`(color1 = "black", type1 = "gelbvieh")]
    salesdf[type1 == "red limousin",                           `:=`(color1 = "red", type1 = "limousin")]
    salesdf[type1 == "angus strs &",                           type1 := "angus"]
    salesdf[type1 == "angus-lh",                               `:=`(type1 = "angus", type2 = "longhorn")]
    salesdf[type1 == "black angus",                            `:=`(color1 = "black", type1 = "angus")]
    salesdf[type1 == "angus & red angus",                      `:=`(color1 = "red", type1 = "angus")]
    salesdf[type1 == "char-red angus",                         `:=`(color1 = "red", type1 = "charolais", type2 = "angus")]
    salesdf[type1 == "choice angus",                           type1 := "angus"]
    salesdf[type1 == "lim flex",                               `:=`(type1 = "limousin", type2 = "angus")]
    salesdf[type1 == "motley & whiteface",                     `:=`(color1 = "motley", color2 = "whiteface", type1 = "none")]
    salesdf[type1 == "whiteface & motley",                     `:=`(color1 = "whiteface", color2 = "motley", type1 = "none")]
    salesdf[type1 == "black limousin",                         `:=`(color1 = "black", type1 = "limousin")]
    salesdf[type1 == "sim angus",                              `:=`(type1 = "simmental", type2 = "angus")]
    salesdf[type1 == "red motley",                             `:=`(color1 = "red", color2 = "motley", type1 = "none")]
    salesdf[type1 == "motley & red",                           `:=`(color1 = "red", color2 = "motley", type1 = "none")]
    salesdf[type1 == "black whiteface & black",                `:=`(color1 = "black whiteface", color2 = "black", type1 = "none")]
    salesdf[type1 == "hereford & black whiteface",             `:=`(color1 = "black whiteface", type1 = "hereford")]
    salesdf[type1 == "white face",                             `:=`(color1 = "whiteface", type1 = "none")]
    salesdf[type1 == "angus&char",                             `:=`(type1 = "angus", type2 = "charolais")]
    salesdf[type1 == "balacner",                               type1 := "balancer"]
    salesdf[type1 == "glebvieh",                               type1 := "gelbvieh"]
    salesdf[type1 == "ang x here",                             `:=`(type1 = "angus", type2 = "hereford")]
    salesdf[type1 == "char or angus",                          `:=`(type1 = "charolais", type2 = "angus")]
    salesdf[type1 == "whiteface & black",                      `:=`(color1 = "whiteface", color2 = "black", type1 = "none")]
    salesdf[type1 == "red ang & char",                         `:=`(color1 = "red", type1 = "angus", type2 = "charolais")]
    salesdf[type1 == "char-red",                               `:=`(color1 = "red", type1 = "charolais")]
    salesdf[type1 == "red or char",                            `:=`(color1 = "red", type1 = "charolais")]
    salesdf[type1 == "sim-angus-whiteface",                    `:=`(color1 = "whiteface", type1 = "simmental", type2 = "angus")]
    salesdf[type1 == "lim-flex & whiteface",                   `:=`(color1 = "whiteface", type1 = "limousin", type2 = "angus")]
    salesdf[type1 == "black or char",                          `:=`(type1 = "charolais", color1 = "black")]
    salesdf[type1 == "black or black whiteface",               `:=`(color1 = "black", type1 = "none")]
    salesdf[type1 == "black or red",                           `:=`(color1 = "black", type1 = "none")]
    salesdf[type1 == "red or black",                           `:=`(color1 = "red", type1 = "none")]
    salesdf[type1 == "red or red whiteface",                   `:=`(color1 = "red", type1 = "none")]
    salesdf[type1 %in% c("brnd", "mis", "re", "weaned"), type1 := "none"] # Not sure how to classify these
    setcolorder(salesdf, c("market", "reportid", "date", "buyer", "quantity", "type1", "type2", "color1", "color2", "reprod", "weight", "price"))

    salesdf[]
}

# Verify that the cleaning process went according to plan
clean_validation <- function(salesdf){

    # Check for NA values
    missinginds <- unique(unlist(lapply(salesdf, function(x) which(is.na(x)))))
    if(any(missinginds)){
        elements_with_missing_values <- salesdf[missinginds]
        elements_with_missing_values[, row_in_table := missinginds]
        stop(sprintf("The following tuples contain missing data:\n%s",
                     df2str(elements_with_missing_values)))
    }

    # Check for colors and types that have not been handled
    valid_colors <- readLines("data-info/valid_colors.txt")
    valid_types <- readLines("data-info/valid_types.txt")
    stop_for_invalid_values <- function(inputcol, valid_values){
        valid_values_source <- "valid_colors"
        if(inputcol %in% c("type1", "type2")) valid_values_source <- "valid_types"
        if(!all(salesdf[[inputcol]] %in% valid_values)){
            invalid <- setdiff(unique(salesdf[[inputcol]]), valid_values)
            stop(sprintf("The following values are not included in data-info/%s.txt:\n%s\n\n%s",
                         valid_values_source,
                         paste(invalid, collapse = "\n"),
                 "You may need to update the clean_*() functions or a valid inputs file."))
        }
    }
    stop_for_invalid_values("color1", valid_colors)
    stop_for_invalid_values("color2", valid_colors)
    stop_for_invalid_values("type1", valid_types)
    stop_for_invalid_values("type2", valid_types)

    # Return TRUE if all tests pass
    TRUE
}

