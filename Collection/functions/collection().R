collection <- function(urls, prevent_use_of_previous_urls = TRUE){
  # Sourcing in helper functions
  source("Collection/functions/extract_webpage_text().R", local = TRUE)
  source("Collection/functions/split_text().R", local = TRUE)
  source("Collection/functions/determine_date_of_sale().R", local = TRUE)
  source("Collection/functions/month_name_to_num().R", local = TRUE)
  source("Collection/functions/remove_unwanted_sections().R", local = TRUE)
  source("Collection/functions/extract_buyer_name().R", local = TRUE)
  source("Collection/functions/insert_buyer_names().R", local = TRUE)
  source("Collection/functions/insert_delimiter().R", local = TRUE)
  source("Collection/functions/cleaning().R", local = TRUE)
  
  
  # Stores previously used URLs in a vector (urls in which we have already collected the data)
  # This condition allows us to prevent repeated data from being added
  if(file.exists("Collection/La Junta URLs.txt") && prevent_use_of_previous_urls){
    used_urls <- read_lines("Collection/La Junta URLs.txt") 
  } else {
    used_urls <- "No previously used URLs" 
  }
  # The first sale date
  previous_date_of_sale <- "01-01-2016"
  
  
  for(URL in urls){
    # simple yet effective way of showing the operation's progress
    message(".")
    
    
    # Pause the operation to lower stress on the server
    Sys.sleep(time = runif(n = 1, min = 10, max = 15))
    
    
    # Checking for previously used URLs -------------------------------------------------
    # If the url has been used before,
    # skip to the next iteration of the loop
    if(URL %in% used_urls){
      next
    }
    
    
    # Reading in data from webpage ------------------------------------------------------
    livestock_data <- extract_webpage_text()
    livestock_data <- split_text()
    # If the return value from split_text() was missing, skip to the
    # next iteration of the loop--the current webpage does not
    # have information we care about
    if(all(is.na(livestock_data))){
      next
    }
    
    
    # Determining location (La Junta) ---------------------------------------------------
    # If we've made it this far, that means that there is information about a market report on the webpage.
    # We now need to determine whether this market report is for La Junta, CO.
    # We assume that "La Junta CO" will not appear in non-La Junta, CO
    # market reports but WILL appear in all La Junta, CO market reports
    if(!any(str_detect(livestock_data, pattern = "la\\s*junta,*\\s*co"))){
      # If "la junta co" is not found, skip to the next iteration of the loop
      next
    }
    
    
    # Finding the date of sale ----------------------------------------------------------
    date_of_sale <- determine_date_of_sale(previous_date = previous_date_of_sale)
    # If the date has already been added, skip to the next iteration of the loop
    # this means there was a duplicate market report url
    if(previous_date_of_sale == date_of_sale){
      next
    }
    
    
    # Removing Unwanted Sections -------------------------------------------------------
    livestock_data <- remove_unwanted_sections()
    # Removing internet auction sales--they should be empty character vectors by this point.
    # For more flexibility, if the length is smaller than 10, the data will not be added
    # Very few (probably zero) market reports have fewer than 10 entries
    if(length(livestock_data) < 10){
      next
    }
    # `livestock_data` now contains only the sales data
    
    
    # previous_date_of_sale -------------------------------------------------------------
    # `previous_date_of_sale` CAN ONLY BE ASSIGNED AFTER WE ARE SURE WE ARE ADDING
    # THE DATA TO THE DATA FRAME (only relevant when adding multiple sales to the csv)
    #
    # Storing the previous date of sale to extract the year later on
    # (for those cases where the year is missing, we can append the previous market report's
    # year to the end of the current one)...I anticipate this strategy to be problematic when
    # two consecutive dates are missing and also around the new year. It is a 'good enough'
    # approximation, however (plus, these situations are pretty rare)
    previous_date_of_sale <- date_of_sale
    
    
    # Buyer names -----------------------------------------------------------------------
    # A character vector containing each buyer's name
    buyers <- extract_buyer_name()
    # Adds the associated buyer name to each sale
    livestock_data <- insert_buyer_names()
    
    
    # Insert Delimiter ------------------------------------------------------------------
    # `livestock_data` should now have the fields separated by tabs
    livestock_data <- insert_delimiter()
    
    
    # Data frame ------------------------------------------------------------------------
    livestock_data <- tibble(entries = livestock_data)
    # Making new columns based off the sections
    # separated by "\t"
    livestock_data <- livestock_data %>% 
      separate(entries, into = c("Buyer", "Quantity", "Type", "Weight", "Price"), sep = "\t") %>%  
      mutate(Quantity = parse_number(Quantity),
             Weight = parse_number(Weight),
             Price = parse_number(Price)) %>% 
      na.omit() %>% 
      mutate("Date" = date_of_sale, .before = 1) %>% 
      mutate("URL" = URL)
    
    
    # Cleaning --------------------------------------------------------------------------
    # Creates the 'Reprod' column and simplifies the 'Type' column down to 8 categories
    livestock_data <- cleaning(lajunta = livestock_data)
    
    
    # Writing to CSV --------------------------------------------------------------------
    write_csv(x = livestock_data,
              file = "La Junta Market Reports.csv",
              append = TRUE,
              col_names = FALSE)
    
    
    # Confirmation message saying data was added to the file
    message(paste0("\nDATA ADDED: ", date_of_sale, "\tURL: ", URL, "\n"))
  }#end of for loop
}