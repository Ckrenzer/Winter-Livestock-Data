collection <- function(urls){
  
  # Stores previously used URLs in a vector (urls in which we have already collected the data)
  # This condition allows us to prevent repeated data from being added
  if(file.exists("Collection/La Junta URLs.txt")){
    used_urls <- read_lines("Collection/La Junta URLs.txt") 
  } else {
    used_urls <- "No previously used URLs" 
  }
  
  for(URL in urls){
    # simple yet effective way of showing the operation's progress
    message(".")
    
    
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
    if(is.na(livestock_data)){
      next
    }
    
    
    # Determining location (La Junta) ---------------------------------------------------
    # If we've made it this far, that means that there is information about a market report on the webpage.
    # We now need to determine whether this market report is for La Junta, CO.
    # We assume that "La Junta CO" will not appear in non-La Junta, CO
    # market reports but WILL appear in all La Junta, CO market reports
    if(!all(str_detect(livestock_data, pattern = "la\\s*junta,*\\s*co"))){
      # If "la junta co" is not found, skip to the next iteration of the loop
      next
    }
    
    
    # Finding the date of sale ----------------------------------------------------------
    if(exists("preivous_date_of_sale")){
      date_of_sale <- determine_date_of_sale(previous_date_of_sale = previous_date_of_sale)
      # If the date has already been added, skip to the next iteration of the loop
      # this means there was a duplicate market report url
      if(previous_date_of_sale == date_of_sale){
        next
      }
    } else {
      date_of_sale <- determine_date_of_sale() 
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
    # Step 1. Names of Buyers:
    # Pulling out the indices that contain the buyer's name.
    buyers <- livestock_data[!str_detect(livestock_data, "^\t+")]
    
    # removing leading newline characters
    buyers <- str_trim(buyers)
    
    # Extracting the buyer's name--the name ends when
    # the first "\t" is encountered. You can see that
    # with this line:
    str_view(buyers, ".*?(?=\t)")
    
    # The names of the buyers:
    buyers <- str_extract(buyers, ".*?(?=\t)")
    
    #If the entries are not tab-delimited, that means they used spaces instead.
    if(all(is.na(buyers))){
      
      # repeating step 1
      # Pulling out the indices that contain the buyer's name (doesn't start with a number, but contains a number; also does not contain a semicolon)
      buyers <- livestock_data[!str_detect(livestock_data, "^\\d") &
                                 str_detect(livestock_data, "\\d") &
                                 !str_detect(livestock_data, ";")]
      
      # removing leading newline characters
      buyers <- str_trim(buyers)
      
      
      
      #extracting the buyer names and the first number
      buyers <- str_extract_all(buyers, "^[a-z].*?\\s\\d", simplify = TRUE)
      buyers <- as.vector(buyers)
      #removing the first number found
      buyers <- str_remove_all(buyers, "\\d")
    }
    
    
    
    
    # removing unneeded white space and missing values
    buyers <- str_trim(buyers)
    buyers <- buyers[!is.na(buyers)]
    
    # We should now have a character vector containing each buyer's name
    
    
    
    
    
    
    # Adding Buyer Names Back In --------------------------------------------------------
    # This section uses ID numbers to identify the buyer for
    # each sale listed on the market report. It then adds
    # the buyer's name back onto the lines from which it
    # was omitted.
    
    # I will provide each buyer an ID number to determine
    # which purchases that buyer made. I am not crazy about
    # using loops, as I could probably write a function
    # to do this task for me, but it does everything we
    # need it to.
    current_ID <- 0
    for(i in 1:length(livestock_data)){
      
      if(str_detect(livestock_data[i], "^\n?\t+\\s*\\d|^\\d")){#if a multiple purchase:
        # Only multiple purchases will have the "\n\t\t" or start with a digit, so we can
        # use "\n?\t+|^//d" as a placeholder for the beginning of the string
        # (where the buyer's name goes) and make it look the same as the
        # lines where the name is present
        livestock_data[i] <- paste0(buyers[current_ID], "\t", livestock_data[i])
        livestock_data[i] <- str_remove(livestock_data[i], "\n")
        
      } else if(!str_detect(livestock_data[i], "^\n?\t+\\s*\\d|^\\d")){
        
        # if the index lands on a new buyer, give them
        # the next ID number
        current_ID <- current_ID + 1 
      }
      
    }# end of for loop
    
    #This is where we will add the tabs:
    str_view_all(livestock_data, "\\d\\s|[a-z]\\s+\\d")
    
    
    #inserting tab characters between the fields--I was careful to not put a tab between the quantity and type
    livestock_data <- str_replace_all(livestock_data, "(\\d)\\s(\\d)", "\\1\t\\2") 
    livestock_data <- str_replace_all(livestock_data, "([a-z])\\s+(\\d)", "\\1\t\\2")
    
    
    #remove any extra "\t" characters after the buyer name
    livestock_data <- str_replace_all(livestock_data, "\t+\\s*", "\t")
    
    #removing any missing values
    livestock_data <- livestock_data[!is.na(livestock_data)]
    
    
    #livestock_data should now have the fields separated by tabs
    
    #You can confirm with this line:
    str_view_all(livestock_data, "\t")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # Data frame ------------------------------------------------------------------------
    # Making a data frame
    livestock_data <- tibble(livestock_data)
    
    
    # making new columns based off the sections
    # separated by "\t"
    livestock_data <- livestock_data %>% 
      separate(livestock_data, into = c("buyer", "quantity", "weight", "price"), sep = "\t")
    
    # Replaces the space separating the quantity and the type with a semicolon
    livestock_data$quantity <- str_replace(livestock_data$quantity, " ", ";")
    
    # Making quantity and type their own columns
    livestock_data <- livestock_data %>% 
      separate(quantity, into = c("quantity", "type"), sep = ";")
    
    
    # Making quantity, weight, and price numeric datatypes
    livestock_data[c(2, 4, 5)] <- sapply(livestock_data[c(2, 4, 5)], as.numeric)
    
    # Adding the date from the market report as a column
    
    
    
    # We are just about there! All that remains is removing the section
    # with NA's introduced:
    as.data.frame(livestock_data)
    
    # How do we fix that? Well, since we may want that information
    # for future use, we won't want to remove it entirely with a
    # keyword search, as was done in the beginning. What we can
    # remove observations with NA values to keep only the data
    # that matches our desired format:
    livestock_data <- na.omit(livestock_data)
    
    
    
    
    
    
    
    
    
    
    # Finishing Touches -----------------------------------------------------------------
    
    # We have all the data we need, though we still want a date column
    
    # Adding in the date and URL
    livestock_data <- mutate(livestock_data, "date" = date_of_sale, .before = 1) %>% 
      mutate("url" = URL)
    
    
    
    
    
    
    
    
    
    
    
    # Writing to CSV --------------------------------------------------------------------
    # We are now ready to write the data to a file!
    
    # If the file is already on your computer, we want to append the new data to the uncleaned csv file.
    if(file.exists("La Junta Market Reports (before cleaning).csv")){
      write_csv(x = livestock_data,
                file = "La Junta Market Reports (before cleaning).csv",
                append = T,
                col_names = F)
      
    } else {
      # If the file is not in your computer, append the data to the new file.
      write_csv(x = livestock_data,
                file = "La Junta Market Reports.csv",
                append = T,
                col_names = F)
      
    }#end of conditional
    
    
    
    # Confirmation message saying data was added to the file
    message(paste0("\nDATA ADDED: ", date_of_sale, "\tURL: ", URL, "\n"))
    
    
    
  } # end of for loop
  
  
}#end of collection()