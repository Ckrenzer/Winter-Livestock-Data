# Instructions -------------------------------------------------
# 1. Add the current market report url to the end of the "url" variable
# 2. Add the market report's date to the end of the "DATE" variable
# 3. click ctrl+a, then click ctrl+enter
# 4. If needed, run the other script
# 5. (optional) Run the cleaning script, "La Junta cleaning.R"

# Note: If you do not use the cleaning script, the dates are not 
# necessarily going to be listed in chronological order. To
# put them back into chronological order,  you can read the csv
# file into R (storing the data in a variable) and then use
# dplyr's arrange() function.
#
# The code would look something like this:
# livestock <- readr::read_csv(file = "La Junta Market Reports.csv",
#                               col_names = TRUE) %>% 
#                     arrange(Date)


# There are ways of getting the next week's data with fewer 'moving
# parts,' but few ways of doing so are simpler than this.


# Removing CSV -------------------------------------------------
if(file.exists("La Junta Market Reports.csv")){
  file.remove("La Junta Market Reports.csv")
}

# Packages -----------------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(rvest, stringr, tidyr, readr, dplyr)

# Data import --------------------------------------------------
# Saving url into a String vector

# Note: you can create a schedule to perform this task
url <- c("http://www.winterlivestock.com/lajunta.php?reportID=12669#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12688#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12703#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12732#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12752#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12774#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12783#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12806#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12840#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12865#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12877#marketreport",
         "http://www.winterlivestock.com/lajunta.php?reportID=12896#marketreport")

DATE <- c("10-20-2020",
          "10-27-2020",
          "11-03-2020",
          "11-10-2020",
          "11-17-2020",
          "11-24-2020",
          "12-01-2020",
          "12-08-2020",
          "12-15-2020",
          "12-22-2020",
          "12-29-2020",
          "1-05-2021")



# The url for the most recent report
# (which is useful for scheduling a task
# on your computer instead of writing in
# all of the urls into a vector):

###url <- "http://www.winterlivestock.com/lajunta.php#marketreport"


for(k in 1:length(url)){
  # Saving the webpage into a variable
  webpage <- read_html(url[k])
  
  # Saving data from the webpage written in html
  # The nodes are consistent across market reports
  livestock_data_html <- html_nodes(webpage, "div:nth-child(9) div.sml")
  
  # Converting to plain text
  livestock_data <- html_text(livestock_data_html)
  
  
  
  # Data cleaning -----------------------------------------------
  # Let's split the String up into an array of Strings using a delimiter
  # ("\r" in this case)
  
  # We have all the lines separated into a list.
  # Now, we just grab the lines we want and manipulate the new vector to extract the numbers
  livestock_data <- strsplit(livestock_data, "\r")
  
  # Let's store everything in a character vector
  livestock_data <- as.vector(livestock_data[[1]])
  
  # making all the text lowercase
  livestock_data <- str_to_lower(livestock_data)
  
  
  # Header Removal with Keywords ------------------------------------------------------
  # A set of keywords designed to remove heading information
  # and other information we are not interested in observing
  keywords <- "\\s+sold|\\s+sale|\\s+monday|\\s+tuesday|\\s+wednesday|\\s+thursday|\\s+friday|\\s+saturday|\\s+sunday|\\s+receipts|\\s+through|\\s+mostly|\\s+winter|\\s+summer|\\s+spring|\\s+fall|\\s+autumn|\\s+is\\s+|\\s+next|\\s+quality|\\s+mostly|\\s+noon|\\s+early|\\s+stock|\\s+steady|\\s+test\\s+|\\s+offer|\\s+selection|\\s+week|\\s+annual|\\s+package|consigned|\\s*now\\s+|special\\s+|\\s+higher|calves\\s&\\syearlings\\s*$|\\s+am\\s+|\\s+pm\\s+|\\s+a.m.\\s+|\\s+p.m.\\s+|report[:]?\\s+|la\\s+junta,|\\s+co$|\\*$"
  
  
  # You can "check your work" for the heading removal
  # with the str_view() or str_view_all() functions:
  str_view_all(livestock_data, keywords)
  
  
  # Removes headings and unrelated information from the data
  livestock_data <- livestock_data[!str_detect(livestock_data, keywords)]
  
  
  
  # Removing Unwanted Sections -------------------------------------------------------
  
  # We can pull out the sales information by removing lines we do not
  # care about. Since we know that the information we want is stored
  # in lines that are much shorter than the others, we can pull out
  # lines that have fewer characters than some optimal number.
  # I chose 60. In other words, I am keeping only those lines
  # (which are stored as elements in the vector) that contain
  # fewer than 60 characters.
  livestock_data <- livestock_data[-c(which((nchar(livestock_data) > 60)))]
  
  
  # The livestock data starts each day with a person's name and then has the quantity, type, weight, and price
  # if the person made more than one purchase, the line starts with "\n\t\t"--this is the reason
  # why we cannot make use of str_trim(). We need the "\n\t\t" to indicate whether
  # that element in the vector is really another purchase by the same buyer.
  
  # You can see what I am talking about by running
  # the code below. The "\n\t\t" is hidden, but
  # you can see the highlighted boxes at the
  # beginning of the lines containing the pattern:
  str_view(livestock_data, "\n\t\t")
  
  
  
  
  # Buyer names -----------------------------------------------------
  # Step 1. Names of Buyers:
  # Pulling out the indices that contain the buyer's name.
  buyers <- livestock_data[!str_detect(livestock_data, "\n\t\t")]
  
  # removing leading newline characters
  buyers <- str_trim(buyers)
  
  # Extracting the buyer's name--the name ends when
  # the first "\t" is encountered. You can see that
  # with this line:
  str_view(buyers, ".*?(?=\t)")
  
  # The names of the buyers:
  buyers <- str_extract(buyers, ".*?(?=\t)")
  
  # removing unneeded white space
  buyers <- str_trim(buyers)
  
  
  
  # Adding Buyer Names Back In ----------------------------------------------
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
    if(str_detect(livestock_data[i], "\n\t\t")){
      # Only multiple purchases will have the "\n\t\t" so we can
      # use "\n\t\t" as a placeholder for the beginning of the string,
      # where the buyer's name goes, and make it look the same as the
      # lines where the name is present
      livestock_data[i] <- str_replace(livestock_data[i], "\n\t\t",  paste(buyers[current_ID], "\t", sep = ""))
      
    } else if(!str_detect(livestock_data[i], "\n\t\t")){
      # if the index lands on a new buyer, give them
      # the next ID number
      current_ID <- current_ID + 1 
    }
    
  }# end of for loop
  
  # removing white leading and trailing white space
  livestock_data <- str_trim(livestock_data)
  
  
  
  
  
  # Data frame ---------------------------------------------------
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
  
  # Finishing Touches --------------------------------------------
  
  # We have all the data we need, though there are still a couple
  # problems with our output.
  #   1. We want a date column
  #   2. We want to remove the "s" at the end of the 
  #      type column to account for cases when there is only one
  #      cow bought by the buyer (Ex. "black cow" and "black cows"
  #      should both read "black cow")
  
  
  livestock_data <- mutate(livestock_data, "date" = DATE[k], .before = 1)
  
  
  # Removing the plural of the type--this is especially helpful for
  # use with group_by()
  livestock_data$type <- str_replace(livestock_data$type, "s$", "")
  
  
  
  # Writing to CSV ---------------------------------------
  # We are now ready to write the data to a file!
  # Just remember to add in the column names if
  # this file is not already on your computer
  write_csv(x = livestock_data,
            file = "La Junta Market Reports.csv",
            append = T,
            col_names = F)
  
} # end of for loop
