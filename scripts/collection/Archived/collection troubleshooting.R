# General Troubleshooting
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7289#marketreport" #BAD
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7507#marketreport" #BAD

URL <- "http://www.winterlivestock.com/lajunta.php?reportID=13373#marketreport"#GOOD

URL <- "http://www.winterlivestock.com/lajunta.php?reportID=6738#marketreport" #GOOD

URL <- "http://www.winterlivestock.com/lajunta.php?reportID=10679#marketreport" #BAD



URL <- "http://www.winterlivestock.com/lajunta.php?reportID=9951#marketreport"

URL <- "http://www.winterlivestock.com/lajunta.php?reportID=11348#marketreport"



# Troubleshooting Dates
#Multi-day sale
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7780#marketreport"
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=8935#marketreport"

#Thought the 'jun' in 'la junta' meant 'June' due to a lack of a word boundary
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=8840#marketreport"

#Multi-day sale and Year not provided
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=11611#marketreport"

#Normal sale
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7166#marketreport"
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=6798#marketreport"

URL <- "http://www.winterlivestock.com/lajunta.php?reportID=9170#marketreport"
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=9221#marketreport" #gets skipped in the loop for some reason

URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7969#marketreport"
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7987#marketreport" #this url has something to do with it
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7998#marketreport" #gets skipped in the loop for some reason
URL <- paste0("http://www.winterlivestock.com/lajunta.php?reportID=", 7960:8000, "#marketreport") #contains the two above


#Not a sale, but is la junta
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7371#marketreport"
URL <- "http://www.winterlivestock.com/lajunta.php?reportID=7384#marketreport"



previous_date_of_sale <- "10-14-2018"



# Reading in data from webpage ------------------------------
# Saving the webpage into a variable
webpage <- read_html(URL)

# Saving data from the webpage written in html
# The nodes are consistent across market reports
livestock_data_html <- html_nodes(webpage, "div:nth-child(9) div.sml")

# Converting to plain text
livestock_data <- html_text(livestock_data_html)








# Determining appropriate delimiter -------------------------
# The first delimiter we will try
delimiter <- "\n"

# Asks whether the list has more than 30 entries after the strsplit(). If there is only one entry, then that means the format uses "\r" as its delimiter or does not contain information we care about.
if(length(strsplit(livestock_data, delimiter)[[1]]) < 30){
  
  # If the first delimiter does not get us what we are looking for, we will try the second delimiter instead.
  delimiter <- "\r" 
  
  # Now we test the second delimiter...
  if(length(strsplit(livestock_data, delimiter)[[1]]) < 30){
    
    # If the second delimiter fails to deliver, skip to the next iteration of the loop (this webpage does not contain information we care about).
    next 
    
  }#end of nested if()
}#end of if()









# Determining location (La Junta) -------------------------
# If we've made it this far, that means that there is information about a market report on the webpage.
# We now need to determine whether this market report is for La Junta, CO.
# This shouldn't be too hard. We can assume that "La Junta" will not appear in non-La Junta, CO market reports. Conversely, if we find "la junta" anywhere in the text, then we found a La Junta market report.


# making all the text lowercase (to avoid the "la junta" != "La Junta" problem)
livestock_data <- str_to_lower(livestock_data)

# Now we can search for "la junta" in the text
if(!str_detect(livestock_data, pattern = "la\\s*junta,*\\s*co")){
  
  # If "la junta" is not found in the data, that means that this is not a La Junta market report--skip to the next iteration of the loop.
  next
}










# Finding the date of sale ----------------------------------------
# Let's store the results from the strsplit() into a variable to more easily extract the date
livestock_data <- strsplit(livestock_data, delimiter)


# Saves the first entry that finds digits followed by the word "cattle"--this entry contains the sentence from which we can pull the date
date_of_sale <- livestock_data[[1]][sapply(livestock_data, str_detect, "\\d+\\s*cattle") %>% 
                                      which() %>% 
                                      min()]
# removing punctuation from the sentence
date_of_sale <- str_remove_all(date_of_sale, "\\.|,|;")


# extracting the month the sale took place from the sentence
month_of_sale <- str_extract(date_of_sale, "january|february|march|april|may|june|july|august|september|october|november|december|jan\\s|feb\\s|mar\\s|apr\\s|jun\\s|jul\\s|aug\\s|sept\\s|oct\\s|nov\\s|dec\\s")



##### CASE 1: The date is in "month ##th YYYY" format (Ex. "march 9th 2021")
#note: some sales take place over two days, so we have to remove the "& ##th" following the first day's date
date_of_sale <- str_extract(date_of_sale, paste0(month_of_sale, "\\s+\\d{1,2}[a-z0-9&\\s]{0,15}\\d{4}")) %>% 
  #replacing the month name with its corresponding number
  str_replace_all(month_of_sale,
                  case_when(month_of_sale == "january" ~ "1",
                            month_of_sale == "february" ~ "2",
                            month_of_sale == "march" ~ "3",
                            month_of_sale == "april" ~ "4",
                            month_of_sale == "may" ~ "5",
                            month_of_sale == "june" ~ "6",
                            month_of_sale == "july" ~ "7",
                            month_of_sale == "august" ~ "8",
                            month_of_sale == "september" ~ "9",
                            month_of_sale == "october" ~ "10",
                            month_of_sale == "november" ~ "11",
                            month_of_sale == "december" ~ "12",
                            month_of_sale == "jan" ~ "1",
                            month_of_sale == "feb" ~ "2",
                            month_of_sale == "mar" ~ "3",
                            month_of_sale == "apr" ~ "4", #you don't need to repeat MAY, remember
                            month_of_sale == "jun" ~ "6",
                            month_of_sale == "jul" ~ "7",
                            month_of_sale == "aug" ~ "8",
                            month_of_sale == "sept" ~ "9",
                            month_of_sale == "oct" ~ "10",
                            month_of_sale == "nov" ~ "11",
                            month_of_sale == "dec" ~ "12",
                  )) %>% 
  #removing letters and replacing space characters with hyphens
  str_remove_all("[a-z]") %>%
  str_replace_all("\\s+", " ") %>% #replacing multiple spaces with just one space
  str_replace_all("\\s", "-")



##### CASE 2: The date is multi-valued but the year is included

# NOTE: the lubridate::mdy() call is meant to ensure we get NA if the formatting did not work as expected.

if(is.na(lubridate::mdy(date_of_sale))){
  #Repeating initial steps:
  # Saves the first entry that finds digits followed by the word "cattle"--this entry
  # contains the sentence from which we can pull the date
  date_of_sale <- livestock_data[[1]][sapply(livestock_data, str_detect, "\\d+\\s*cattle") %>% 
                                        which() %>% 
                                        min()]
  # removing punctuation from the sentence
  date_of_sale <- str_remove_all(date_of_sale, "\\.|,|;")
  
  
  
  # Pulling out the part of the sentence containing the date
  date_of_sale <- str_extract(date_of_sale, paste0(month_of_sale, "\\s+\\d{1,2}.{0,40}\\d{4}"))
  
  #Pulling out the month and day
  md <- str_extract(date_of_sale, paste0(month_of_sale, "\\s+\\d{1,2}"))
  #Pulling out the year
  y <- str_extract(date_of_sale, "\\d{4}")
  
  # Changing the month to its corresponding number
  md <- str_replace_all(string = md,
                        pattern = month_of_sale,
                        replacement = case_when(month_of_sale == "january" ~ "1",
                                                month_of_sale == "february" ~ "2",
                                                month_of_sale == "march" ~ "3",
                                                month_of_sale == "april" ~ "4",
                                                month_of_sale == "may" ~ "5",
                                                month_of_sale == "june" ~ "6",
                                                month_of_sale == "july" ~ "7",
                                                month_of_sale == "august" ~ "8",
                                                month_of_sale == "september" ~ "9",
                                                month_of_sale == "october" ~ "10",
                                                month_of_sale == "november" ~ "11",
                                                month_of_sale == "december" ~ "12",
                                                month_of_sale == "jan" ~ "1",
                                                month_of_sale == "feb" ~ "2",
                                                month_of_sale == "mar" ~ "3",
                                                month_of_sale == "apr" ~ "4", #you don't need to repeat MAY, remember
                                                month_of_sale == "jun" ~ "6",
                                                month_of_sale == "jul" ~ "7",
                                                month_of_sale == "aug" ~ "8",
                                                month_of_sale == "sept" ~ "9",
                                                month_of_sale == "oct" ~ "10",
                                                month_of_sale == "nov" ~ "11",
                                                month_of_sale == "dec" ~ "12",
                        )) %>% 
    #replacing the date with a hyphen
    str_replace_all("\\s+", "-")
  
  #we only want to do this step if date_of_sale is not null (we will get a string saying "NA-NA" otherwise,
  #which is a pain to correct for in subsequent case-checking)
  if(!is.na(date_of_sale)){
    # Concatenating the md and y together (into mdy format!!!)
    date_of_sale <- paste(md, y, sep = "-")
  }
  
}


##### CASE 3: The year was not provided
if(is.na(lubridate::mdy(date_of_sale))){
  #Repeating initial steps:
  # Saves the first entry that finds digits followed by the word "cattle"--this entry
  # contains the sentence from which we can pull the date
  date_of_sale <- livestock_data[[1]][sapply(livestock_data, str_detect, "\\d+\\s*cattle") %>% 
                                        which() %>% 
                                        min()]
  # removing punctuation from the sentence
  date_of_sale <- str_remove_all(date_of_sale, "\\.|,|;")
  
  
  
  
  
  
  #Pulling out the month and day
  md <- str_extract(date_of_sale, paste0(month_of_sale, "\\s+\\d{1,2}"))
  #Setting the year equal to the previous sale's year
  y <- clock::get_year(clock::date_parse(previous_date_of_sale, format = "%m-%d-%y"))  
  
  
  # Changing the month to its corresponding number
  md <- str_replace_all(string = md,
                        pattern = month_of_sale,
                        replacement = case_when(month_of_sale == "january" ~ "1",
                                                month_of_sale == "february" ~ "2",
                                                month_of_sale == "march" ~ "3",
                                                month_of_sale == "april" ~ "4",
                                                month_of_sale == "may" ~ "5",
                                                month_of_sale == "june" ~ "6",
                                                month_of_sale == "july" ~ "7",
                                                month_of_sale == "august" ~ "8",
                                                month_of_sale == "september" ~ "9",
                                                month_of_sale == "october" ~ "10",
                                                month_of_sale == "november" ~ "11",
                                                month_of_sale == "december" ~ "12",
                                                month_of_sale == "jan" ~ "1",
                                                month_of_sale == "feb" ~ "2",
                                                month_of_sale == "mar" ~ "3",
                                                month_of_sale == "apr" ~ "4", #you don't need to repeat MAY, remember
                                                month_of_sale == "jun" ~ "6",
                                                month_of_sale == "jul" ~ "7",
                                                month_of_sale == "aug" ~ "8",
                                                month_of_sale == "sept" ~ "9",
                                                month_of_sale == "oct" ~ "10",
                                                month_of_sale == "nov" ~ "11",
                                                month_of_sale == "dec" ~ "12",
                        )) %>% 
    #replacing the date with a hyphen
    str_replace_all("\\s+", "-")
  
  #we only want to do this step if date_of_sale is not null (we will get a string saying "NA-NA" otherwise,
  #which is a pain to correct for in subsequent case-checking)
  if(!is.na(date_of_sale)){
    # Concatenating the md and y together (into mdy format!!!)
    date_of_sale <- paste(md, y, sep = "-")
  }
  
}



##### CASE 4: The Catch-All
#If worse comes to worst, we can just add one week from the previous sale's date
if(is.na(lubridate::mdy(date_of_sale))){
  
  date_of_sale <- lubridate::mdy(previous_date_of_sale) %>% 
    clock::add_weeks(n = 1) %>% 
    as.character()
  
}



# If the date has already been added, skip to the next iteration of the loop
# (this means there was either A: a repeat of market reports
# or B: two different reports in one day--I am unfamiliar with any instances of two market
# reports given on the same day, though it could happen)
if(exists("previous_date_of_sale")){
  if(previous_date_of_sale == date_of_sale){
    next
  }
}









# Now that we have the dates settled, let's store
# livestock_data back into a character vector
livestock_data <- as.vector(livestock_data[[1]])



# Header Removal with Keywords ------------------------------------------------------
# A set of keywords designed to remove heading information
# and other information we are not interested in observing
keywords <- "\\s+sold|\\s+monday|\\s+tuesday|\\s+wednesday|\\s+thursday|\\s+friday|\\s+saturday|\\s+sunday|\\s+receipts|\\s+through|\\s+mostly|\\s+winter|\\s+summer|\\s+spring|\\s+fall|\\s+autumn|\\s+is\\s+|\\s+next|\\s+quality|\\s+mostly|\\s+noon|\\s+early|\\s+stock|\\s+steady|\\s+test\\s+|\\s+offer|\\s+selection|\\s+week|\\s+annual|\\s+package|consigned|\\s*now\\s+|special\\s+|\\s+higher|calves\\s&\\syearlings\\s*$|\\s+am\\s+|\\s+pm\\s+|\\s+a.m.\\s+|\\s+p.m.\\s+|report[:]?\\s+|la\\s+junta,|\\s+co$|\\*$|estimate|internet\\svideo"                                           


# Removes headings and unrelated information from the data
livestock_data <- livestock_data[!str_detect(livestock_data, keywords)]

#Removes all remaining entries with months and/or dates
livestock_data <- livestock_data[!str_detect(livestock_data, "january\\s+\\d|february\\s+\\d|march\\s+\\d|april\\s+\\d|may\\s+\\d|june\\s+\\d|july\\s+\\d|august\\s+\\d|september\\s+\\d|october\\s+\\d|november\\s+\\d|december\\s+\\d|jan\\s+\\d|feb\\s+\\d|mar\\s+\\d|apr\\s+\\d|jun\\s+\\d|jul\\s+\\d|aug\\s+\\d|sept\\s+\\d|oct\\s+\\d|nov\\s+\\d|dec\\s+\\d")]





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
str_view(livestock_data, "\t\t")


# the sales are the entries that do not have semicolons but do have digits
livestock_data <- livestock_data[!str_detect(livestock_data, ";") & str_detect(livestock_data, "\\d")]

# Keeping only those entries with more than 12 characters (for those entries that slip through the other filters)
livestock_data <- livestock_data[which(nchar(livestock_data) > 12)]

#Removing internet auction sales--they will be empty character vectors at this point in the code
#For more flexibility, if the length is smaller than 10, the data will not be added
#Very few (probably zero) market reports have fewer than 10 entries
if(length(livestock_data) < 10){
  next #go to the next iteration of the loop
}

# livestock_data now contains only the sales data





##### SETTING THE `previous_date_of_sale` MUST TAKE PLACE AFTER WE ARE SURE WE ARE ADDING
##### THE DATA TO THE DATA FRAME
#Storing the previous date of sale to extract the year later on
#(for those cases where the year is missing, we can append the previous market report's
#year to the end of the current one)...I anticipate this strategy to be problematic when
#two consecutive dates are missing and also around the new year. It is a 'good enough'
#approximation, however (plus, these situations are pretty rare)
previous_date_of_sale <- as.character(date_of_sale)










# Buyer names -----------------------------------------------------
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
livestock_data_copy <- livestock_data
livestock_data <- livestock_data_copy
i <- 1
i <- i+1

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

# We have all the data we need, though we still want a date column

# Adding in the date and URL
livestock_data <- mutate(livestock_data, "date" = date_of_sale, .before = 1) %>% 
  mutate("url" = URL)
