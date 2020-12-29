if(!require(pacman)) install.packages("pacman")
pacman::p_load(rvest, stringr)

# Data import --------------------------------------------------
# Saving url into a String variable
url <- "http://www.winterlivestock.com/lajunta.php"
url <- "http://www.winterlivestock.com/lajunta.php?reportID=12783#marketreport"
# Saving the webpage into a variable
webpage <- read_html(url)

# Saving data from the webpage written in html
# The nodes are consistent across market reports
livestock_data_html <- html_nodes(webpage, "div:nth-child(9) div.sml")

# Converting to plain text
livestock_data <- html_text(livestock_data_html)



# Data cleaning -----------------------------------------------
# Let's split the String up into an array of Strings using a delimiter
# ("\r" in this case--I think "\n" would have worked just fine)

# We have all the lines separated into a list.
# Now, we just grab the lines we want and manipulate the new vector to extract the numbers
livestock_data <- strsplit(livestock_data, "\r")

# Let's store everything in a character vector
livestock_data <- as.vector(livestock_data[[1]])

# making all the text lowercase
livestock_data <- str_to_lower(livestock_data)


# Keyword ideas ------------------------------------------------------
keywords <- "\\s+sold|\\s+sale|\\s+monday|\\s+tuesday|\\s+wednesday|\\s+thursday|\\s+friday|\\s+saturday|\\s+sunday|\\s+receipts|\\s+through|\\s+mostly|\\s+winter|\\s+summer|\\s+spring|\\s+fall|\\s+autumn|\\s+is\\s+|\\s+next|\\s+quality|\\s+mostly|\\s+noon|\\s+early|\\s+stock|\\s+steady|\\s+test\\s+|\\s+offer|\\s+selection|\\s+week|\\s+package|consigned|\\s*now\\s+|special\\s+|\\s+higher|calves\\s&\\syearlings\\s*$|\\s+am\\s+|\\s+pm\\s+|report[:]?\\s+"


# You can "check your work" to the heading removal
# with the str_view() or str_view_all() functions:
str_view_all(livestock_data, keywords)


# Removes headings and unrelated information from the data
livestock_data <- livestock_data[!str_detect(livestock_data, keywords)]



# Picking Data -------------------------------------------------------

# We can pull out the sales information by removing lines we do not
# care about. Since we know that the information we want is stored
# in lines that are much shorter than the others, we can pull out
# lines that have fewer characters than some optimal number.
# I chose 60. In other words, I am keeping only those lines
# (which are stored as elements in the vector) that contain
# fewer than 60 characters.
livestock_data <- livestock_data[-c(which((nchar(livestock_data) > 60)))]



# The livestock data starts each day with a person's name and then has the quantity, type, weight, and price
# if the person made more than one purchase, the line starts with "\n\t\t"








  # It would appear as if we are only interested in indicies 5-100...
      # To access a list, we are looking for the first index at the 5th position
observations <- livestock_data[[1]][5]
for(i in 6:100){
  observations <- c(observations, livestock_data[[1]][i]) # All of our data is stored in the first position of the list, so now we index through until we get to the 100th position
}

  # This is the new array with only the observations we care about
print(observations)

# See for yourself on the website if these are the correct results.
# Now we have to clean the lines up with then gsub() function the observations
# and save into a numeric vector (to export to another file, or just use it locally)



# Perhaps you will want a data frame--one column with the type of cattle,
# and another containing the price?

# Before we get too far ahead of ourselves, let's finish cleaning this mess up:
  # Removing newline characters
observations <- gsub("\n", "", observations)
head(observations)

  # Removing tab characters
observations <- gsub("\t", "", observations)
head(observations)