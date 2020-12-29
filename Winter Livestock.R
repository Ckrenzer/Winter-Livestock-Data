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


# Keywords ------------------------------------------------------
# A set of keywords designed to remove heading information
# and other information we are not interested in observing
keywords <- "\\s+sold|\\s+sale|\\s+monday|\\s+tuesday|\\s+wednesday|\\s+thursday|\\s+friday|\\s+saturday|\\s+sunday|\\s+receipts|\\s+through|\\s+mostly|\\s+winter|\\s+summer|\\s+spring|\\s+fall|\\s+autumn|\\s+is\\s+|\\s+next|\\s+quality|\\s+mostly|\\s+noon|\\s+early|\\s+stock|\\s+steady|\\s+test\\s+|\\s+offer|\\s+selection|\\s+week|\\s+package|consigned|\\s*now\\s+|special\\s+|\\s+higher|calves\\s&\\syearlings\\s*$|\\s+am\\s+|\\s+pm\\s+|report[:]?\\s+"


# You can "check your work" for the heading removal
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
# if the person made more than one purchase, the line starts with "\n\t\t"--this is the reason
# why we cannot make use of str_trim(). We need the "\n\t\t" to indicate whether
# that element in the vector is really another purchase by the same buyer.

# You can see what I am talking about by running
# the code below. The "\n\t\t" is hidden, but
# you can see the highlighted boxes at the
# beginning of the lines containing the pattern:
str_view(livestock_data, "\n\t\t")



# What I need:
#   1. names of buyers (use the str_detect() line for that one)
#   2. indices where there is more than one sale?
#   3. paste the names of buyers on the rows where it is missing,
#      following the same pattern as those that do have the names
#      present. You are essentially adding the name back into the
#      observations.




# Step 1. Names of Buyers:
# Pulling out the indices that contain the buyer's name.
buyers <- livestock_data[!str_detect(livestock_data, "\n\t\t")]

# removing leading newline characters
buyers <- str_trim(buyers)

# Extracting the buyer's name--the name ends when
# the first "\t" is encountered:

# From the looks of things, there are four formats for the buyer's name,
# where the underscore represents a space:
#   1. word_word
#   2. word_word_word
#   3. word_&_word_word
#   4. word_word_&_word

# This regex should take care of the four cases above:
str_view(buyers, "^([a-z]*\\s*&*\\s*[a-z]*\\s*&*\\s*[a-z]*)\t")

# The names of the buyers:
buyers <- str_extract(buyers, "^([a-z]*\\s*&*\\s*[a-z]*\\s*&*\\s*[a-z]*)\t")
# removing unneded whitespace
buyers <- str_trim(buyers)




# I will provide each buyer an ID number to determine
# which name goes where
current_ID <- 1
ID_nums <- 0
for(i in 1:length(livestock_data)){
  if(str_detect(livestock_data[i], "\n\t\t")){
    # Only multiple purchases will have the "\n\t\t" so we can
    # use "\n\t\t" as a placeholder for the beginning of the string,
    # where the buyer's name goes, and make it look the same as the
    # lines where the name is present
    livestock_data[i] <- str_replace(livestock_data[i], "\n\t\t",  paste(buyers[current_ID], "\t", sep = ""))
  } else {
    # if the index lands on the next buyer, give them
    # a new ID number
    current_ID <- current_ID + 1 
  }
  
  # add the ID numbers to the ID_nums variable
  ID_nums <- c(ID_nums, current_ID)
  
}# end of for loop
# removing the first element of ID_nums
ID_nums <- ID_nums[-1]


paste("hi,", "how", "are", "you?", sep = " ")







# I'm no fan of loops, but I think a loop is
# one of the most straightforward ways of
# pulling out the needed information...
for(i in livestock_data){
  
  
}