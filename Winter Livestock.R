library(rvest)


# Saving url into a String variable
url <- "http://www.winterlivestock.com/lajunta.php"

# Saving the read webpage into a variable
webpage <- read_html(url)

# Saving some data from the webpage written in html
livestock_data_html <- html_nodes(webpage, "div:nth-child(9) div.sml")

# Converting the html code to plain text
livestock_data <- html_text(livestock_data_html)

# This is still quite messy
head(livestock_data)


# Next step: cleaning
# Let's split the String up into an array of Strings using a delimiter ("\r" in this case--I think "\n" would have worked just fine)

  # We have all the lines separated into a list. Now, we just grab the lines we want and manipulate the new vector to extract the numbers
livestock_data <- strsplit(livestock_data, "\r")

# To see what I just did:
print(livestock_data)

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


# At this point, you will want to find a function that can pull
# out information by a keyword or number. I'll leave that to you.

# Cheers!
# -Connor