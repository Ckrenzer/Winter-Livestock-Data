# Each element of the vector represents a row in the final data frame,
# and the different columns will be created with a separate() call.
# For the separate() call to work, we need a delimiter between each field.
insert_delimiter <- function(text = livestock_data){
  # Inserting tab characters between the fields except quantity and type
  text <- str_replace_all(text, "(\\d)\\s(\\d)", "\\1\t\\2") 
  text <- str_replace_all(text, "([a-z])\\s+(\\d)", "\\1\t\\2")
  
  # Remove any extra "\t" characters after the buyer name
  text <- str_replace_all(text, "\t+\\s*", "\t")
  # Remove missing values
  text <- text[!is.na(text)]
  
  return(text)
}