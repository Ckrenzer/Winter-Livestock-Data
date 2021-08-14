# Each element of the vector represents a row in the final data frame,
# and the different columns will be created with a separate() call.
# For the separate() call to work, we need a delimiter between each field.
insert_delimiter <- function(livestock_data = livestock_data){
  # Inserting tab characters between the fields except quantity and type
  livestock_data <- str_replace_all(livestock_data, "(\\d)\\s(\\d)", "\\1\t\\2") 
  livestock_data <- str_replace_all(livestock_data, "([a-z])\\s+(\\d)", "\\1\t\\2")
  
  # Remove any extra "\t" characters after the buyer name
  livestock_data <- str_replace_all(livestock_data, "\t+\\s*", "\t")
  # Remove missing values
  livestock_data <- livestock_data[!is.na(livestock_data)]
  
  return(livestock_data)
}