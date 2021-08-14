split_text <- function(text = livestock_data){
  # Asks whether the list has more than 30 entries after splitting.
  # If it does, that means we can split the text up with that delimiter
  # and should return. If neither delimiter works, we will return a NULL
  # and the loop will go to the next iteration. Returns data in lowercase.
  data <- str_split(text, "\n")[[1]]
  if(length(data) >= 30){
    return(str_to_lower(data))
  } 
  data <- str_split(text, "\r")[[1]]
  if(length(data) >= 30){
    return(str_to_lower(data))
  }
  return(NULL)
}