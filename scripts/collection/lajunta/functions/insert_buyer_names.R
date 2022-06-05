# This section uses ID numbers to identify the buyer for
# each sale listed on the market report. It then adds
# the buyer's name back onto the lines from which it
# was omitted.
insert_buyer_names <- function(text = livestock_data, buyer_names = buyers){
  # Provides each buyer with an ID number to determine
  # which purchases that buyer made.
  current_ID <- 0
  for(i in seq_along(text)){
    # Multiple purchases:
    # Only multiple purchases will have the "\n\t\t" or start with a digit, so we can
    # use "\n?\t+|^//d" as a placeholder for the beginning of the string
    # (where the buyer's name goes) and make it look the same as the
    # lines where the name is present
    is_multiple_purchase <- str_detect(text[i], "^\n?\t+\\s*\\d|^\\d")
    
    if(is_multiple_purchase){
      text[i] <- paste0(buyer_names[current_ID], "\t", text[i])
      text[i] <- str_remove(text[i], "\n")
    } else {
      # Move to the next buyer
      current_ID <- current_ID + 1 
    }
  }
  
  return(text)
}