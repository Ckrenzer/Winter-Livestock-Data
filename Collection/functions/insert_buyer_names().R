# This section uses ID numbers to identify the buyer for
# each sale listed on the market report. It then adds
# the buyer's name back onto the lines from which it
# was omitted.
insert_buyer_names <- function(livestock_data = livestock_data, buyers = buyers){
  # Provides each buyer with an ID number to determine
  # which purchases that buyer made.
  current_ID <- 0
  for(i in seq_along(livestock_data)){
    # Multiple purchases:
    # Only multiple purchases will have the "\n\t\t" or start with a digit, so we can
    # use "\n?\t+|^//d" as a placeholder for the beginning of the string
    # (where the buyer's name goes) and make it look the same as the
    # lines where the name is present
    is_multiple_purchase <- str_detect(livestock_data[i], "^\n?\t+\\s*\\d|^\\d")
    
    if(is_multiple_purchase){
      livestock_data[i] <- paste0(buyers[current_ID], "\t", livestock_data[i])
      livestock_data[i] <- str_remove(livestock_data[i], "\n")
    } else {
      # Move to the next buyer
      current_ID <- current_ID + 1 
    }
  }
  
  return(livestock_data)
}