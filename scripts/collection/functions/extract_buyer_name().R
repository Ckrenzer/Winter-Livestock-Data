extract_buyer_name <- function(text = livestock_data){
  # Pulling out the indexes that contain the buyer's name.
  buyers <- text[!str_detect(text, "^\t+")] %>% 
    str_trim()
  
  # Extracting the buyer's name--the name ends when
  # the first "\t" is encountered:
  buyers <- str_extract(buyers, ".*?(?=\t)")
  
  # If the entries are not tab-delimited, that means they used spaces instead.
  if(all(is.na(buyers))){
    # Pulling out the indexes that contain the buyer's name
    # (doesn't start with a number and does not contain a semicolon or comma)
    buyers <- text[!str_detect(text, "^\\d") &
                     !str_detect(text, ";|,")] %>% 
      str_trim()
    # Extracting the buyer names
    buyers <- str_extract_all(buyers, "^[a-z].*?\\s\\d", simplify = FALSE) %>% 
      str_remove_all("\\d") %>% 
      unlist()
  }
  
  # Removing unneeded white space and missing values
  buyers <- str_trim(buyers)
  buyers <- buyers[!is.na(buyers)] 
  
  
  return(buyers)
}