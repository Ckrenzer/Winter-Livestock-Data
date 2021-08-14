extract_buyer_name <- function(livestock_data = livestock_data){
  # Pulling out the indexes that contain the buyer's name.
  buyers <- livestock_data[!str_detect(livestock_data, "^\t+")] %>% 
    str_trim()
  
  # Extracting the buyer's name--the name ends when
  # the first "\t" is encountered:
  buyers <- str_extract(buyers, ".*?(?=\t)")
  
  # If the entries are not tab-delimited, that means they used spaces instead.
  if(all(is.na(buyers))){
    # Pulling out the indexes that contain the buyer's name
    # (doesn't start with a number, but contains a number; also does not contain a semicolon)
    buyers <- livestock_data[!str_detect(livestock_data, "^\\d") &
                               str_detect(livestock_data, "\\d") &
                               !str_detect(livestock_data, ";")] %>% 
      str_trim()
    # Extracting the buyer names
    buyers <- str_extract_all(buyers, "^[a-z].*?\\s\\d", simplify = FALSE)[[1]] %>% 
      str_remove_all("\\d")
  }
  
  # Removing unneeded white space and missing values
  buyers <- str_trim(buyers)
  buyers <- buyers[!is.na(buyers)] 
  
  
  return(buyers)
}