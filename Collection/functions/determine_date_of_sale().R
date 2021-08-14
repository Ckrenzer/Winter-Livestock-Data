determine_date_of_sale <- function(text = livestock_data, previous_date = NULL){
  # Saves the first entry that finds digits followed by the word "cattle"--this entry
  # contains the sentence from which we can extract the date
  first_element_containing_date <- which(str_detect(text, "\\d+\\s*cattle"))[1]
  date_of_sale_sentence <- text[first_element_containing_date] %>% 
    str_remove_all("\\.|,|;")
  
  # Extracting the sale's month from the sentence
  month_name_regex <- "january|february|march|april|may|june|july|august|september|october|november|december|jan\\s|feb\\s|mar\\s|apr\\s|jun\\s|jul\\s|aug\\s|sept\\s|oct\\s|nov\\s|dec\\s"
  month_of_sale <- str_extract(date_of_sale_sentence, month_name_regex)
  
  
  
  
  ##### CASE 1: The date is in "month ##th YYYY" format (Ex. "march 9th 2021")
  #note: some sales take place over two days, so we have to remove the "& ##th" following the first day's date
  date_of_sale <- str_extract(date_of_sale_sentence, paste0(month_of_sale, "\\s+\\d{1,2}[a-z0-9&\\s]{0,15}\\d{4}")) %>% 
    month_name_to_num(text = .) %>% 
    str_remove_all("[a-z]") %>%
    str_replace_all("\\s+", " ") %>% 
    str_replace_all("\\s", "-")
  #note: if letters are found, that means the date did not parse correctly
  
  
  ##### CASE 2: The date is multi-valued and the year is included
  if(str_detect(date_of_sale, "[a-zA-Z]")){
    # Pulling out the part of the sentence containing the date
    date_of_sale <- str_extract(date_of_sale_sentence, paste0(month_of_sale, "\\s+\\d{1,2}.{0,40}\\d{4}"))
    
    # month and day
    md <- str_extract(date_of_sale, paste0(month_of_sale, "\\s+\\d{1,2}")) %>% 
      month_name_to_num(text = .) %>% 
      str_replace_all("\\s+", "-")
    # year
    y <- str_extract(date_of_sale, "\\d{4}")
    
    # Concatenating the md and y together (into mdy format!!!)
    date_of_sale <- paste(md, y, sep = "-")
  }
  
  
  ##### CASE 3: The year was not provided
  # We use the month and day provided then append the year of the last sale
  if(str_detect(date_of_sale, "[a-zA-Z]")){
    # month and day
    md <- str_extract(date_of_sale_sentence, paste0(month_of_sale, "\\s+\\d{1,2}"))
    # previous sale's year
    y <- clock::get_year(clock::date_parse(previous_date, format = "%m-%d-%y"))  
    
    # converting month name to month number
    md <- month_name_to_num(text = md) %>% 
      str_replace_all("\\s+", "-")
    
    # Concatenating the md and y together (into mdy format!!!)
    date_of_sale <- paste(md, y, sep = "-")
  }
  
  
  ##### CASE 4: The Catch-All
  # If worse comes to worst, we can just add one week from the previous sale's date
  if(str_detect(date_of_sale, "[a-zA-Z]")){
    date_of_sale <- lubridate::mdy(previous_date) %>% 
      clock::add_weeks(n = 1) %>% 
      as.character()
  } 
  
  return(date_of_sale)
}