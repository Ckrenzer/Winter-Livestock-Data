determine_date_of_sale <- function(livestock_data = livestock_data){
  # Saves the first entry that finds digits followed by the word "cattle"--this entry
  # contains the sentence from which we can extract the date
  first_element_containing_date <- which(str_detect(livestock_data, "\\d+\\s*cattle"))[1]
  date_of_sale_sentence <- livestock_data[first_element_containing_date] %>% 
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
  
  
  ##### CASE 2: The date is multi-valued and the year is included
  #note: the lubridate::mdy() call is meant to ensure we get NA if the formatting did not work
  #in the previous step
  if(is.na(lubridate::mdy(date_of_sale))){
    
    # Pulling out the part of the sentence containing the date
    date_of_sale <- str_extract(date_of_sale_sentence, paste0(month_of_sale, "\\s+\\d{1,2}.{0,40}\\d{4}"))
    
    # month and day
    md <- str_extract(date_of_sale, paste0(month_of_sale, "\\s+\\d{1,2}")) %>% 
      month_name_to_num(text = .) %>% 
      str_replace_all("\\s+", "-")
    # year
    y <- str_extract(date_of_sale, "\\d{4}")
    
    #we only want to do this step if date_of_sale is not null (we will get a string saying "NA-NA" otherwise,
    #which is a pain to correct for in subsequent case-checking)
    if(!is.na(date_of_sale)){
      # Concatenating the md and y together (into mdy format!!!)
      date_of_sale <- paste(md, y, sep = "-")
    }
    
  }
  
  
  ##### CASE 3: The year was not provided
  if(is.na(lubridate::mdy(date_of_sale))){
    
    #Pulling out the month and day
    md <- str_extract(date_of_sale_sentence, paste0(month_of_sale, "\\s+\\d{1,2}"))
    #Setting the year equal to the previous sale's year
    y <- clock::get_year(clock::date_parse(previous_date_of_sale, format = "%m-%d-%y"))  
    
    
    # Changing the month to its corresponding number
    md <- month_name_to_num(text = md) %>% 
      #replacing the date with a hyphen
      str_replace_all("\\s+", "-")
    
    #we only want to do this step if date_of_sale is not null (we will get a string saying "NA-NA" otherwise,
    #which is a pain to correct for in subsequent case-checking)
    if(!is.na(date_of_sale)){
      # Concatenating the md and y together (into mdy format!!!)
      date_of_sale <- paste(md, y, sep = "-")
    }
    
  }
  
  
  
  ##### CASE 4: The Catch-All
  #If worse comes to worst, we can just add one week from the previous sale's date
  if(is.na(lubridate::mdy(date_of_sale))){
    
    date_of_sale <- lubridate::mdy(previous_date_of_sale) %>% 
      clock::add_weeks(n = 1) %>% 
      as.character()
    
  } 
  
  
}