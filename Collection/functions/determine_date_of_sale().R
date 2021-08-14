determine_date_of_sale <- function(livestock_data = livestock_data){
  # Finding the date of sale ----------------------------------------
  # Saves the first entry that finds digits followed by the word "cattle"--this entry
  # contains the sentence from which we can extract the date
  date_of_sale_sentence <- livestock_data[which(str_detect(livestock_data, "\\d+\\s*cattle"))[1]]
  # removing punctuation from the sentence
  date_of_sale_sentence <- str_remove_all(date_of_sale, "\\.|,|;")
  
  
  # extracting the month the sale took place from the sentence
  month_of_sale <- str_extract(date_of_sale_sentence, "january|february|march|april|may|june|july|august|september|october|november|december|jan\\s|feb\\s|mar\\s|apr\\s|jun\\s|jul\\s|aug\\s|sept\\s|oct\\s|nov\\s|dec\\s")
  
  
  
  ##### CASE 1: The date is in "month ##th YYYY" format (Ex. "march 9th 2021")
  #note: some sales take place over two days, so we have to remove the "& ##th" following the first day's date
  date_of_sale <- str_extract(date_of_sale_sentence, paste0(month_of_sale, "\\s+\\d{1,2}[a-z0-9&\\s]{0,15}\\d{4}")) %>% 
    #replacing the month name with its corresponding number
    str_replace_all(month_of_sale,
                    case_when(month_of_sale == "january" ~ "1",
                              month_of_sale == "february" ~ "2",
                              month_of_sale == "march" ~ "3",
                              month_of_sale == "april" ~ "4",
                              month_of_sale == "may" ~ "5",
                              month_of_sale == "june" ~ "6",
                              month_of_sale == "july" ~ "7",
                              month_of_sale == "august" ~ "8",
                              month_of_sale == "september" ~ "9",
                              month_of_sale == "october" ~ "10",
                              month_of_sale == "november" ~ "11",
                              month_of_sale == "december" ~ "12",
                              month_of_sale == "jan" ~ "1",
                              month_of_sale == "feb" ~ "2",
                              month_of_sale == "mar" ~ "3",
                              month_of_sale == "apr" ~ "4", #you don't need to repeat MAY, remember
                              month_of_sale == "jun" ~ "6",
                              month_of_sale == "jul" ~ "7",
                              month_of_sale == "aug" ~ "8",
                              month_of_sale == "sept" ~ "9",
                              month_of_sale == "oct" ~ "10",
                              month_of_sale == "nov" ~ "11",
                              month_of_sale == "dec" ~ "12",
                    )) %>% 
    #removing letters and replacing space characters with hyphens
    str_remove_all("[a-z]") %>%
    str_replace_all("\\s+", " ") %>% #replacing multiple spaces with just one space
    str_replace_all("\\s", "-")
  
  
  
  ##### CASE 2: The date is multi-valued but the year is included
  
  # NOTE: the lubridate::mdy() call is meant to ensure we get NA if the formatting did not work as expected.
  
  if(is.na(lubridate::mdy(date_of_sale))){
    
    # Pulling out the part of the sentence containing the date
    date_of_sale <- str_extract(date_of_sale_sentence, paste0(month_of_sale, "\\s+\\d{1,2}.{0,40}\\d{4}"))
    
    #Pulling out the month and day
    md <- str_extract(date_of_sale, paste0(month_of_sale, "\\s+\\d{1,2}"))
    #Pulling out the year
    y <- str_extract(date_of_sale, "\\d{4}")
    
    # Changing the month to its corresponding number
    md <- str_replace_all(string = md,
                          pattern = month_of_sale,
                          replacement = case_when(month_of_sale == "january" ~ "1",
                                                  month_of_sale == "february" ~ "2",
                                                  month_of_sale == "march" ~ "3",
                                                  month_of_sale == "april" ~ "4",
                                                  month_of_sale == "may" ~ "5",
                                                  month_of_sale == "june" ~ "6",
                                                  month_of_sale == "july" ~ "7",
                                                  month_of_sale == "august" ~ "8",
                                                  month_of_sale == "september" ~ "9",
                                                  month_of_sale == "october" ~ "10",
                                                  month_of_sale == "november" ~ "11",
                                                  month_of_sale == "december" ~ "12",
                                                  month_of_sale == "jan" ~ "1",
                                                  month_of_sale == "feb" ~ "2",
                                                  month_of_sale == "mar" ~ "3",
                                                  month_of_sale == "apr" ~ "4", #you don't need to repeat MAY, remember
                                                  month_of_sale == "jun" ~ "6",
                                                  month_of_sale == "jul" ~ "7",
                                                  month_of_sale == "aug" ~ "8",
                                                  month_of_sale == "sept" ~ "9",
                                                  month_of_sale == "oct" ~ "10",
                                                  month_of_sale == "nov" ~ "11",
                                                  month_of_sale == "dec" ~ "12",
                          )) %>% 
      #replacing the date with a hyphen
      str_replace_all("\\s+", "-")
    
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
    md <- str_replace_all(string = md,
                          pattern = month_of_sale,
                          replacement = case_when(month_of_sale == "january" ~ "1",
                                                  month_of_sale == "february" ~ "2",
                                                  month_of_sale == "march" ~ "3",
                                                  month_of_sale == "april" ~ "4",
                                                  month_of_sale == "may" ~ "5",
                                                  month_of_sale == "june" ~ "6",
                                                  month_of_sale == "july" ~ "7",
                                                  month_of_sale == "august" ~ "8",
                                                  month_of_sale == "september" ~ "9",
                                                  month_of_sale == "october" ~ "10",
                                                  month_of_sale == "november" ~ "11",
                                                  month_of_sale == "december" ~ "12",
                                                  month_of_sale == "jan" ~ "1",
                                                  month_of_sale == "feb" ~ "2",
                                                  month_of_sale == "mar" ~ "3",
                                                  month_of_sale == "apr" ~ "4", #you don't need to repeat MAY, remember
                                                  month_of_sale == "jun" ~ "6",
                                                  month_of_sale == "jul" ~ "7",
                                                  month_of_sale == "aug" ~ "8",
                                                  month_of_sale == "sept" ~ "9",
                                                  month_of_sale == "oct" ~ "10",
                                                  month_of_sale == "nov" ~ "11",
                                                  month_of_sale == "dec" ~ "12",
                          )) %>% 
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