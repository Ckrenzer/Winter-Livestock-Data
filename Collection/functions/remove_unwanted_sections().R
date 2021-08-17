remove_unwanted_sections <- function(text = livestock_data){
  
  # A set of keywords designed to remove heading information
  # and other information we are not interested in observing
  keywords <- "\\s+sold|\\s+monday|\\s+tuesday|\\s+wednesday|\\s+thursday|\\s+friday|\\s+saturday|\\s+sunday|\\s+receipts|\\s+through|\\s+mostly|\\s+winter|\\s+summer|\\s+spring|\\s+fall|\\s+autumn|\\s+is\\s+|\\s+next|\\s+quality|\\s+mostly|\\s+noon|\\s+early|\\s+stock|\\s+steady|\\s+test\\s+|\\s+offer|\\s+selection|\\s+week|\\s+annual|\\s+package|consigned|\\s*now\\s+|special\\s+|\\s+higher|calves\\s&\\syearlings\\s*$|\\s+am\\s+|\\s+pm\\s+|\\s+a.m.\\s+|\\s+p.m.\\s+|report[:]?\\s+|la\\s+junta,|\\s+co$|\\*$|estimate|internet\\svideo"                                           
  # Removes headings and unrelated information from the data
  text <- text[!str_detect(text, keywords)]
  # Removes all remaining entries with months and/or dates
  text <- text[!str_detect(text, "january\\s+\\d|february\\s+\\d|march\\s+\\d|april\\s+\\d|may\\s+\\d|june\\s+\\d|july\\s+\\d|august\\s+\\d|september\\s+\\d|october\\s+\\d|november\\s+\\d|december\\s+\\d|jan\\s+\\d|feb\\s+\\d|mar\\s+\\d|apr\\s+\\d|jun\\s+\\d|jul\\s+\\d|aug\\s+\\d|sept\\s+\\d|oct\\s+\\d|nov\\s+\\d|dec\\s+\\d")]
  
  
  # We can pull out the sales information by removing lines we do not
  # care about. Since we know that the information we want is stored
  # in lines that are much shorter than the others, we can pull out
  # lines that have fewer characters than some optimal number.
  # I chose 60. In other words, I am keeping only those lines
  # (which are stored as elements in the vector) that contain
  # fewer than 60 characters.
  text <- text[-c(which((nchar(text) > 60)))]
  
  
  # The livestock data starts each day with a person's name and then has the quantity, type, weight, and price
  # if the person made more than one purchase, the line starts with "\n\t\t"--this is the reason
  # why we cannot make use of str_trim(). We need the "\n\t\t" to indicate whether
  # that element in the vector is really another purchase by the same buyer.
  
  
  # the sales are the entries that do not have semicolons but do have digits
  text <- text[!str_detect(text, ";") & str_detect(text, "\\d")]
  
  # the sales are the entries with at least two distinct numbers (quantity, price, weight)
  text <- text[str_detect(text, "\\d+\\D+\\d+")]
  
  # Keeping only those entries with more than 12 characters (for those entries that slip through the other filters)
  text <- text[which(nchar(text) > 12)] 
  
  return(text)
}