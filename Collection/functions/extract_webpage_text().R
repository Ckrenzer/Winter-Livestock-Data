extract_webpage_text <- function(URL = URL){
  # The HTML elements are consistent across webpages
  livestock_data <- read_html(URL) %>% 
    html_nodes("div:nth-child(9) div.sml") %>% 
    html_text() %>% 
    return()
}