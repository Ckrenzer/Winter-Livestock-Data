extract_webpage_text <- function(url = URL){
  # The HTML elements are consistent across webpages
  livestock_data <- read_html(url) %>% 
    html_nodes("div:nth-child(9) div.sml") %>% 
    html_text() %>% 
    return()
}