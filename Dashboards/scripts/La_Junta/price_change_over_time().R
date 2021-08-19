# Price changes over time using the median to calculate the weekly average
price_change_over_time <- function(df){
  plot1 <- df %>%  
    filter(Reprod %in% c("str", "hfr")) %>% 
    group_by(Date, Reprod) %>% 
    summarize(Price = median(Price)) %>% 
    ggplot() +
    geom_line(mapping = aes(x = Date, y = Price), color = "orange") +
    facet_wrap(~Reprod) +
    ggtitle("Median Weekly Price") +
    theme_dark()
  
  
  plot2 <-  df %>%  
    filter(Reprod %in% c("bull", "cow")) %>% 
    group_by(Date, Reprod) %>% 
    summarize(Price = median(Price)) %>% 
    ggplot() +
    geom_line(mapping = aes(x = Date, y = Price), color = "orange") +
    facet_wrap(~Reprod) +
    theme_dark()
  
  # using patchwork's "/" operator
  plot1 / plot2
}