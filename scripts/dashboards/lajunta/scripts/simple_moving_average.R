simple_moving_average <- function(df, reprod_status, variable_name, n){
  weekly_avg <- df %>% 
    filter(Reprod == reprod_status) %>%  
    group_by(Date) %>% 
    summarize("mean_price" = mean({{variable_name}}))
  
  avg <- numeric(nrow(weekly_avg) - n)
  
  #pre-allocating space in a vector
  last_date_in_period <- character(nrow(weekly_avg) - n)
  
  for(i in n:nrow(weekly_avg)){
    avg[(i-n)] <- mean(weekly_avg$mean_price[(i-(n-1)):i])
    last_date_in_period[i-n] <- as.character(weekly_avg$Date[i])
    
  }
  last_date_in_period <- as.Date(last_date_in_period)
  
  sma_results <- data.frame(Date = last_date_in_period, Avg_Price = avg) %>% 
    ggplot() +
    geom_line(mapping = aes(x = Date, y = Avg_Price), color = "green", size = 1) +
    xlab("Final Date in Period") +
    ylab("Average Price") +
    theme_dark()
  
  return(sma_results)
}