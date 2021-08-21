# Plots the results of an arima model forecasting 6mo into the future
arima_plot <- function(df, reprod, reprod_fullname, lags, diff_deg, ma_term){
  
  
  # A date vector for forecasted dates
  dates <- as.Date(character(26))
  for(week_num in 1:26){
    dates[week_num] <- clock::add_weeks(df$Date[df$Date == max(df$Date)], week_num)
  }#end of for loop
  
  
  avg_prices <- df %>% 
    filter(Reprod == reprod) %>% 
    group_by(Date) %>% 
    summarize("median_price" = median(Price)) %>% 
    mutate(
      "cleaned_price" = tsclean(ts(median_price)),
      "ma_6mo" = ma(cleaned_price, order = 26),
      "seas_adj" = c(
        rep(NA, 26),
        seasadj(stl(ts(na.omit(ma_6mo), frequency = 26), s.window = "periodic"))
      ), 
      "seas_adj_diff" = c(rep(NA, diff_deg),
                          diff(seas_adj, differences = diff_deg)
      )
    )
  
  
  # The steer model
  model_fit <- arima(avg_prices$seas_adj, order = c(lags, diff_deg, ma_term))
  
  # The predictions for the model, six months out.
  price_predictions <- forecast(model_fit, h = 26)
  
  # Adding in the date column for the six month forecast
  price_predictions <- as_tibble(price_predictions) %>% 
    mutate(Date = dates)
  
  # The six month forecast
  price_forecast <- ggplot() +
    geom_line(data = avg_prices, mapping = aes(x = Date, y = seas_adj), color = "orange", size = 1) +
    geom_ribbon(data = price_predictions, mapping = aes(x = Date, ymin = `Lo 95`, ymax = `Hi 95`),
                fill = "red") +
    geom_ribbon(data = price_predictions, mapping = aes(x = Date, ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue") +
    geom_line(data = price_predictions, mapping = aes(x = Date, y = `Point Forecast`), color = "blue", size = 1) +
    ggtitle(reprod_fullname) +
    xlab("Date") +
    ylab("Cents Per Pound") +
    theme_dark()
  
  return(price_forecast)
}