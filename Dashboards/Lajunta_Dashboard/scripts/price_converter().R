# Calculates the price to be passed to price_calculation({})
price_converter <- function(model, date, weight, quantity, reprod){
  # Extracting the predicted price based on the inputs
  price <- predict(object = model,
                   new_data = tibble(Date = date,
                                     Weight = weight,
                                     Quantity = quantity,
                                     Reprod = reprod)) %>% 
    pull(.pred)
  
  # Converting the price to dollars, from log(cents)
  price <- round((exp(price) / 100), 2)
  return(price)
}