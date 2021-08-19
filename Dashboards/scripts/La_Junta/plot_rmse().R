# Creates the plots showing the root mean squared error
plot_rmse <- function(trained_models, test_df){
  # trained_models is a list containing the trained models
  
  # Calculating predicted values
  results <- test_df %>% 
    bind_cols(predict(trained_models[["lm_fit"]], test_df)) %>% 
    rename(lm_estimates = .pred) %>% 
    bind_cols(predict(trained_models[["rf_fit"]], test_df)) %>% 
    rename(rf_estimates = .pred)
  
  # Converting the price back to original units
  # and making a column that extracts the year, for plotting
  results <- results %>% 
    mutate(lm_estimates = exp(lm_estimates),
           rf_estimates = exp(rf_estimates),
           Price = exp(Price),
           Year = as.factor(get_year(Date)))
  
  # The MSE output graphs
  lm_rmse_plot <- results %>% 
    ggplot(mapping = aes(x = Price, y = lm_estimates)) +
    geom_abline(lty = 2, color = "gray50") +
    geom_point(aes(color = Year), size = 1.5, alpha = 0.3, show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Actual Price") +
    ylab("Predicted Price") +
    ggtitle("Linear Regression (cents per pound)")
  
  rf_rmse_plot <- results %>% 
    ggplot(mapping = aes(x = Price, y = rf_estimates)) +
    geom_abline(lty = 2, color = "gray50") +
    geom_point(aes(color = Year), size = 1.5, alpha = 0.3, show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Actual Price") +
    ylab("Predicted Price") +
    ggtitle("Random Forest (cents per pound)")
  
  return(lm_rmse_plot + rf_rmse_plot + plot_annotation(title = "Predicted vs. Actual Price, by Model")) 
}