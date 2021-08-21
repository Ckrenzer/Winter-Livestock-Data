# Fits a model based on the reproductive status
fit_models <- function(df, reprod){
  # Setting the random number generator
  set.seed(2021)
  
  # Testing and training splits
  initial_split_data <- df %>% 
    dplyr::filter(Reprod == reprod) %>% 
    dplyr::select(-Reprod) %>% 
    initial_split(prob = 0.75)
  training_set <- rsample::training(initial_split_data)
  testing_set <- rsample::testing(initial_split_data)
  
  # Model specifications
  lm_mod <- parsnip::linear_reg() %>% 
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("lm")
  rf_mod <- parsnip::rand_forest() %>% 
    parsnip::set_mode("regression") %>% 
    parsnip::set_engine("randomForest")
  
  # Model fitting
  lm_fit <- lm_mod %>% 
    parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set)
  rf_fit <- rf_mod %>% 
    parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set)
  
  return(list(lm_fit = lm_fit, rf_fit = rf_fit, test = testing_set))
}