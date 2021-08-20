# Setting the random number generator -------------------------------------------------
set.seed(2021)

# Testing and training splits ---------------------------------------------------------
# Steers
initial_split_data_steer <- model_data %>% 
  dplyr::filter(Reprod == "str") %>% 
  dplyr::select(-Reprod) %>% 
  initial_split(prob = 0.75)
training_set_steer <- rsample::training(initial_split_data_steer)
testing_set_steer <- rsample::testing(initial_split_data_steer)

# Heifers
initial_split_data_heifer <- model_data %>% 
  dplyr::filter(Reprod == "hfr",
                Price < log(225)) %>% 
  dplyr::select(-Reprod) %>% 
  initial_split(prob = 0.75)
training_set_heifer <- rsample::training(initial_split_data_heifer)
testing_set_heifer <- rsample::testing(initial_split_data_heifer)

# Cows
initial_split_data_cow <- model_data %>% 
  dplyr::filter(Reprod == "cow") %>% 
  dplyr::select(-Reprod) %>% 
  initial_split(prob = 0.75)
training_set_cow <- rsample::training(initial_split_data_cow)
testing_set_cow <- rsample::testing(initial_split_data_cow)

# Bulls
initial_split_data_bull <- model_data %>% 
  dplyr::filter(Reprod == "bull",
                Price < log(115)) %>% 
  dplyr::select(-Reprod) %>% 
  initial_split(prob = 0.75)
training_set_bull <- rsample::training(initial_split_data_bull)
testing_set_bull <- rsample::testing(initial_split_data_bull)

# Model specifications ----------------------------------------------------------------
lm_mod <- parsnip::linear_reg() %>% 
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("lm")
rf_mod <- parsnip::rand_forest() %>% 
  parsnip::set_mode("regression") %>% 
  parsnip::set_engine("randomForest")

# Model fitting -----------------------------------------------------------------------
# Steers
lm_fit_steer <- lm_mod %>% 
  parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set_steer)
rf_fit_steer <- rf_mod %>% 
  parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set_steer)

# Heifer
lm_fit_heifer <- lm_mod %>% 
  parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set_heifer)
rf_fit_heifer <- rf_mod %>% 
  parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set_heifer)

# Cows
lm_fit_cow <- lm_mod %>% 
  parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set_cow)
rf_fit_cow <- rf_mod %>% 
  parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set_cow)

# Bulls
lm_fit_bull <- lm_mod %>% 
  parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set_bull)
rf_fit_bull <- rf_mod %>% 
  parsnip::fit(Price ~ Date + Weight + Quantity, data = training_set_bull)

# Storing Results -----------------------------------------------------------------------
model_results <- list(steer = list(lm_fit = lm_fit_steer, rf_fit = rf_fit_steer, test = testing_set_steer),
                      heifer = list(lm_fit = lm_fit_heifer, rf_fit = rf_fit_heifer, test = testing_set_heifer),
                      cow = list(lm_fit = lm_fit_cow, rf_fit = rf_fit_cow, test = testing_set_cow),
                      bull = list(lm_fit = lm_fit_bull, rf_fit = rf_fit_bull, test = testing_set_bull))

# Writing to an RDS file
write_rds(x = model_results,
          file = "Dashboards/scripts/La_Junta/saved_objects/La Junta lm and rf models.rds",
          compress = "gz")



# Removing unneeded references to model results -----------------------------------------
rm(initial_split_data_steer, initial_split_data_heifer, initial_split_data_cow, initial_split_data_bull,
   training_set_steer, training_set_heifer, training_set_cow, training_set_bull,
   testing_set_steer, testing_set_heifer, testing_set_cow, testing_set_bull,
   lm_mod, rf_mod,
   lm_fit_steer, lm_fit_heifer, lm_fit_cow, lm_fit_bull,
   rf_fit_steer, rf_fit_heifer, rf_fit_cow, rf_fit_bull)