# We load in the app's data using this script

# Reading in the file and removing the URL column
# Fortunately, we only have to do this once
lajunta <- readr::read_csv("https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/La%20Junta%20Market%20Reports.csv",
                           col_types = readr::cols(Date = col_date("%m-%d-%Y"),
                                                   Buyer = readr::col_factor(),
                                                   Quantity = readr::col_double(),
                                                   Type = readr::col_factor(),
                                                   Weight = readr::col_double(),
                                                   Price = readr::col_double(),
                                                   URL = readr::col_character(),
                                                   Reprod = readr::col_factor())) %>% 
  dplyr::select(-URL)


# The full, unfiltered dataset for export
lajunta_full <- lajunta


lajunta <- lajunta %>% 
  dplyr::filter(!is.na(Reprod))

# This is meant to speed up the runtime instead of placing it in multiple reactive functions
outliers_removed <- lajunta %>% 
  filter(Price < 325)

# recodes bulls that act like steers on the market as steers
# and similarly for cows that act like heifers
model_data <- outliers_removed %>% 
  dplyr::select(-Buyer) %>% 
  mutate(Price = log(Price),
         Reprod = as.character(Reprod),
         Reprod = case_when(
           Reprod == "bull" && Price > 125 ~ "str",
           Reprod == "cow" && Price > 100 ~ "hfr",
           TRUE ~ Reprod),
         Reprod = as.factor(Reprod)
  )