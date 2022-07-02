# We load in the app's data using this script

# Reading in the file and removing the URL column
# Fortunately, we only have to do this once (readr::col_date() was giving me a hard time, so we use a mutate() call)
lajunta <- readr::read_csv("https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/data/ljmr.csv",
                           col_types = readr::cols(Date = readr::col_character(),
                                                   Buyer = readr::col_factor(),
                                                   Quantity = readr::col_double(),
                                                   Type = readr::col_factor(),
                                                   Weight = readr::col_double(),
                                                   Price = readr::col_double(),
                                                   Reprod = readr::col_factor(),
                                                   URL = readr::col_character())) %>% 
  dplyr::select(-URL) %>% 
  dplyr::mutate(Date = readr::parse_date(x = Date, format = "%m-%d-%Y"))


# The full, unfiltered dataset for export
lajunta_full <- lajunta


lajunta <- lajunta %>% 
  dplyr::filter(!is.na(Reprod),
                Reprod != "NA")

# This is meant to speed up the runtime instead of placing it in multiple reactive functions
outliers_removed <- lajunta %>% 
  dplyr::filter(Price < 325,
                Weight < 4000)

# recodes bulls that act like steers on the market as steers
# and similarly for cows that act like heifers
model_data <- outliers_removed %>% 
  dplyr::select(-Buyer) %>% 
  dplyr::mutate(Price = log(Price),
                Reprod = as.character(Reprod),
                Reprod = case_when(
                  Reprod == "bull" && Price > 125 ~ "str",
                  Reprod == "cow" && Price > 100 ~ "hfr",
                  TRUE ~ Reprod),
                Reprod = as.factor(Reprod)
  )
