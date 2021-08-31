# This script identifies problem-areas within the La Junta dataset after scraping
if(!require(pacman)) install.packages("pacman")
pacman::p_load(readr, dplyr, ggplot2)





#lajunta <- read_csv("https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/La%20Junta%20Market%20Reports.csv")
lajunta <- read_csv("La Junta Market Reports.csv")



# finding market reports using the most recent market report instead of the permalink.
lajunta %>% 
  filter(URL == "http://www.winterlivestock.com/lajunta.php") %>% 
  as.data.frame()




# finds counts of all types--good for finding how many types are in the data
count(lajunta, Type) %>% 
  arrange(desc(n)) %>% 
  as.data.frame()

# finds counts of all reproductive statuses--there should be five: bull, cow, steer, heifer, and NA (if there are missing values).
count(lajunta, Reprod) %>% 
  arrange(desc(n)) %>% 
  as.data.frame()

# How many sales were there on a given day?
lajunta %>% 
  count(Date)

# Taking a look at the newest data
tail(lajunta)


# MISSING VALUES _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

# relevant info when imputing values:
lajunta %>% 
  group_by(Reprod) %>% 
  dplyr::summarize("Median price" = median(Price, na.rm = T),
                   "Median weight" = median(Weight, na.rm = T),
                   "Median quantity" = median(Quantity, na.rm = T),
                   "Mean Price" = mean(Price, na.rm = T),
                   "Mean weight" = mean(Weight, na.rm = T),
                   "Mean quantity" = mean(Quantity))
# KNN is probably the best imputation method for this data



# DATE -__-__-__-__-__-__-__-__-__-__-
# how many missing values are in the Date column?
lajunta %>% 
  filter(is.na(Date)) %>% 
  slice_sample(n = 20)


# BUYER _--_--_--_--_--_--_--_--_--_--_
lajunta %>% 
  filter(is.na(Buyer))



# QUANTITY -__-__-__-__-__-__-__-__-__-__-
lajunta %>% 
  filter(is.na(Quantity))



# TYPE _--_--_--_--_--_--_--_--_--_--_
lajunta %>% 
  filter(is.na(Type))



# WEIGHT -__-__-__-__-__-__-__-__-__-__-
lajunta %>% 
  filter(is.na(Weight))



# PRICE _--_--_--_--_--_--_--_--_--_--_
lajunta %>% 
  filter(is.na(Price))



# REPROD -__-__-__-__-__-__-__-__-__-__-
# see any patterns?
lajunta %>% 
  dplyr::select(Type, Weight, Reprod, URL) %>% 
  filter(is.na(Reprod)) %>% 
  as.data.frame()

# You can see the distribution of missing Reprod values
lajunta %>% 
  filter(is.na(Reprod)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = Weight, y = Price))

