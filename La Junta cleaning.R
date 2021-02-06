# This script is optional, though it will simplify your analysis

# There are two primary goals with this script:
#   1. Add column names to the csv
#   2. Organize types of cattle into a small number of distinct groups

# goal 1 is a piece of cake, but goal 2 is easier said than done.
# You can analyze some of my judgment calls and decide for
# yourself what needs to be done to group the different types
# of livestock.


# Packages ------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, readr, dplyr)



# Step 1: adding a header to the csv ----------------
lajunta <- readr::read_csv("La Junta Market Reports.csv", 
                           col_names = F)

# Adding in the column names
lajunta <- lajunta %>% 
  rename("Date" = X1,
         "Buyer" = X2,
         "Quantity" = X3,
         "Type" = X4,
         "Weight" = X5,
         "Price" = X6)



# Step 2: categorizing cattle -----------------------
# Making a column assigning cattle reproductive status
lajunta <- lajunta %>% 
  mutate(Reprod = str_extract(lajunta$Type, "hfr$|str$|bull$|cow$"))

# The bulk of the edits
lajunta$Type <- lajunta %>% 
  select(Type) %>% 
  unlist() %>% 
  str_remove_all("\\sx.*$|\\s[^\\s]*$") %>%  # removing " x hfr", " hfr", " x str", etc.
  str_replace_all("angus", "ang") %>% 
  str_remove_all("sim-") %>%                      # removing the sim- from sim-angus
  str_replace_all("sim\\s+ang", "ang") %>%        # removing "sim " from "sim angus"
  str_replace_all("\\s+sim", " ang") %>%          # replacing " sim" with " ang"
  str_replace_all("limousin", "lim") %>% 
  str_replace_all("limo", "lim") %>%              # replacing "limo" with "lim"
  str_replace_all(".*\\s+lim|lim flex", "lim") %>%# combining all different types of limousins 
  str_replace_all("gelbvieh", "gel") %>% 
  str_replace_all("hereford", "here") %>% 
  str_replace_all("charolais", "char") %>% 
  str_replace_all("brahman", "brah") %>%
  str_remove_all("wf-|bwf-|rwf-") %>%             # removing face types from the data when there is other info
  str_remove_all("\\s*&") %>%                     # removing ampersands
  str_replace_all("\\s?blk\\s?", "black") %>%     # replacing abbreviation "blk" with "black"
  str_replace_all("red black", "black red") %>%   # reordering "red and black" to "black and red"
  str_replace_all(".*-.*", "mix") %>%             # replaces any named cross-breed with "mix"
  str_replace_all("balancer", "mix") %>%          # replaces "balancer" with "mix", since it's a hybrid
  str_remove_all("\\s+wf|\\s+bwf|\\s+rwf") %>%    # removing face types at after the name
  str_replace_all("^wf$", "bwf") %>%              # recoding "wf" as "bwf" because it performs similarly and has few observations
  str_remove_all("wf\\s+|bwf\\s+|rwf\\s+") %>%    # removing face types before the name
  str_replace_all("here|brah", "red") %>%         # replacing "here" and "brah" with "red" due to few observations and similar weight
  str_replace_all("black\\s+(ang)", "\\1") %>%    # removing color starts on this line
  str_replace_all("black\\s+(lim)", "\\1") %>% 
  str_replace_all("black\\s+(gel)", "\\1") %>% 
  str_replace_all("black\\s+(here)", "\\1") %>% 
  str_replace_all("black\\s+(char)", "\\1") %>% 
  str_replace_all("red\\s+(ang)", "\\1") %>% 
  str_replace_all("red\\s+(lim)", "\\1") %>% 
  str_replace_all("red\\s+(gel)", "\\1") %>% 
  str_replace_all("red\\s+(here)", "\\1") %>% 
  str_replace_all("red\\s+(char)", "\\1") %>%     # removing color ends on this line
  str_squish() %>% 
  str_trim()


#combining all colors into a new category--"clr" (optional)
lajunta$Type <- lajunta %>% 
  select(Type) %>% 
  unlist() %>% 
  str_replace_all("black red|black|red", "clr") %>%   # "black red," "black," and "red" are all 
  str_replace_all("bwf|rwf|wf", "face") %>%
  str_replace_all("face", "clr")                  # remove this line if you want to stop pooling the face group with the color group

# We now have six categories in the "Type" variable





# Step 3: Writing to file ---------------------------
write_csv(x = lajunta,
          file = "La Junta Market Reports.csv",
          append = F,
          col_names = T)
