# This script is optional, though it will simplify your analysis

# There are two primary goals with this script:
#   1. Add column names to the csv
#   2. Organize types of cattle into a small number of distinct groups

# goal 1 is a piece of cake, but goal 2 is easier said than done.
# You can analyze some of my judgment calls and decide for
# yourself what needs to be done to group the different types
# of livestock.

# Should the header have been made beforehand and
# therefore not need this code? Probably. That sounds
# like a headache to implement if the file is already
# in your system, so a separate script seems easier.

# Step 1: adding a header to the csv ----------------
lajunta <- readr::read_csv("La Junta Market Reports.csv")






# Step 2: categorizing cattle

# # # KEY # # #
# angus is "ang"
# limousin is "lim"
# gelbvieh is "gel"
# charolais is "char"
# bwf is "black white faced"
# rwf is "red white faced"
# wf is "white faced"
# sim is "sims-angus"
# lim flex is "flex limousin"
# brah is "brahman"
# here is "hereford"
# clr is what is assigned when only the cattle's color is provided




market <- read_csv("La Junta Market Reports.csv")
market$Date <- lubridate::mdy(market$Date)

# Making a column assigning cattle reproductive status
market <- market %>% 
  mutate(Reprod = str_extract(market$Type, "hfr$|str$|bull$|cow$"))

# removing "x hfr", " hfr", " x str", etc. from the Type column
market$Type <- str_remove_all(market$Type, "\\sx.*$|\\s[^\\s]*$")


# Making a column assigning cattle reproductive status
market <- market %>% 
  mutate(Reprod = str_extract(market$Type, "hfr$|str$|bull$|cow$"))

# removing "x hfr", " hfr", " x str", etc. from the Type column
market$Type <- str_remove_all(market$Type, "\\sx.*$|\\s[^\\s]*$")

market$Type <- market %>% 
  select(Type) %>% 
  unlist() %>% 
  str_replace_all("angus", "ang") %>% 
  str_remove_all("sim-") %>%                      # removing the sim- from sim-angus, as we are not interested in this characteristic
  str_replace_all("sim\\s+ang", "ang") %>% 
  str_replace_all("\\s+sim", " ang") %>% 
  str_replace_all("limousin", "lim") %>% 
  str_replace_all("limo", "lim") %>%              # replacing "limo" with "lim"
  str_replace_all(".*\\s+lim|lim flex", "lim") %>%# combining all different types of limousins 
  str_replace_all("gelbvieh", "gel") %>% 
  str_replace_all("hereford", "here") %>% 
  str_replace_all("charolais", "char") %>% 
  str_replace_all("brahman", "brah") %>%
  str_remove_all("wf-|bwf-|rwf-") %>%             # removing face types from the data when there is other info
  str_remove_all("\\s*&") %>%                     # removing ampersands
  str_replace_all("\\s?blk\\s?", "black") %>% 
  str_replace_all("red black", "black red") %>%   # reordering "red and black" to "black and red"
  str_replace_all(".*-.*", "mix") %>%             # replaces any named cross-breed with "mix"
  str_replace_all("balancer", "mix")              # replaces "balancer" with "mix", since it's a hybrid


# Ok, I am making the executive decision to remove the color on all cattle that have more information
# I am also going to recode "here" and "brah" as "red", since they have a very small sample size (and are very heavy)
market$Type <- market %>% 
  select(Type) %>% 
  unlist() %>% 
  str_remove_all("\\s+wf|\\s+bwf|\\s+rwf") %>%    # removing face types at after the name
  str_replace_all("^wf$", "bwf") %>%              # recoding "wf" as "bwf" because it performs similarly and has very few observations
  str_remove_all("wf\\s+|bwf\\s+|rwf\\s+") %>%    # removing face types before the name
  str_replace_all("here|brah", "red") %>%         # replacing "here" and "brah" with "red"
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


#combining all colors into a new category--"clr"
market$Type <- market %>% 
  select(Type) %>% 
  unlist() %>% 
  str_replace_all("black red|black|red", "clr") %>%   # "black red," "black," and "red" are all 
  str_replace_all("bwf|rwf|wf", "face") %>%
  str_replace_all("face", "clr")                  # remove this line if you want to stop pooling the face only group with the color only group

# We now have six categories in the "Type" variable

# heifers are between one and two years old and have has not given birth.
# Bred heifers are pregnant and have not given birth yet.
# Cows have been mothers already.
# a heifer is a female, steer is a castrated male
# steers are bulls that have been castrated
# the x means nothing

# the hyphen means a cross breed (a mix)
# angus cross limousine
# saler is a breed of cattle
# sims stands for sims angus, a type of angus

# bwf stands for black wwhite face (60 days of no
# red white face
# flex limousine is a lim-flex
# white face is wf
# balancer is a combination of gelbvieh and angus
# brah is brahman cattle

# These are sales data