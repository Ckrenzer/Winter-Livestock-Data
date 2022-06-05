# The file should have the data in the 
lajunta <- read_csv("data/ljmr.csv")


# Checking for Permalinks -----------------------------------------------------
# Finding market reports using the most recent market report instead of the permalink.
has_permalinks <- any(lajunta$URL == "http://www.winterlivestock.com/lajunta.php")

