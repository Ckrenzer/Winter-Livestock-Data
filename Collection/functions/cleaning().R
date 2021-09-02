# This function is one of those functions that works--as far as you can tell--but inflicts
# extreme pain when trying to debug or understand it... I've come a long way since December 2020.
# This is an artifact of a simpler time when best practices were merely a suggestion.

# Creates the 'Reprod' column and simplifies the 'Type' column down to 8 categories
cleaning <- function(lajunta){

  # Reproductive types ------------------------------------------------------------------
  # Making a column assigning cattle reproductive status
  # (removing values with 's' as the last letter before this step)
  lajunta <- lajunta %>% 
    mutate(Type = str_remove_all(Type, "s$")) %>% 
    mutate(Reprod = str_extract(Type, "hfr$|str$|bull$|cow$|heifer$|steer$|pair$|bow$|hrf$|hr$"),
           .before = 7)
  
  lajunta <- lajunta %>% 
    mutate(Reprod = Reprod %>% 
             str_replace_all("pair", "cow") %>%   #replacing 'pair' with cow. It seemed reasonable...
             str_replace_all("bow", "cow") %>%    #replacing a typo with the intended value
             str_replace_all("hrf|hr", "hfr") %>% #replacing typos with the intended values
             str_replace_all("heifer", "hfr") %>% #shortening name
             str_replace_all("steer", "str"))     #shortening name
  
  
  # Categorizing cattle -----------------------------------------------------------------
  # Removing the plural of the type (Ex. "black cows" becomes "black cow")
  lajunta$Type <- str_remove(lajunta$Type, "s$")
  
  
  # The bulk of the edits
  lajunta$Type <- lajunta %>% 
    dplyr::pull(Type) %>% 
    str_remove_all("\\.|,") %>%                     # removing punctuation
    str_remove_all("\\sx.*$|\\s[^\\s]*$") %>%  # removing " x hfr", " hfr", " x str", etc.
    str_replace_all("angus", "ang") %>% 
    str_remove_all("sim-") %>%                      # removing the sim- from sim-angus
    str_replace_all("sim\\s+ang", "ang") %>%        # removing "sim " from "sim angus"
    str_replace_all("\\s*sim", " ang") %>%          # replacing " sim" with " ang"
    str_replace_all("beefmaster|beefmstr", "bfmstr") %>%     # replacing "beefmaster" with "bfmstr"
    str_replace_all("shorthorn|shthrn", "sthrn") %>%# replacing "shorthorn" with "sthrn"
    str_replace_all("clr longhorn", "lnhrn") %>%    # replacing "longhorn" with "lnhrn" 
    str_replace_all("limousin", "lim") %>% 
    str_replace_all("limo", "lim") %>%              # replacing "limo" with "lim"
    str_replace_all(".*\\s+lim|lim flex", "lim") %>%# combining all different types of limousins 
    str_replace_all("gelbvieh|gelb|gelvieh", "gel") %>% 
    str_replace_all("hereford", "here") %>% 
    str_replace_all("charolais", "char") %>% 
    str_replace_all("brahman", "brah") %>%
    str_remove_all("wf-|bwf-|rwf-") %>%             # removing face types from the data when there is other info
    str_remove_all("\\s*&") %>%                     # removing ampersands
    str_replace_all("\\s?blk\\s?", "black") %>%     # replacing abbreviation "blk" with "black"
    str_replace_all("red black", "black red") %>%   # reordering "red and black" to "black and red"
    str_replace_all(".*-.*|mixed", "mix") %>%       # replaces any named cross-breed with "mix"
    str_replace_all("ang char|char ang", "mix") %>% # replaces "char ang" with "mix" explicitly 
    str_replace_all("balancer|bal|bclr", "mix") %>% # replaces "balancer" with "mix", since it's a hybrid
    str_replace_all("santa gert", "mix") %>%        # replaces "santa gert" with "mix", since it's a hybrid
    str_replace_all("stabilizer|stab", "mix") %>%   # replaces "stabilizer" with "mix", since it's a hybrid 
    str_replace_all("barzona", "mix") %>%           # replaces "barzona" with "mix", since it's a hybrid
    str_remove_all("\\s+wf|\\s+bwf|\\s+rwf") %>%    # removing face types at after the name
    str_replace_all("^wf$", "bwf") %>%              # recoding "wf" as "bwf" because it performs similarly and has few observations
    str_remove_all("wf\\s+|bwf\\s+|rwf\\s+") %>%    # removing face types before the name
    #str_replace_all("here|brah", "red") %>%         # replacing "here" and "brah" with "red" due to few observations and similar weight (Since the sample size increased, these cattle can now be kept)
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
  
  
  # Combining all colors into a new category--"clr" (optional)
  lajunta$Type <- lajunta %>% 
    dplyr::pull(Type) %>% 
    str_replace_all("black red|black|balck|red|gray|grey|roan|brown|brwn|bwn", "clr") %>%   # "black red," "black," and "red" are all colors
    str_replace_all("bwf|rwf|wf|spot", "face") %>%
    str_replace_all("face", "clr")                  # remove this line if you want to stop pooling the face group with the color group
  
  
  
  
  # Miscellaneous Corrections -----------------------------------------------------------
  # This correction of the Type column is done after finding errors explicitly. I am 'hard-coding' in the correct values. I checked the distinct Type values after running the above lines in this section, then fixed the remaining values to fit into a few categories. An admittedly lazy way of doing things, but the file is small--and I'm not a software engineer!
  #Note: many of these operations assume that cross-breeds have already been assigned "mix"
  lajunta$Type <- lajunta %>%
    dplyr::pull(Type) %>% 
    str_remove_all("x ") %>%                  # removing stray 'x' characters
    str_replace_all("weaned", "clr") %>%      # Weaned cattle isn't a type, so it gets pooled in with "clr"
    str_replace_all("sal", "clr") %>%         # I don't know what type of cattle "sal" is, so it gets pooled in with "clr"
    str_replace_all("lh", "clr") %>%          # I don't know what type of cattle "lh" is, so it gets pooled in with "clr"
    str_replace_all("brnd", "clr") %>%        # I don't know what type of cattle "brnd" is, so it gets pooled in with "clr"
    str_replace_all("clrrod", "clr") %>%      # I don't know what type of cattle "clrrod" is (before "clr" was appended to the name in an earlier operation), so it gets pooled in with "clr"
    str_replace_all("mis", "clr") %>%         # I don't know what type of cattle "mis" is, so it gets pooled in with "clr"
    str_replace_all(".*char$", "char") %>%    # removing all characters preceding "char"
    str_replace_all("chr|char.*", "char") %>% # replacing "chr" with "char"
    str_replace_all(".*ang$", "ang") %>%      # removing all characters preceding "ang"
    str_replace_all("angmental", "ang") %>%   # replacing "angmental" with "ang"
    str_replace_all(".*maine$", "maine") %>%  # removing all characters preceding "maine"
    str_replace_all("maine", "me") %>%        # replacing "maine" with "me"
    str_remove_all("irish") %>%               # removing the word "irish" anywhere it appears (usually accompanying 'red')
    str_replace_all(".*clr$", "clr") %>%      # removing all characters preceding "clr" (this can only be done after the cases for more specific types [like char] have been addressed)
    str_replace_all("hererord", "here") %>%  # Fixing Errors
    str_replace_all("ang strs", "ang") %>% 
    str_replace_all("mixck", "mix")          # End of Fixing Errors
  
  
  # After doing more testing it would appear that the remaining breeds do not have a large enough sample to be worth using. All cattle types not in the below list will be assigned to "clr":
  types <- c("clr", "ang", "mix", "char", "lim", "gel", "here", "mot")
  
  
  lajunta$Type[which(!lajunta$Type %in% types)] <- "clr"
  # We now have eight categories in the "Type" variable
  
  
  return(lajunta)
}