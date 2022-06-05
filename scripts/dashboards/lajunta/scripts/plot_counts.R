# Counts the number of observations in the data
plot_counts <- function(df){
  cattle_type <- df %>% 
    count(Type) %>% 
    arrange(desc(n)) %>% 
    ggplot() +
    geom_col(mapping = aes(x = reorder(Type, desc(n)),
                           y = n,
                           fill = Type),
             show.legend = FALSE) +
    ggtitle("Counts by Type") +
    xlab("Cattle Type") +
    ylab("Number of Observations in Data") +
    theme_dark()
  
  cattle_reprod <- df %>% 
    count(Reprod) %>% 
    arrange(desc(n)) %>% 
    ggplot() +
    geom_col(mapping = aes(x = reorder(Reprod, desc(n)),
                           y = n,
                           fill = Reprod),
             show.legend = FALSE) +
    ggtitle("Counts by Reproductive Status") +
    xlab("Reproducive Status") +
    ylab(NULL) +
    theme_dark()
  
  return(cattle_type + cattle_reprod)    
}