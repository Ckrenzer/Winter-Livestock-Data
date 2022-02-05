# Plots the distribution functions of the variables
plot_densities <- function(df){
  
  # The plots to be built
  weight1 <- NULL
  weight2 <- NULL
  weights <- NULL
  
  price1 <- NULL
  price2 <- NULL
  prices <- NULL
  
  quantity1 <- NULL
  quantity2 <- NULL
  quantities <- NULL
  
  return_plot <- NULL
  
  
  
  
  weight1 <- df %>% 
    filter(Reprod %in% c("str", "hfr")) %>% 
    ggplot(mapping = aes(x = Weight)) +
    geom_histogram(mapping = aes(x = Weight, y = ..density..), fill = "orange", bins = 100) +
    geom_density(color = "blue", size = 1.25) +
    ggtitle("Weight") +
    xlab(NULL) +
    facet_wrap(~ Reprod) +
    theme_dark()
  
  weight2 <- df %>% 
    filter(!Reprod %in% c("str", "hfr"),
           Weight > 750) %>% 
    ggplot(mapping = aes(x = Weight)) +
    geom_histogram(mapping = aes(x = Weight, y = ..density..), fill = "orange", bins = 35) +
    geom_density(color = "blue", size = 1.25) +
    ggtitle(NULL) +
    xlab("Pounds") +
    facet_wrap(~ Reprod) +
    theme_dark()
  
  weights <- weight1 / weight2
  
  
  
  
  price1 <- df %>% 
    filter(Reprod %in% c("str", "hfr")) %>% 
    ggplot(mapping = aes(x = Price)) +
    geom_histogram(mapping = aes(x = Price, y = ..density..), fill = "orange", bins = 100) +
    geom_density(color = "blue", size = 1.25) +
    ggtitle("Price") +
    xlab(NULL) +
    facet_wrap(~ Reprod) +
    theme_dark()
  
  
  price2 <- df %>% 
    filter(!Reprod %in% c("str", "hfr"),
           Weight > 750) %>% 
    ggplot(mapping = aes(x = Price)) +
    geom_histogram(mapping = aes(x = Price, y = ..density..), fill = "orange", bins = 35) +
    geom_density(color = "blue", size = 1.25) +
    ggtitle(NULL) +
    xlab("Cents Per Pound") +
    facet_wrap(~ Reprod) +
    theme_dark()
  
  prices <- price1 / price2
  
  
  
  
  quantity1 <- df %>% 
    filter(Reprod %in% c("str", "hfr")) %>% 
    ggplot(mapping = aes(x = Quantity)) +
    geom_histogram(mapping = aes(x = Quantity, y = ..density..), fill = "orange", bins = 100) +
    geom_density(color = "blue", size = 1.25) +
    ggtitle("Quantity") +
    xlab(NULL) +
    facet_wrap(~ Reprod) +
    theme_dark()
  
  quantity2 <- df %>% 
    filter(Reprod %in% c("bull", "cow")) %>% 
    ggplot(mapping = aes(x = Quantity)) +
    geom_histogram(mapping = aes(x = Quantity, y = ..density..), fill = "orange", bins = 20) +
    geom_density(color = "blue", size = 1.25) +
    ggtitle(NULL) +
    xlab("Quantity Sold") +
    facet_wrap(~ Reprod) +
    theme_dark()
  
  quantities <- quantity1 / quantity2
  
  
  
  return_plot <- weights + prices + quantities + plot_annotation(title = "Density Functions", caption = "Probability density functions, found empirically. Can you tell which distributions these variables follow?")
  return(return_plot)
}