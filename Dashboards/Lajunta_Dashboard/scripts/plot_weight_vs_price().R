# Plots weight against price
plot_weight_vs_price <- function(df){
  df %>% 
    ggplot() +
    geom_point(mapping = aes(x = Weight, y = Price, color = Reprod)) +
    ggtitle("Weight vs. Price") +
    ylab("Price (cents per pound)") +
    theme_dark()
}