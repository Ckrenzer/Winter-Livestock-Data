if(!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinycssloaders, shinyjs, dplyr, lubridate, readr, ggplot2, patchwork, plotly)


# Reading in the file and removing the URL column
# Fortunately, we only have to do this once
lajunta <- read_csv("https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/La%20Junta%20Market%20Reports.csv",
                    col_types = cols(Date = "D",
                                     Buyer = col_factor(),
                                     Quantity = col_double(),
                                     Type = col_factor(),
                                     Weight = col_double(),
                                     Price = col_double(),
                                     URL = col_character(),
                                     Reprod = col_factor())) %>% 
    dplyr::select(-URL) %>% 
    filter(!is.na(Reprod))

# This is meant to speed up the runtime instead of placing it in multiple reactive functions
outliers_removed <- lajunta %>% 
    filter(Price < 475)



# Helper functions--these may be moved to their own Rscripts,
# but keeping things in one file makes things simpler...

# Plots weight against price
plot_weight_vs_price <- function(df){
    df %>% 
        ggplot() +
        geom_point(mapping = aes(x = Weight, y = Price, color = Reprod)) +
        ggtitle("Price vs. Weight") +
        theme_dark()
    
}#end of plot_weight_vs_price()


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
        theme_dark()
    
    return(cattle_type + cattle_reprod)    
}#end of plot_counts()



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
}#end of plot_densities()









# Define UI for application that draws a histogram
ui <- navbarPage("Lajunta, CO Market Overview",
                 
                 # Initializing shinyjs
                 useShinyjs(),
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("Price Summary",
                          sidebarPanel(
                              #
                          ),
                          
                          # Put all outputs for this tab here
                          mainPanel(
                              plotOutput("lajunta") %>% withSpinner(color = "#0dc5c1")
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("Visuals",
                          sidebarPanel(
                              # Determines whether to show the Weight vs. Price graph
                              selectInput(
                                  inputId = "wt_vs_pr",
                                  label = "Would you like to see a plot of cattle weight against price?",
                                  choices = list("Hide" = "hide_wvp",
                                                 "Yes" = "plot_wvp",
                                                 "Yes, with a graph for each reproductive status" = "wvp_by_reprod",
                                                 "Yes, with a graph for each cattle type" = "wvp_by_type"),
                                  selected = "plot_wvp"
                              ),
                              
                              # Determines whether to remove extreme values
                              checkboxInput(
                                  inputId = "remove_outliers",
                                  label = "Remove extreme values?",
                                  value = TRUE
                              ),
                              
                              # Determines whether to show the raw count bar plots
                              checkboxInput(
                                  inputId = "counts",
                                  label = "Show the number of cattle in each category?",
                                  value = FALSE
                              ),
                              
                              # Determines whether to show the distribution plots
                              checkboxInput(
                                  inputId = "distributions",
                                  label = "Show density functions?",
                                  value = FALSE
                              )
                              
                          ),#end of sidebarPanel()
                          
                          # The four plots for this tab
                          mainPanel(
                              plotOutput("weight_vs_price_plot"),
                              plotOutput("raw_counts_plots"),
                              plotlyOutput("plotly_3d"),
                              plotOutput("distribution_plot", height = 1200)
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("Price Estimation",
                          sidebarPanel(
                              # ADD INPUTS HERE
                          ),
                          
                          mainPanel(
                              # ADD OUTPUTS HERE
                          )
                 )
)#end of navbarPage()






# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # 'Price Summary' Tab Output
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 'Visuals' Tab Output
    output$weight_vs_price_plot <- renderPlot({
        
        # The weight vs price object
        wvp <- NULL
        
        # If the user asks to remove outliers
        if(input$remove_outliers){
            
            # determines which type of plot to return
            if(input$wt_vs_pr == "plot_wvp"){
                wvp <- outliers_removed %>%  
                    plot_weight_vs_price()
                
            } else if(input$wt_vs_pr == "wvp_by_reprod"){
                wvp <- outliers_removed %>% 
                    plot_weight_vs_price() +
                    facet_wrap(~ Reprod)
                
            } else if(input$wt_vs_pr == "wvp_by_type"){
                wvp <- outliers_removed %>% 
                    plot_weight_vs_price() +
                    facet_wrap(~ Type)
                
            }#end of nested conditional
            
        } else {
            
            # determines which type of plot to return
            if(input$wt_vs_pr == "plot_wvp"){
                wvp <- lajunta %>%  
                    plot_weight_vs_price()
                
            } else if(input$wt_vs_pr == "wvp_by_reprod"){
                wvp <- lajunta %>% 
                    plot_weight_vs_price() +
                    facet_wrap(~ Reprod)
                
            } else if(input$wt_vs_pr == "wvp_by_type"){
                wvp <- lajunta %>% 
                    plot_weight_vs_price() +
                    facet_wrap(~ Type)
                
            }#end of nested conditional
        }#end of conditional
        
        return(wvp)
    })
    # Toggles the weight vs price graph--the toggle() function is difficult
    # to use here because of four possible values instead of two
    observeEvent(input$wt_vs_pr, {
        if(input$wt_vs_pr == "hide_wvp"){
            shinyjs::hide("weight_vs_price_plot")
        } else {
            shinyjs::show("weight_vs_price_plot")
        }
    })
    
    
    
    output$raw_counts_plots <- renderPlot({
        return(plot_counts(lajunta))
    })
    # Toggles the graphs containing the counts
    observeEvent(input$counts, {
        toggle("raw_counts_plots")
    })
    
    
    output$plotly_3d <- renderPlotly({
        plotly::plot_ly(x = outliers_removed  %>% 
                            pull(Weight),
                        y = outliers_removed  %>% 
                            pull(Price),
                        z = outliers_removed  %>% 
                            pull(Quantity),
                        color = outliers_removed  %>% 
                            pull(Reprod)) %>% 
            layout(title = "Livestock by Weight, Price, and Quantity", 
                   scene = list(
                       xaxis = list(title = "Weight"),
                       yaxis = list(title = "Price"),
                       zaxis = list(title = "Quantity")
                   )
            )
    })
    
    output$distribution_plot <- renderPlot({
        plot_densities(outliers_removed)
    })
    # Toggles the distribution graphs
    observeEvent(input$distributions, {
        toggle("distribution_plot")
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 'Price Estimation' Tab Output
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
