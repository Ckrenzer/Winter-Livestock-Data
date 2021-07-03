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
# Added in the order they are used in the server() function

# Price changes over time using the median to calculate the weekly average
price_change_over_time <- function(df){
    plot1 <- df %>%  
        filter(Reprod %in% c("str", "hfr")) %>% 
        group_by(Date, Reprod) %>% 
        summarize(Price = median(Price)) %>% 
        ggplot() +
        geom_line(mapping = aes(x = Date, y = Price), color = "orange") +
        facet_wrap(~Reprod) +
        ggtitle("Median Weekly Price") +
        theme_dark()
    
    
    plot2 <-  df %>%  
        filter(Reprod %in% c("bull", "cow")) %>% 
        group_by(Date, Reprod) %>% 
        summarize(Price = median(Price)) %>% 
        ggplot() +
        geom_line(mapping = aes(x = Date, y = Price), color = "orange") +
        facet_wrap(~Reprod) +
        theme_dark()
    
    # using patchwork's "/" operator
    plot1 / plot2
}#end of price_change_over_time()


# A simple moving average
simple_moving_average <- function(df, reprod_status, variable_name, n){
    weekly_avg <- df %>% 
        filter(Reprod == reprod_status) %>%  
        group_by(Date) %>% 
        summarize("mean_price" = mean({{variable_name}}))
    
    avg <- numeric(nrow(weekly_avg) - n)
    
    #pre-allocating space in a vector
    last_date_in_period <- character(nrow(weekly_avg) - n)
    
    for(i in n:nrow(weekly_avg)){
        avg[(i-n)] <- mean(weekly_avg$mean_price[(i-(n-1)):i])
        last_date_in_period[i-n] <- as.character(weekly_avg$Date[i])
        
    }
    last_date_in_period <- as.Date(last_date_in_period)
    
    sma_results <- data.frame(Date = last_date_in_period, Avg_Price = avg) %>% 
        ggplot() +
        geom_line(mapping = aes(x = Date, y = Avg_Price), color = "orange", size = 1) +
        xlab("Final Date in Period") +
        ylab("Average Price") +
        theme_dark()
    
    return(sma_results)
    
}#end of simple_moving_average()



















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
                              # Determines the date range to show in the data
                              dateRangeInput(inputId = "daterange",
                                             label = "Date range:",
                                             start  = "2016-01-05",
                                             end    = max(lajunta$Date),
                                             min    = "2016-01-05",
                                             max    = max(lajunta$Date),
                                             format = "yyyy-mm-dd",
                                             separator = " THROUGH "),
                              
                              # Determines the number of weeks to use in the sma calculation
                              sliderInput(inputId = "numweeks",
                                          label = "How many weeks do you want to use for the moving average?",
                                          min = 4,
                                          max = 52,
                                          step = 4,
                                          value = 4,
                                          post = "weeks"
                              ),
                              
                          ),#end of sidebarPanel()
                          
                          # Put all outputs for this tab here
                          mainPanel(
                              plotOutput("price_changes_over_time") %>% withSpinner(color = "#0dc5c1"),
                              plotOutput("moving_average"),
                              plotOutput("six_month_forecast")
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
    output$price_changes_over_time <- renderPlot({
        
        # Making sure the date goes between the user's input dates
        lajunta %>% 
            filter(Date >= input$daterange[1],
                   Date <= input$daterange[2]) %>% 
            price_change_over_time()
        
    })
    
    output$moving_average <- renderPlot({
        
        # Making sure the date goes between the user's input dates
        data <- lajunta %>% 
            filter(Date >= input$daterange[1],
                   Date <= input$daterange[2])
        
        
        
        # Building the plots
        steer_sma_plot <- data %>% 
            simple_moving_average(df = .,
                                  reprod_status = "str",
                                  variable_name = Price,
                                  n = input$numweeks) +
            ggtitle("Steer")
        
        heifer_sma_plot <- data %>% 
            simple_moving_average(df = .,
                                  reprod_status = "hfr",
                                  variable_name = Price,
                                  n = input$numweeks) +
            ggtitle("Heifer")
        
        cow_sma_plot <- data %>% 
            simple_moving_average(df = .,
                                  reprod_status = "cow",
                                  variable_name = Price,
                                  n = input$numweeks) +
            ggtitle("Cow")
        
        bull_sma_plot <- data %>% 
            simple_moving_average(df = .,
                                  reprod_status = "bull",
                                  variable_name = Price,
                                  n = input$numweeks) +
            ggtitle("Bull")
        
        
        # The graph to return
        steer_sma_plot + heifer_sma_plot + cow_sma_plot + bull_sma_plot + plot_annotation(title = paste0("The Simple Moving Average Using the Previous ",  input$numweeks, " Sales"))
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
