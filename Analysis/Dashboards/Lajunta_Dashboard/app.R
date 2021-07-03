if(!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinycssloaders, shinyjs, dplyr, magrittr, lubridate, clock, readr, ggplot2, patchwork, plotly, forecast, tseries)


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
        geom_line(mapping = aes(x = Date, y = Avg_Price), color = "green", size = 1) +
        xlab("Final Date in Period") +
        ylab("Average Price") +
        theme_dark()
    
    return(sma_results)
    
}#end of simple_moving_average()


# Plots the results of an arima model forecasting 6mo into the future
arima_plot <- function(df, reprod, reprod_fullname, lags, diff_deg, ma_term){
    
    
    # A date vector for forecasted dates
    dates <- as.Date(character(26))
    for(week_num in 1:26){
        dates[week_num] <- clock::add_weeks(df$Date[df$Date == max(df$Date)], week_num)
    }#end of for loop
    
    
    avg_prices <- df %>% 
        filter(Reprod == reprod) %>% 
        group_by(Date) %>% 
        summarize("median_price" = median(Price)) %>% 
        mutate(
            "cleaned_price" = tsclean(ts(median_price)),
            "ma_6mo" = ma(cleaned_price, order = 26),
            "seas_adj" = c(
                rep(NA, 26),
                seasadj(stl(ts(na.omit(ma_6mo), frequency = 26), s.window = "periodic"))
            ), 
            "seas_adj_diff" = c(rep(NA, diff_deg),
                                diff(seas_adj, differences = diff_deg)
            )
        )
    
    
    # The steer model
    model_fit <- arima(avg_prices$seas_adj, order = c(lags, diff_deg, ma_term))
    
    # The predictions for the model, six months out.
    price_predictions <- forecast(model_fit, h = 26)
    
    # Adding in the date column for the six month forecast
    price_predictions <- as_tibble(price_predictions) %>% 
        mutate(Date = dates)
    
    # The six month forecast
    price_forecast <- ggplot() +
        geom_line(data = avg_prices, mapping = aes(x = Date, y = seas_adj), color = "orange", size = 1) +
        geom_ribbon(data = price_predictions, mapping = aes(x = Date, ymin = `Lo 95`, ymax = `Hi 95`),
                    fill = "red") +
        geom_ribbon(data = price_predictions, mapping = aes(x = Date, ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue") +
        geom_line(data = price_predictions, mapping = aes(x = Date, y = `Point Forecast`), color = "blue", size = 1) +
        ggtitle(reprod_fullname) +
        xlab("Date") +
        ylab("Cents Per Pound") +
        theme_dark()
    
    return(price_forecast)
}#end of arima_plot()


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
        ylab(NULL) +
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
                 selected = "Visuals",
                 
                 # Initializing shinyjs
                 useShinyjs(),
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("Price Summary",
                          
                          
                          
                          sidebarPanel(
                              
                              
                              helpText("Supply a reproductive status, weight range, and date range (make the dates the same to check one sale) to find the average price for cattle meeting the specifications."),
                              
                              
                              # Determines the reproductive status for price calculations
                              selectInput("reprod",
                                          "What reproductive status are you interested in?",
                                          choices = list("Steer" = "str",
                                                         "Heifer" = "hfr",
                                                         "Cow" = "cow",
                                                         "Bull" = "bull"),
                                          selected = "str"),
                              
                              # Determines the weight range for price calculations
                              sliderInput("weight_range", "Weight Range:",
                                          min = 100,
                                          max = 2800,
                                          value = c(0, 2800),
                                          step = 10,
                                          post = " pounds",
                                          width = 600),
                              
                              # Determines the date range to show in the data
                              dateRangeInput(inputId = "daterange",
                                             label = "Date range:",
                                             start  = "2016-01-05",
                                             end    = max(lajunta$Date),
                                             min    = "2016-01-05",
                                             max    = max(lajunta$Date),
                                             format = "yyyy-mm-dd",
                                             separator = " THROUGH "),
                              
                              
                              helpText("The date range allows you to choose dates to include for your historical price calculation. It also adjusts the data shown in the historical price data. Further, the date range decides which data to include on the download button."),
                              
                              
                              # Allows the user to download the dataset
                              downloadButton("market_report", "Get data (in date range)"),
                              
                              # Determines whether to show the graphs
                              checkboxInput(
                                  inputId = "hide_price_graphs",
                                  label = "Hide graphs?",
                                  value = FALSE
                              ),
                              
                              # Determines the number of weeks to use in the sma calculation
                              sliderInput(inputId = "numweeks",
                                          label = "How many weeks do you want to use for the moving average?",
                                          min = 4,
                                          max = 52,
                                          step = 4,
                                          value = 4,
                                          post = " weeks"
                              ),
                              
                          ),#end of sidebarPanel()
                          
                          # Put all outputs for this tab here
                          mainPanel(
                              verbatimTextOutput("price_summary"),
                              plotOutput("price_changes_over_time") %>% withSpinner(color = "#0dc5c1"),
                              plotOutput("moving_average"),
                              plotOutput("six_month_forecast", height = 1600)
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
    
    # The dataset filtered down by the input date range
    date_filtered_data <- reactive({
        lajunta %>% 
            filter(Date >= input$daterange[1],
                   Date <= input$daterange[2])
    })
    
    
    # The dataset filtered by date, reproductive statues, and weight
    price_summary_data <- reactive({
        date_filtered_data() %>% 
            filter(Reprod == input$reprod,
                   Weight >= input$weight_range[1],
                   Weight <= input$weight_range[2])
    })
    
    
    
    
    
    
    # 'Price Summary' Tab Output
    output$market_report <- downloadHandler(
        filename = "La Junta Market Reports.csv",
        content = function(file){
            write_csv(x = date_filtered_data(), file = file, col_names = TRUE)
        }
    )
    
    
    output$price_summary <- renderPrint({
        price <- price_summary_data() %>% 
            dplyr::pull(Price) %>% 
            mean() %>% 
            divide_by(100) %>% 
            round(2)
        
        if(is.nan(price)){
            price <- "(NO DATA)"
        }
        
        reproductive_status <- case_when(
            input$reprod == "str" ~ "steers",
            input$reprod == "hrr" ~ "heifers",
            input$reprod == "cow" ~ "cows",
            input$reprod == "bull" ~ "bulls"
        )
        
        cat("Average price of ", reproductive_status, " matching your criteria: $", price, sep = "")
    })
    
    
    
    
    output$price_changes_over_time <- renderPlot({
        
        # Making sure the date goes between the user's input dates
        date_filtered_data() %>% 
            price_change_over_time()
        
    })
    
    output$moving_average <- renderPlot({
        
        # Building the plots
        steer_sma_plot <- date_filtered_data() %>% 
            simple_moving_average(df = .,
                                  reprod_status = "str",
                                  variable_name = Price,
                                  n = input$numweeks) +
            ggtitle("Steer")
        
        heifer_sma_plot <- date_filtered_data() %>% 
            simple_moving_average(df = .,
                                  reprod_status = "hfr",
                                  variable_name = Price,
                                  n = input$numweeks) +
            ggtitle("Heifer")
        
        cow_sma_plot <- date_filtered_data() %>% 
            simple_moving_average(df = .,
                                  reprod_status = "cow",
                                  variable_name = Price,
                                  n = input$numweeks) +
            ggtitle("Cow")
        
        bull_sma_plot <- date_filtered_data() %>% 
            simple_moving_average(df = .,
                                  reprod_status = "bull",
                                  variable_name = Price,
                                  n = input$numweeks) +
            ggtitle("Bull")
        
        
        # The graph to return
        steer_sma_plot + heifer_sma_plot + cow_sma_plot + bull_sma_plot + plot_annotation(title = paste0("Simple Moving Average Using the Previous ",  input$numweeks, " Sales"))
    })
    
    
    
    output$six_month_forecast <- renderPlot({
        # Making a bunch of forecasts
        steer_forecast <- arima_plot(df = lajunta, reprod = "str", reprod_fullname = "Steer", lags = 1, diff_deg = 2, ma_term = 2)
        heifer_forecast <- arima_plot(df = lajunta, reprod = "hfr", reprod_fullname = "Heifer", lags = 1, diff_deg = 1, ma_term = 2)
        cow_forecast <- arima_plot(df = lajunta, reprod = "cow", reprod_fullname = "Cow", lags = 1, diff_deg = 1, ma_term = 2)
        bull_forecast <- arima_plot(df = lajunta, reprod = "bull", reprod_fullname = "Bull", lags = 1, diff_deg = 1, ma_term = 2)
        
        return(steer_forecast / heifer_forecast / cow_forecast / bull_forecast + plot_annotation(title = "Seasonally Adjusted Six Month ARIMA Forecasts"))
    })
    
    # Toggles the graphs on and off
    observeEvent(input$hide_price_graphs, {
        if(input$hide_price_graphs){
            shinyjs::hide("price_changes_over_time")
            shinyjs::hide("moving_average")
            shinyjs::hide("six_month_forecast")
        } else {
            shinyjs::show("price_changes_over_time")
            shinyjs::show("moving_average")
            shinyjs::show("six_month_forecast")
        }
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
