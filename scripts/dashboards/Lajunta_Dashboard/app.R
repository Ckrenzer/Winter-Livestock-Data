# PACKAGES ------------------------------------------------------------------------------
if(!library(shiny, logical.return = TRUE)) install.packages("shiny")
if(!library(shinycssloaders, logical.return = TRUE)) install.packages("shinycssloaders")
if(!library(shinyjs, logical.return = TRUE)) install.packages("shinyjs")

if(!library(dplyr, logical.return = TRUE)) install.packages("dplyr")
if(!library(magrittr, logical.return = TRUE)) install.packages("magrittr")
if(!library(lubridate, logical.return = TRUE)) install.packages("lubridate")
if(!library(clock, logical.return = TRUE)) install.packages("clock")
if(!library(readr, logical.return = TRUE)) install.packages("readr")
if(!library(datagovindia, logical.return = TRUE)) install.packages("datagovindia")

if(!library(ggplot2, logical.return = TRUE)) install.packages("ggplot2")
if(!library(patchwork, logical.return = TRUE)) install.packages("patchwork")
if(!library(plotly, logical.return = TRUE)) install.packages("plotly")

if(!library(forecast, logical.return = TRUE)) install.packages("forecast")
if(!library(tseries, logical.return = TRUE)) install.packages("tseries")

if(!library(tidymodels, logical.return = TRUE)) install.packages("tidymodels")
if(!library(rsample, logical.return = TRUE)) install.packages("rsample")
if(!library(parsnip, logical.return = TRUE)) install.packages("parsnip")
if(!library(randomForest, logical.return = TRUE)) install.packages("randomForest")




# SETUP ---------------------------------------------------------------------------------
repo_functions_path <- "https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/scripts/dashboards/Lajunta_Dashboard/scripts/"

# Data
source(paste0(repo_functions_path, "data.R"))

# If you prefer reading data from the web:
#model_results <- datagovindia::read_rds_from_github("https://github.com/Ckrenzer/Winter-Livestock-Data/raw/main/scripts/dashboards/Lajunta_Dashboard/scripts/saved_objects/La%20Junta%20lm%20and%20rf%20models.rds")
model_results <- readr::read_rds("scripts/saved_objects/La Junta lm and rf models.rds")


# Helper functions
# Sourced in the order they are used in the server() function
source(paste0(repo_functions_path, "price_change_over_time().R"))
source(paste0(repo_functions_path, "simple_moving_average().R"))
source(paste0(repo_functions_path, "arima_plot().R"))
source(paste0(repo_functions_path, "plot_weight_vs_price().R"))
source(paste0(repo_functions_path, "plot_counts().R"))
source(paste0(repo_functions_path, "plot_densities().R"))
source(paste0(repo_functions_path, "price_converter().R"))
source(paste0(repo_functions_path, "plot_rmse().R"))





# SHINY APP -----------------------------------------------------------------------------
ui <- navbarPage("Lajunta, CO Market Overview",
                 selected = "Visuals",
                 
                 # Initializing shinyjs
                 useShinyjs(),
                 
                 
                 
                 
                 
                 
                 # Price Summary Layout -------------------------------------------------
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
                                          value = c(100, 2800),
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
                              
                              
                              helpText("The date range allows you to choose dates to include for your historical price calculation. It also adjusts the data shown in the historical price data."),
                              
                              
                              # Allows the user to download the dataset
                              downloadButton("market_report", "Get data"),
                              
                              
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
                              h1("Historical Prices"),
                              p("The tool below calculates the mean price based on the reproductive status, weight, price, and date of sale (sales in the date range) for the cattle. The graphs under that contain raw prices over time, simple moving averages, and price forecasts using ARIMA models."),
                              verbatimTextOutput("price_summary"),
                              plotOutput("price_changes_over_time") %>% withSpinner(color = "#0dc5c1"),
                              plotOutput("moving_average"),
                              plotOutput("six_month_forecast", height = 1600)
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 # Visuals Panel Layout -------------------------------------------------
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
                              h1("Granular Information"),
                              p("My favorite graph, \"Weight vs. Price\"...It can be adjusted to show a couple different breakdowns. The 'cattle in each category' graphs show the number of times a given type of cattle was shown in the dataset (it is NOT a sum of the cattle quantities grouped by type). If you feel so inclined, the 3D graph is interactive and can be explored at your leisure. Finally, at the bottom are some Honest-to-God probability density functions. Did someone say lognormal?"),
                              plotOutput("weight_vs_price_plot") %>% withSpinner(color = "#0dc5c1"),
                              plotOutput("raw_counts_plots"),
                              plotlyOutput("plotly_3d"),
                              plotOutput("distribution_plot", height = 1200)
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 # Price Estimation Panel Layout ----------------------------------------
                 tabPanel("Price Estimation",
                          sidebarPanel(
                              # Chooses which reproductive status price to predict
                              selectInput("reprod_model",
                                          "What reproductive status are you interested in?",
                                          choices = list("Steer" = "str",
                                                         "Heifer" = "hfr",
                                                         "Cow" = "cow",
                                                         "Bull" = "bull"),
                                          selected = "str"),
                              
                              
                              helpText("Please note that the models become less accurate the further into the future you try to predict."),
                              
                              helpText("The following fields accept keyboard input."),
                              
                              
                              # Determines the date the user wants to sell for input to predict()
                              dateInput(inputId = "sale_date",
                                        label = "When do you plan to sell?",
                                        value = Sys.Date(),
                                        min = Sys.Date(),
                                        max = NULL,
                                        format = "yyyy-mm-dd"),
                              
                              # Determines the number of cattle to sell for input to predict()
                              numericInput(inputId = "sale_quantity",
                                           label = "How many animals do you plan to sell?",
                                           min = 1,
                                           max = Inf,
                                           value = 1,
                                           step = 10
                              ),
                              
                              # Determines the number of cattle to sell for input to predict()
                              numericInput(inputId = "sale_weight",
                                           label = "What is the average weight of the livestock in this category?",
                                           min = 100,
                                           max = 3000,
                                           value = 750,
                                           step = 25
                              )
                          ),
                          
                          mainPanel(
                              h1("Modeling"),
                              p("This section creates models using the data and allow you to predict what the price will be at a future sale."),
                              strong("Instructions"),
                              p("Provide inputs to the sidebar on the left-hand side, and the models will return the predicted price."),
                              p("You will want to use the performance metrics provided in the graphs below to judge the accuracy of the model. Accurate predictions are those where the line of best fit comes close to the dotted line. If the prediction the model gave out does not come close to the dotted line, do not trust the result."),
                              strong("Use these models with caution--I am a data scientist, not a fortune-teller."),
                              verbatimTextOutput("lm_price_estimation"),
                              verbatimTextOutput("rf_price_estimation"),
                              plotOutput("rmse") %>% withSpinner(color = "#0dc5c1")
                          )
                 )
)#end of navbarPage()







server <- function(input, output) {
    
    # Server setup ----------------------------------------------------------------------
    # The dataset filtered down by the input date range,
    # after removing probable outliers (price above 500)
    date_filtered_data <- reactive({
        lajunta %>% 
            filter(Price < 300,
                   Date >= input$daterange[1],
                   Date <= input$daterange[2])
    })
    
    
    # The dataset filtered by date, reproductive statues, and weight
    price_summary_data <- reactive({
        date_filtered_data() %>% 
            filter(Reprod == input$reprod,
                   Weight >= input$weight_range[1],
                   Weight <= input$weight_range[2])
    })
    
    
    
    
    
    
    # 'Price Summary' Tab Output --------------------------------------------------------
    output$market_report <- downloadHandler(
        filename = "La Junta Market Reports.csv",
        content = function(file){
            write_csv(x = lajunta_full, file = file, col_names = TRUE)
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
            input$reprod == "hfr" ~ "heifers",
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
        steer_forecast <- arima_plot(df = outliers_removed, reprod = "str", reprod_fullname = "Steer", lags = 1, diff_deg = 2, ma_term = 2)
        heifer_forecast <- arima_plot(df = outliers_removed, reprod = "hfr", reprod_fullname = "Heifer", lags = 1, diff_deg = 1, ma_term = 2)
        cow_forecast <- arima_plot(df = outliers_removed, reprod = "cow", reprod_fullname = "Cow", lags = 1, diff_deg = 1, ma_term = 2)
        bull_forecast <- arima_plot(df = outliers_removed, reprod = "bull", reprod_fullname = "Bull", lags = 1, diff_deg = 1, ma_term = 2)
        
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 'Visuals' Tab Output --------------------------------------------------------------
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
                       xaxis = list(title = "Weight (x)"),
                       yaxis = list(title = "Price (y)"),
                       zaxis = list(title = "Quantity (z)")
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 'Price Estimation' Tab Output -----------------------------------------------------
    # Calculates the price to show in the prediction
    price_calculation <- reactive({
        
        # Linear regression price estimates
        steer_lm_price <- price_converter(model = model_results[["steer"]][["lm_fit"]], 
                                          date = input$sale_date, 
                                          weight = input$sale_weight, 
                                          quantity = input$sale_quantity, 
                                          reprod = input$reprod_model)
        
        heifer_lm_price <- price_converter(model = model_results[["heifer"]][["lm_fit"]], 
                                           date = input$sale_date, 
                                           weight = input$sale_weight, 
                                           quantity = input$sale_quantity, 
                                           reprod = input$reprod_model)
        
        cow_lm_price <- price_converter(model = model_results[["cow"]][["lm_fit"]], 
                                        date = input$sale_date, 
                                        weight = input$sale_weight, 
                                        quantity = input$sale_quantity, 
                                        reprod = input$reprod_model)
        
        bull_lm_price <- price_converter(model = model_results[["bull"]][["lm_fit"]], 
                                         date = input$sale_date, 
                                         weight = input$sale_weight, 
                                         quantity = input$sale_quantity, 
                                         reprod = input$reprod_model)
        
        # Random forest price estimates
        steer_rf_price <- price_converter(model = model_results[["steer"]][["rf_fit"]], 
                                          date = input$sale_date, 
                                          weight = input$sale_weight, 
                                          quantity = input$sale_quantity, 
                                          reprod = input$reprod_model)
        
        heifer_rf_price <- price_converter(model = model_results[["heifer"]][["rf_fit"]], 
                                           date = input$sale_date, 
                                           weight = input$sale_weight, 
                                           quantity = input$sale_quantity, 
                                           reprod = input$reprod_model)
        
        cow_rf_price <- price_converter(model = model_results[["cow"]][["rf_fit"]], 
                                        date = input$sale_date, 
                                        weight = input$sale_weight, 
                                        quantity = input$sale_quantity, 
                                        reprod = input$reprod_model)
        
        bull_rf_price <- price_converter(model = model_results[["bull"]][["rf_fit"]], 
                                         date = input$sale_date, 
                                         weight = input$sale_weight, 
                                         quantity = input$sale_quantity, 
                                         reprod = input$reprod_model)
        
        # Adding all prices to a named list
        all_prices <- list(steer_lm_price = steer_lm_price,
                           steer_rf_price = steer_rf_price,
                           heifer_lm_price = heifer_lm_price,
                           heifer_rf_price = heifer_rf_price,
                           cow_lm_price = cow_lm_price,
                           cow_rf_price = cow_rf_price,
                           bull_lm_price = bull_lm_price,
                           bull_rf_price = bull_rf_price)
        return(all_prices)
    })
    
    
    
    # Shows the predicted price using a linear regression
    output$lm_price_estimation <- renderPrint({
        
        price_category <- case_when(
            input$reprod_model == "str" ~ "steer_lm_price",
            input$reprod_model == "hfr" ~ "heifer_lm_price",
            input$reprod_model == "cow" ~ "cow_lm_price",
            TRUE ~ "bull_lm_price"
        )
        
        # Extracting the predicted price based on the inputs
        price <- price_calculation()[[price_category]]
        
        
        cat("The estimated price using a linear model is $", price, " per pound.", sep = "")
        
    })
    
    # Shows the predicted price using a random forest regression
    output$rf_price_estimation <- renderPrint({
        
        price_category <- case_when(
            input$reprod_model == "str" ~ "steer_rf_price",
            input$reprod_model == "hfr" ~ "heifer_rf_price",
            input$reprod_model == "cow" ~ "cow_rf_price",
            TRUE ~ "bull_rf_price"
        )
        
        # Extracting the predicted price based on the inputs
        price <- price_calculation()[[price_category]]
        
        
        cat("The estimated price using a random forest model is $", price, " per pound.", sep = "")
        
    })
    
    # Shows the RMSE on the testing set
    output$rmse <- renderPlot({
        
        # Stores the rmse plots into variables
        steer_rmse_facet <- plot_rmse(trained_models = list(lm_fit = model_results[["steer"]][["lm_fit"]],
                                                            rf_fit = model_results[["steer"]][["rf_fit"]]),
                                      test_df = model_results[["steer"]][["test"]])
        heifer_rmse_facet <- plot_rmse(trained_models = list(lm_fit = model_results[["heifer"]][["lm_fit"]],
                                                             rf_fit = model_results[["heifer"]][["rf_fit"]]),
                                       test_df = model_results[["heifer"]][["test"]])
        cow_rmse_facet <- plot_rmse(trained_models = list(lm_fit = model_results[["cow"]][["lm_fit"]],
                                                          rf_fit = model_results[["cow"]][["rf_fit"]]),
                                    test_df = model_results[["cow"]][["test"]])
        bull_rmse_facet <- plot_rmse(trained_models = list(lm_fit = model_results[["bull"]][["lm_fit"]],
                                                           rf_fit = model_results[["bull"]][["rf_fit"]]),
                                     test_df = model_results[["bull"]][["test"]])
        
        # Decides which plot to show
        if(input$reprod_model == "str"){
            return(steer_rmse_facet)
        } else if(input$reprod_model == "hfr"){
            return(heifer_rmse_facet)
        } else if(input$reprod_model == "cow"){
            return(cow_rmse_facet)
        } else {
            return(bull_rmse_facet) 
        }#end of conditional
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
