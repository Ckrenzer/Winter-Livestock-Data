# PACKAGES --------------------------------------
if(!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinycssloaders, shinyjs,
               dplyr, magrittr, lubridate, clock, readr,
               ggplot2, patchwork, plotly,
               forecast, tseries,
               tidymodels, randomForest)


# DATA SETUP ------------------------------------
# Reading in the file and removing the URL column
# Fortunately, we only have to do this once
lajunta <- readr::read_csv("https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/La%20Junta%20Market%20Reports.csv",
                           col_types = readr::cols(Date = "D",
                                                   Buyer = readr::col_factor(),
                                                   Quantity = readr::col_double(),
                                                   Type = readr::col_factor(),
                                                   Weight = readr::col_double(),
                                                   Price = readr::col_double(),
                                                   URL = readr::col_character(),
                                                   Reprod = readr::col_factor())) %>% 
    dplyr::select(-URL) %>% 
    dplyr::filter(!is.na(Reprod))

# This is meant to speed up the runtime instead of placing it in multiple reactive functions
outliers_removed <- lajunta %>% 
    filter(Price < 475)

# recodes bulls that act like steers on the market as steers
# and similarly for cows that act like heifers
model_data <- outliers_removed %>% 
    dplyr::select(-Buyer) %>% 
    mutate(Price = log(Price),
           Reprod = as.character(Reprod),
           Reprod = case_when(
               Reprod == "bull" && Weight < 1050 ~ "str",
               Reprod == "cow" && Price > 100 ~ "hfr",
               TRUE ~ Reprod),
           Reprod = as.factor(Reprod)
    )





# HELPER FUNCTIONS ------------------------------
# These may be moved to their own Rscripts,
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
        ggtitle("Weight vs. Price") +
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
}#end of plot_densities()

# Fits a model based on the reproductive status
fit_models <- function(df, reprod){
    # Setting the random number generator
    set.seed(2021)
    
    # Testing and training splits
    initial_split_data <- df %>% 
        dplyr::filter(Reprod == reprod) %>% 
        dplyr::select(-Reprod) %>% 
        initial_split(prob = 0.75)
    training_set <- training(initial_split_data)
    testing_set <- testing(initial_split_data)
    
    # Model specifications
    lm_mod <- linear_reg() %>% 
        set_mode("regression") %>% 
        set_engine("lm")
    rf_mod <- rand_forest() %>% 
        set_mode("regression") %>% 
        set_engine("randomForest")
    
    # Model fitting
    lm_fit <- lm_mod %>% 
        fit(Price ~ Date + Weight + Quantity, data = training_set)
    rf_fit <- rf_mod %>% 
        fit(Price ~ Date + Weight + Quantity, data = training_set)
    
    return(list(lm_fit = lm_fit, rf_fit = rf_fit, test = testing_set))
}#end of fit_models()

# Calculates the price to be passed to price_calculation({})
price_converter <- function(model, date, weight, quantity, reprod){
    # Extracting the predicted price based on the inputs
    price <- predict(object = model,
                     new_data = tibble(Date = date,
                                       Weight = weight,
                                       Quantity = quantity,
                                       Reprod = reprod)) %>% 
        pull(.pred)
    
    # Converting the price to dollars, from log(cents)
    price <- round((exp(price) / 100), 2)
    return(price)
}#end of price_converter()

# Creates the plots showing the root mean squared error
plot_rmse <- function(trained_models, test_df){
    # trained_models is a list containing the trained models
    
    # Calculating predicted values
    results <- test_df %>% 
        bind_cols(predict(trained_models[["lm_fit"]], test_df)) %>% 
        rename(lm_estimates = .pred) %>% 
        bind_cols(predict(trained_models[["rf_fit"]], test_df)) %>% 
        rename(rf_estimates = .pred)
    
    # Converting the price back to original units
    # and making a column that extracts the year, for plotting
    results <- results %>% 
        mutate(lm_estimates = exp(lm_estimates),
               rf_estimates = exp(rf_estimates),
               Price = exp(Price),
               Year = as.factor(get_year(Date)))
    
    # The MSE output graphs
    lm_rmse_plot <- results %>% 
        ggplot(mapping = aes(x = Price, y = lm_estimates)) +
        geom_abline(lty = 2, color = "gray50") +
        geom_point(aes(color = Year), size = 1.5, alpha = 0.3, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE) +
        xlab("Actual Price") +
        ylab("Predicted Price") +
        ggtitle("Linear Regression")
    
    rf_rmse_plot <- results %>% 
        ggplot(mapping = aes(x = Price, y = rf_estimates)) +
        geom_abline(lty = 2, color = "gray50") +
        geom_point(aes(color = Year), size = 1.5, alpha = 0.3, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE) +
        xlab("Actual Price") +
        ylab("Predicted Price") +
        ggtitle("Random Forest")
    
    return(lm_rmse_plot + rf_rmse_plot + plot_annotation(title = "Predicted vs. Actual Price, by Model")) 
}#end of plot_rmse()




# SHINY APP -------------------------------------

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
                              h1("Historical Prices"),
                              p("The tool below calculates the mean price based on the reproductive status, weight, price, and date of sale (sales in the date range) for the cattle. The graphs under that contain raw prices over time, simple moving averages, and price forecasts using ARIMA models."),
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
                              h1("Granular Information"),
                              p("My favorite graph, \"Weight vs. Price\"...It can be adjusted to show a couple different breakdowns. The 'cattle in each category' graphs show the number of times a given type of cattle was shown in the dataset (it is NOT a sum of the cattle quantities). If you feel so inclined, the 3D graph is interactive and can be explored at your leisure. Finally, at the bottom are some Honest-to-God probability density functions. Did someone say lognormal?"),
                              plotOutput("weight_vs_price_plot") %>% withSpinner(color = "#0dc5c1"),
                              plotOutput("raw_counts_plots"),
                              plotlyOutput("plotly_3d"),
                              plotOutput("distribution_plot", height = 1200)
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
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
                              h1("Modelling"),
                              p("This section creates models using the data and allow you to predict what the price will be at a future sale. It takes a long time to load due to the computationally-heavy tasks it performs on startup (~35 seconds with an Intel i5-8250U CPU)."),
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 'Price Estimation' Tab Output
    # Fits the models and then returns them in a named list
    model_fitting <- reactive({
        steer_models <- fit_models(df = model_data, reprod = "str")
        heifer_models <- fit_models(df = model_data, reprod = "hfr")
        cow_models <- fit_models(df = model_data, reprod = "cow")
        bull_models <- fit_models(df = model_data, reprod = "bull")
        
        model_list <- list(steer_models = steer_models,
                           heifer_models = heifer_models,
                           cow_models = cow_models,
                           bull_models = bull_models)
        
        return(model_list)
    })
    
    # Calculates the price to show in the prediction
    price_calculation <- reactive({
        
        # Linear regression price estimates
        steer_lm_price <- price_converter(model = model_fitting()[["steer_models"]][["lm_fit"]], 
                                          date = input$sale_date, 
                                          weight = input$sale_weight, 
                                          quantity = input$sale_quantity, 
                                          reprod = input$reprod_model)
        
        heifer_lm_price <- price_converter(model = model_fitting()[["heifer_models"]][["lm_fit"]], 
                                           date = input$sale_date, 
                                           weight = input$sale_weight, 
                                           quantity = input$sale_quantity, 
                                           reprod = input$reprod_model)
        
        cow_lm_price <- price_converter(model = model_fitting()[["cow_models"]][["lm_fit"]], 
                                        date = input$sale_date, 
                                        weight = input$sale_weight, 
                                        quantity = input$sale_quantity, 
                                        reprod = input$reprod_model)
        
        bull_lm_price <- price_converter(model = model_fitting()[["bull_models"]][["lm_fit"]], 
                                         date = input$sale_date, 
                                         weight = input$sale_weight, 
                                         quantity = input$sale_quantity, 
                                         reprod = input$reprod_model)
        
        # Random forest price estimates
        steer_rf_price <- price_converter(model = model_fitting()[["steer_models"]][["rf_fit"]], 
                                          date = input$sale_date, 
                                          weight = input$sale_weight, 
                                          quantity = input$sale_quantity, 
                                          reprod = input$reprod_model)
        
        heifer_rf_price <- price_converter(model = model_fitting()[["heifer_models"]][["rf_fit"]], 
                                           date = input$sale_date, 
                                           weight = input$sale_weight, 
                                           quantity = input$sale_quantity, 
                                           reprod = input$reprod_model)
        
        cow_rf_price <- price_converter(model = model_fitting()[["cow_models"]][["rf_fit"]], 
                                        date = input$sale_date, 
                                        weight = input$sale_weight, 
                                        quantity = input$sale_quantity, 
                                        reprod = input$reprod_model)
        
        bull_rf_price <- price_converter(model = model_fitting()[["bull_models"]][["rf_fit"]], 
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
        steer_rmse_facet <- plot_rmse(trained_models = list(lm_fit = model_fitting()$steer_models[["lm_fit"]],
                                                            rf_fit = model_fitting()$steer_models[["rf_fit"]]),
                                      test_df = model_fitting()$steer_models[["test"]])
        heifer_rmse_facet <- plot_rmse(trained_models = list(lm_fit = model_fitting()$heifer_models[["lm_fit"]],
                                                             rf_fit = model_fitting()$heifer_models[["rf_fit"]]),
                                       test_df = model_fitting()$heifer_models[["test"]])
        cow_rmse_facet <- plot_rmse(trained_models = list(lm_fit = model_fitting()$cow_models[["lm_fit"]],
                                                          rf_fit = model_fitting()$cow_models[["rf_fit"]]),
                                    test_df = model_fitting()$cow_models[["test"]])
        bull_rmse_facet <- plot_rmse(trained_models = list(lm_fit = model_fitting()$bull_models[["lm_fit"]],
                                                           rf_fit = model_fitting()$bull_models[["rf_fit"]]),
                                     test_df = model_fitting()$bull_models[["test"]])
        
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
