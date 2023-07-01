library(shiny)
library(forecast)
library(ggplot2)
library(plotly)
library(pracma)

detect_seasonality <- function(time_series, transformation = "boxcox", lambda = NULL, outlier_method = "hampel", k = 3, 
                               test_type = "auto", window = "periodic", detect_freq = TRUE, freq = 12, 
                               plot_color = "blue", plot_linetype = "solid", plot_title = "Seasonal Component",
                               plot_original = TRUE) {
  
  # Handle missing values
  time_series <- na.omit(time_series)
  
  # Handle non-stationary time series with transformation
  if (transformation == "boxcox") {
    if (is.null(lambda)) {
      lambda <- BoxCox.lambda(time_series)
    }
    time_series_transformed <- BoxCox(time_series, lambda)
  } else if (transformation == "log") {
    time_series_transformed <- log(time_series)
  } else {
    time_series_transformed <- time_series
  }
  
  # Detect and handle outliers
  if (outlier_method == "hampel") {
    time_series_transformed <- ts(hampel(time_series_transformed, k = k)$y, 
                                  start = start(time_series_transformed), frequency = frequency(time_series_transformed))
  }
  
  # Test for seasonality
  if(test_type == "auto"){
    ndiffs <- nsdiffs(time_series_transformed)
  }
  
  # If ndiffs > 0, there is seasonality
  if (ndiffs > 0) {
    
    # Detect frequency
    if(is.null(freq)){
      if(detect_freq == TRUE){
        freq <- findfrequency(time_series_transformed)
      } else {
        freq <- frequency(time_series_transformed)
      }
    }
    
    # Change frequency of time series
    time_series_transformed <- ts(time_series_transformed, frequency = freq)
    
    # Decompose the time series
    decomposed <- stl(time_series_transformed, s.window = window)
    
    # Extract the seasonal component
    seasonal_component <- decomposed$time.series[, "seasonal"]
    
    # Scale the original time series and the seasonal component for comparison
    original_time_series_scaled <- scale(time_series)
    seasonal_component_scaled <- scale(seasonal_component)
    
    # Convert the time series to data frame before plotting
    original_time_series_df <- data.frame(Time = as.numeric(time(original_time_series_scaled)), 
                                          Original = original_time_series_scaled)
    seasonal_component_df <- data.frame(Time = as.numeric(time(seasonal_component_scaled)), 
                                        Seasonality = seasonal_component_scaled)
    plot_data <- merge(original_time_series_df, seasonal_component_df, by = "Time", all = TRUE)
    
    # Create interactive plot
    p <- ggplot(plot_data, aes(x = Time)) +
      geom_line(aes(y = Seasonality), color = plot_color, linetype = plot_linetype)
    
    if (plot_original) {
      p <- p + geom_line(aes(y = Original), color = "black", linetype = "dashed")
    }
    
    p <- p + theme_minimal() +
      labs(y = "Scaled Value", title = plot_title)
    
    # Return the original and transformed time series, seasonal component, decomposed series, and plot
    return(list("original_time_series" = time_series, "transformed_time_series" = time_series_transformed, 
                "seasonal_component" = seasonal_component, "decomposed" = decomposed, "interactive_plot" = p))
  } else {
    print("No significant seasonality detected")
  }
}

ui <- fluidPage(
  titlePanel("Seasonality Detection"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a .csv file containing your time series data (optional)"),
      numericInput("freq_input", "Frequency", 12),
      selectInput("transformation_input", "Transformation", 
                  choices = list("boxcox" = "boxcox", "log" = "log", "none" = "none"), selected = "boxcox"),
      numericInput("lambda_input", "Lambda", 0),
      selectInput("outlier_method_input", "Outlier Method", 
                  choices = list("none" = "none", "hampel" = "hampel"), selected = "none"),
      numericInput("k_input", "K Value for Hampel", 3),
      selectInput("test_type_input", "Test Type", 
                  choices = list("auto" = "auto", "ch" = "ch", "ocsb" = "ocsb"), selected = "auto"),
      selectInput("window_input", "Window", 
                  choices = list("periodic" = "periodic"), selected = "periodic"),
      checkboxInput("detect_freq_input", "Detect Frequency", TRUE),
      textInput("plot_color_input", "Plot Color", "blue"),
      textInput("plot_linetype_input", "Plot Line Type", "solid"),
      textInput("plot_title_input", "Plot Title", "Seasonal Component"),
      checkboxInput("plot_original_input", "Plot Original", TRUE)
    ),
    
    mainPanel(
      plotlyOutput("plot_output")
    )
  )
)

server <- function(input, output) {
  
  output$plot_output <- renderPlotly({
    inFile <- input$file
    if (is.null(inFile)) {
      data("AirPassengers")
      time_series <- AirPassengers
    } else {
      data <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
      time_series <- ts(data[,1])
    }
    
    lambda <- if(input$lambda_input == 0) NULL else input$lambda_input
    
    results <- detect_seasonality(time_series, 
                                  transformation = input$transformation_input, 
                                  lambda = lambda, 
                                  outlier_method = input$outlier_method_input, 
                                  k = input$k_input, 
                                  test_type = "auto", 
                                  window = input$window_input, 
                                  detect_freq = input$detect_freq_input, 
                                  freq = input$freq_input, 
                                  plot_color = input$plot_color_input, 
                                  plot_linetype = input$plot_linetype_input, 
                                  plot_title = input$plot_title_input,
                                  plot_original = input$plot_original_input)
    
    ggplotly(results$interactive_plot)
  })
}

shinyApp(ui, server)
