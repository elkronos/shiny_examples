# Hosted: https://elkronos.shinyapps.io/bar_time/

# Library
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)
library(zoo)
library(lubridate)
library(readr)

# Define the 'bar_time' function
bar_time <- function(data, date_col, y_col, group_by = "day", window_size = 7, bar_stat = "sum", line_stat = "mean") {
  
  if (!is.character(date_col) || !is.character(y_col) || !is.character(group_by)) {
    stop("The date_col, y_col, and group_by arguments must be character.")
  }
  
  if (!is.numeric(window_size)) {
    stop("The window_size argument must be numeric.")
  }
  
  if (!bar_stat %in% c("sum", "mean", "median")) {
    stop("The bar_stat argument must be one of 'sum', 'mean', or 'median'.")
  }
  
  if (!line_stat %in% c("mean", "median", "max", "min")) {
    stop("The line_stat argument must be one of 'mean', 'median', 'max', or 'min'.")
  }
  
  data <- data %>%
    mutate(date = as.Date(!!sym(date_col))) %>%
    mutate(y = as.numeric(!!sym(y_col))) %>%
    select(date, y)
  
  if (group_by == "month") {
    data <- data %>%
      mutate(date = as.Date(format(date, "%Y-%m-01")))
  } else if (group_by == "year") {
    data <- data %>%
      mutate(date = as.Date(format(date, "%Y-01-01")))
  } else if (group_by == "quarter") {
    data <- data %>%
      mutate(date = ymd(date)) %>%
      mutate(date = floor_date(date, unit = "quarter"))
  }
  
  data_summary <- data %>%
    group_by(date) %>%
    summarize(y_sum = sum(y, na.rm = TRUE),
              y_avg = mean(y, na.rm = TRUE),
              y_median = median(y, na.rm = TRUE))
  
  data_summary <- data_summary %>%
    arrange(date)
  
  if (bar_stat == "sum") {
    data_summary <- data_summary %>%
      mutate(bar_value = y_sum)
  } else if (bar_stat == "mean") {
    data_summary <- data_summary %>%
      mutate(bar_value = y_avg)
  } else if (bar_stat == "median") {
    data_summary <- data_summary %>%
      mutate(bar_value = y_median)
  }
  
  data_summary <- data_summary %>%
    mutate(moving_stat = switch(
      line_stat,
      mean = rollapply(bar_value, width = window_size, FUN = mean, align = "right", fill = NA),
      median = rollapply(bar_value, width = window_size, FUN = median, align = "right", fill = NA),
      max = rollapply(bar_value, width = window_size, FUN = max, align = "right", fill = NA),
      min = rollapply(bar_value, width = window_size, FUN = min, align = "right", fill = NA)
    ))
  
  date_format_string <- ifelse(group_by == "day", "%Y-%m-%d",
                               ifelse(group_by == "month", "%Y-%m",
                                      ifelse(group_by == "year", "%Y",
                                             ifelse(group_by == "quarter", "Q%q, %Y", ""))))
  
  date_break_interval <- ifelse(group_by == "day", "1 month",
                                ifelse(group_by == "month", "6 months",
                                       ifelse(group_by == "year", "1 year",
                                              ifelse(group_by == "quarter", "6 months", ""))))
  
  plot <- ggplot(data_summary, aes(x = date, y = bar_value)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
    geom_line(aes(y = moving_stat), color = "red", na.rm = TRUE, size = 1) +
    scale_x_date(labels = date_format(date_format_string), breaks = date_breaks(date_break_interval)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = bar_stat, title = paste("Time Series Bar Plot of", bar_stat, "with rolling", line_stat))
  
  return(plot)
}

# Example data
set.seed(123)
example_data <- data.frame(date = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by = "day"),
                           value = rnorm(4018, 10, 2))

# Define the function to determine grouping
determine_grouping <- function(date_range) {
  date_diff <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
  if (date_diff <= 30) {
    return("day")
  } else if (date_diff <= 180) {
    return("month")
  } else if (date_diff <= 365*2) {
    return("quarter")
  } else {
    return("year")
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Bar Time Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = ".csv"),
      numericInput("window_size", "Window Size:", value = 7, min = 1),
      selectInput("bar_stat", "Bar Statistic:", choices = c("sum", "mean", "median"), selected = "sum"),
      selectInput("line_stat", "Line Statistic:", choices = c("mean", "median", "max", "min"), selected = "mean"),
      selectInput("group_by", "Group By:", choices = c("day", "month", "quarter", "year"), selected = "month")  # New UI element for group_by
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  data_reactive <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(example_data)
    }
    read.csv(inFile$datapath)
  })
  
  output$plot <- renderPlotly({
    
    data <- req(data_reactive())
    
    date_col <- "date"
    y_col <- "value"
    
    # Use the group_by input from the UI directly to set the group_by parameter
    group_by <- input$group_by
    
    plot <- bar_time(data, date_col, y_col, group_by, input$window_size, input$bar_stat, input$line_stat)
    
    plotly::plotly_build(plot)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
