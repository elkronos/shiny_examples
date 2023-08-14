library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)
library(zoo)
library(lubridate)
library(readr)

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
      mutate(date = floor_date(date, unit = "quarter"))  # Adjusting date to the start of the quarter
  }
  
  data_summary <- data %>%
    group_by(date) %>%
    summarize(y_sum = sum(y, na.rm = TRUE),
              y_avg = mean(y, na.rm = TRUE),
              y_median = median(y, na.rm = TRUE))
  
  data_summary <- data_summary %>%
    arrange(date)
  
  moving_stat <- switch(
    line_stat,
    mean = rollapply(data_summary$y_avg, width = window_size, FUN = mean, align = "right", fill = NA),
    median = rollapply(data_summary$y_median, width = window_size, FUN = median, align = "right", fill = NA),
    max = rollapply(data_summary$y_avg, width = window_size, FUN = max, align = "right", fill = NA),
    min = rollapply(data_summary$y_sum, width = window_size, FUN = min, align = "right", fill = NA)
  )
  
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
    geom_line(aes(y = moving_stat), color = "red", na.rm = TRUE, linewidth = 1, position = "identity") +
    scale_x_date(labels = date_format(date_format_string), breaks = date_breaks(date_break_interval)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = bar_stat, title = paste("Time Series Bar Plot of", bar_stat, "with rolling", line_stat))
  
  return(plot)
}

# Example data
set.seed(123)
example_data <- data.frame(date = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by = "day"),
                           value = rnorm(4018, 10, 2))

# Function to determine the highest level of aggregation in the data
determine_grouping <- function(data) {
  date_values <- as.Date(data$date)
  if (length(unique(floor_date(date_values, "year"))) > 1) return("year")
  if (length(unique(floor_date(date_values, "quarter"))) > 1) return("quarter")
  if (length(unique(floor_date(date_values, "month"))) > 1) return("month")
  return("day")
}

ui <- fluidPage(
  navbarPage(
    "Time Series Bar Plot with Moving Line",
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose CSV File (Optional)", accept = ".csv"),
                 selectInput("date_col", "Date Column:", choices = names(example_data), selected = if ("date" %in% names(example_data)) "date" else names(example_data)[1]),
                 selectInput("y_col", "Value Column:", choices = names(example_data), selected = if (!"date" %in% names(example_data)) names(example_data)[2] else setdiff(names(example_data), "date")),
                 selectInput("group_by", "Group By:", choices = c("day", "month", "quarter", "year"), selected = determine_grouping(example_data)),
                 numericInput("window_size", "Window Size:", value = 3, min = 1),
                 selectInput("bar_stat", "Bar Statistic:", choices = c("sum", "mean", "median"), selected = "mean"),
                 selectInput("line_stat", "Line Statistic:", choices = c("mean", "median", "max", "min"), selected = "max")
               ),
               mainPanel(
                 plotlyOutput("plot")
               )
             )),
    tabPanel("Help", 
             h4("Required Data Structure"),
             verbatimTextOutput("help_data"))
  )
)

server <- function(input, output, session) {
  data <- reactive({
    file <- input$file
    if (is.null(file)) {
      return(data.frame(date = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by = "day"),
                        value = rnorm(4018, 10, 2)))
    }
    read_data <- read.csv(file$datapath)
    read_data$date <- as.Date(read_data$date)
    return(read_data)
  })
  
  observe({
    updateSelectInput(session, "date_col", choices = c("date", names(data())[names(data()) != "date"]))
    updateSelectInput(session, "y_col", choices = names(data())[names(data()) != "date"])
    updateSelectInput(session, "group_by", selected = determine_grouping(data()))
  })
  
  output$plot <- renderPlotly({
    ggplotly(bar_time(data(), input$date_col, input$y_col, input$group_by, input$window_size, input$bar_stat, input$line_stat))
  })
  
  output$help_data <- renderPrint({
    cat("Head of the data:\n")
    print(head(data()))
    cat("\nStructure of the data:\n")
    print(str(data()))
  })
}

shinyApp(ui, server)