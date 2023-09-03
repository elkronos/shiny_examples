library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)

plot_timeseries_fill <- function(data, time_col, series_cols, colors = NULL, fill_between = FALSE, alpha_val = 0.5, line_width = 2) {
  
  if (fill_between && length(series_cols) != 2) {
    stop("fill_between option requires exactly two series.")
  }
  
  data$top_color = ifelse(data[[series_cols[1]]] <= data[[series_cols[2]]], colors[1], colors[2])
  
  p <- ggplot(data, aes_string(x = time_col))
  
  if(fill_between) {
    p <- p + geom_segment(aes(xend = !!sym(time_col), y = !!sym(series_cols[1]), yend = !!sym(series_cols[2]), color = top_color), alpha = alpha_val, size = line_width/2, lineend = 'round')
  }
  
  for (i in seq_along(series_cols)) {
    p <- p + geom_line(aes_string(y = series_cols[i]), color = colors[i], size = line_width)
  }
  
  p <- p + theme_light() +
    theme(panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "black"))
  
  return(p)
}

# Sample data
sample_data <- data.frame(
  time = seq.Date(from = as.Date("2021-01-01"), by = "day", length.out = 730),
  series1 = c(rnorm(365, 10, 2), rnorm(365, 15, 2)),
  series2 = c(rnorm(365, 15, 2), rnorm(365, 10, 2))
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = ".csv"),
      checkboxInput("fill_between", "Fill Between", value = TRUE),
      sliderInput("alpha_val", "Alpha Value", min = 0, max = 1, value = 0.5, step = 0.01),
      sliderInput("line_width", "Line Width", min = 0.5, max = 5, value = 2, step = 0.1),
      selectInput("agg_func", "Aggregation", c("week", "month", "year"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("plot")),
        tabPanel("Data", verbatimTextOutput("data_info"))
      )
    )
  )
)

server <- function(input, output) {
  data_to_use <- reactive({
    if (is.null(input$file)) {
      return(sample_data)
    } else {
      df <- read.csv(input$file$datapath)
      colnames(df)[1] <- "time"
      return(df)
    }
  })
  
  output$data_info <- renderPrint({
    df <- data_to_use()
    list(head(df), str(df))
  })
  
  output$plot <- renderPlotly({
    df <- data_to_use()
    
    date_col <- "time"
    series_cols <- names(df)[sapply(df, is.numeric)]
    
    if (input$agg_func == "week") {
      df <- df %>% group_by(week = week(df[[date_col]])) %>% summarise(across(all_of(series_cols), mean, .groups = 'drop'))
      date_col <- "week"
    } else if (input$agg_func == "month") {
      df <- df %>% group_by(month = month(df[[date_col]])) %>% summarise(across(all_of(series_cols), mean, .groups = 'drop'))
      date_col <- "month"
    } else if (input$agg_func == "year") {
      df <- df %>% group_by(year = year(df[[date_col]])) %>% summarise(across(all_of(series_cols), mean, .groups = 'drop'))
      date_col <- "year"
    }
    
    p <- plot_timeseries_fill(df, date_col, series_cols, colors = c("blue", "red"), fill_between = input$fill_between, alpha_val = input$alpha_val, line_width = input$line_width)
    ggplotly(p)
  })
}


shinyApp(ui, server)
