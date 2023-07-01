library(shiny)
library(dygraphs)
library(xts)
library(tidyverse)
library(lubridate)
library(nycflights13)

# Define UI
ui <- fluidPage(
  titlePanel("NYC Flights Visualizer"),
  sidebarLayout(
    sidebarPanel(
      p("Select the range of data to display:"),
      dateRangeInput("date_range", label = NULL, start = as.Date("2013-01-01"), end = as.Date("2013-12-31")),
      p("Select the roll period:"),
      sliderInput("roll_period", label = NULL, min = 1, max = 30, value = 1),
      p("Select the fill alpha:"),
      sliderInput("fill_alpha", label = NULL, min = 0, max = 1, step = 0.1, value = 0.1)
    ),
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
)

# Define server
server <- function(input, output) {
  # Read the data
  data <- nycflights13::flights %>%
    select(datetime = time_hour, count = dep_delay) %>%
    filter(!is.na(count))
  
  # Convert time to date-time format
  data$datetime <- as.POSIXct(data$datetime)
  
  # Create xts object
  don <- xts(x = data$count, order.by = data$datetime)
  
  # Create dygraph
  output$dygraph <- renderDygraph({
    dygraph(don, height = 500) %>%
      dyRangeSelector(dateWindow = c(input$date_range[1], input$date_range[2])) %>%
      dyRoller(rollPeriod = input$roll_period) %>%
      dyOptions(
        fillGraph = TRUE,
        fillAlpha = input$fill_alpha,
        drawGrid = FALSE,
        colors = "#D8AE5A",
        labelsUTC = TRUE
      ) %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = FALSE
      )
  })
}

# Run the app
shinyApp(ui, server)
