library(shiny)
library(plotly)

ui <- fluidPage(
  selectInput("group_var", "Select group variable:",
              c("Group" = "group", "Selection" = "selection")),
  plotlyOutput("dumbbell_plot")
)

server <- function(input, output) {
  data <- data.frame(
    group = factor(c("high", "medium", "low"), levels = c("high", "medium", "low")),
    selection = factor(c("Selected", "Unselected", "Alternate"), levels = c("Selected", "Alternate", "Unselected")),
    start = as.Date(c("2022-01-01", "2022-06-01", "2022-12-01")),
    end = as.Date(c("2023-01-01", "2023-06-01", "2023-12-01"))
  )
  
  output$dumbbell_plot <- renderPlotly({
    plot_ly(data) %>%
      add_trace(y = data[[input$group_var]], x = data$start, type = 'scatter', mode = 'markers', name = 'start', marker = list(size = 10, symbol = "circle")) %>%
      add_trace(y = data[[input$group_var]], x = data$end, type = 'scatter', mode = 'markers', name = 'end', marker = list(color = 'red', size = 10, symbol = "circle")) %>%
      add_segments(y = data[[input$group_var]], yend = data[[input$group_var]], x = data$start, xend = data$end, name = 'segment', line = list(color = 'gray', width = 2)) %>%
      layout(yaxis = list(title = input$group_var, categoryorder = "category ascending"), xaxis = list(title = "Date", type = "date"))
  })
}

shinyApp(ui, server)
