library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("scatterPlot")
)

server <- function(input, output) {
  output$scatterPlot <- renderPlotly({
    plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", mode = "markers")
  })
}

shinyApp(ui, server)
