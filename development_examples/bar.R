library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  plotlyOutput("barplot")
)

# Define server logic
server <- function(input, output) {
  output$barplot <- renderPlotly({
    # Calculate mean horsepower by cylinder count
    means <- tapply(mtcars$hp, mtcars$cyl, mean)
    
    # Create a bar plot
    p <- plot_ly(x = names(means), y = means, type = "bar")
    
    p
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
