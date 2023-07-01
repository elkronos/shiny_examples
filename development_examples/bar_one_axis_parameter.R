library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  selectInput("variable", "Variable:",
              c("Cylinder" = "cyl",
                "Carb" = "carb")),
  plotlyOutput("barplot")
)

# Define server logic
server <- function(input, output) {
  output$barplot <- renderPlotly({
    # Get the selected variable
    variable <- input$variable
    
    # Calculate mean horsepower by selected variable
    means <- tapply(mtcars$hp, mtcars[[variable]], mean)
    
    # Create a bar plot
    p <- plot_ly(x = names(means), y = means, type = "bar")
    
    p
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
