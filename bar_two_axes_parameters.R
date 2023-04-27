library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  selectInput("variable", "Variable:",
              c("Cylinder" = "cyl",
                "Carb" = "carb")),
  selectInput("outcome", "Outcome:",
              c("Horsepower" = "hp",
                "Miles per Gallon" = "mpg")),
  plotlyOutput("barplot")
)

# Define server logic
server <- function(input, output) {
  output$barplot <- renderPlotly({
    # Get the selected variable and outcome
    variable <- input$variable
    outcome <- input$outcome
    
    # Calculate mean outcome by selected variable
    means <- tapply(mtcars[[outcome]], mtcars[[variable]], mean)
    
    # Create a bar plot
    p <- plot_ly(x = names(means), y = means, type = "bar")
    
    p
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
