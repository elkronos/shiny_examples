library(shiny)
library(plotly)

# Convert carb and cyl to factors
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$carb <- as.factor(mtcars$carb)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$vs <- as.factor(mtcars$vs)

# Define UI
ui <- fluidPage(
  selectInput("variable", "Variable:",
              choices = names(mtcars)[sapply(mtcars, is.factor)]),
  selectInput("outcome", "Outcome:",
              choices = names(mtcars)[sapply(mtcars, is.numeric)]),
  plotlyOutput("barplot")
)

# Define server logic
server <- function(input, output) {
  output$barplot <- renderPlotly({
    # Get the selected variable and outcome
    variable <- mtcars[[input$variable]]
    outcome <- mtcars[[input$outcome]]
    
    # Calculate mean outcome by selected variable
    means <- tapply(outcome, variable, mean)
    
    # Create a bar plot
    p <- plot_ly(x = names(means), y = means, type = "bar")
    
    p
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
