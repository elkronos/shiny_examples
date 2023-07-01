library(shiny)
library(plotly)

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$carb <- as.factor(mtcars$carb)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$vs <- as.factor(mtcars$vs)

ui <- fluidPage(
  selectInput("variable", "Variable:",
              choices = names(mtcars)[sapply(mtcars, is.factor)]),
  selectInput("outcome", "Outcome:",
              choices = names(mtcars)[sapply(mtcars, is.numeric)]),
  selectInput("aggregate", "Aggregate:",
              choices = c("Mean", "Sum")),
  plotlyOutput("barplot")
)

server <- function(input, output) {
  output$barplot <- renderPlotly({
    variable <- mtcars[[input$variable]]
    outcome <- mtcars[[input$outcome]]
    
    if(input$aggregate == "Mean") {
      aggregate_result <- tapply(outcome, variable, mean)
    } else {
      aggregate_result <- tapply(outcome, variable, sum)
    }
    
    p <- plot_ly(x = names(aggregate_result), y = aggregate_result, type = "bar")
    
    p
  })
}

shinyApp(ui = ui, server = server)
