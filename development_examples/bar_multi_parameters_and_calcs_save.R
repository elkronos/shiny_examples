library(shiny)
library(plotly)
library(writexl)
library(grDevices)

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
  plotlyOutput("barplot"),
  downloadButton("download_data", "Download Data"),
  downloadButton("download_pdf", "Download PDF")
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
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$variable, "_", input$outcome, "_", input$aggregate, ".xlsx", sep = "")
    },
    content = function(file) {
      variable <- mtcars[[input$variable]]
      outcome <- mtcars[[input$outcome]]
      
      if(input$aggregate == "Mean") {
        aggregate_result <- tapply(outcome, variable, mean)
      } else {
        aggregate_result <- tapply(outcome, variable, sum)
      }
      
      write_xlsx(as.data.frame(aggregate_result), file)
    }
  )
  
  output$downloadPDF <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      pdf(file, height = 7, width = 10)
      print(output$barplot)
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
