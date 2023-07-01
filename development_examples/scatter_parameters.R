library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Scatter Plot with ggplot2"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X-axis variable:", 
                  c("Sepal.Length" = "Sepal.Length", 
                    "Sepal.Width" = "Sepal.Width", 
                    "Petal.Length" = "Petal.Length", 
                    "Petal.Width" = "Petal.Width")),
      selectInput("y_var", "Y-axis variable:", 
                  c("Sepal.Length" = "Sepal.Length", 
                    "Sepal.Width" = "Sepal.Width", 
                    "Petal.Length" = "Petal.Length", 
                    "Petal.Width" = "Petal.Width"))
    ),
    
    mainPanel(
      plotOutput("scatter_plot")
    )
  )
)

server <- function(input, output) {
  
  scatter_plot <- reactive({
    ggplot(iris, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() + 
      ggtitle(paste("Scatter Plot of", input$x_var, "vs", input$y_var))
  })
  
  output$scatter_plot <- renderPlot({
    scatter_plot()
  })
}

shinyApp(ui, server)
