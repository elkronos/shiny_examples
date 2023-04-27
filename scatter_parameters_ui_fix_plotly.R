library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("Scatter Plot with Plotly"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X-axis variable:", 
                  choices = colnames(iris)[sapply(iris, is.numeric)], 
                  selected = "Petal.Length"),
      selectInput("y_var", "Y-axis variable:", 
                  choices = colnames(iris)[sapply(iris, is.numeric)],
                  selected = "Petal.Width")
    ),
    
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

server <- function(input, output) {
  
  scatter_plot <- reactive({
    if (!is.numeric(iris[,input$x_var]) || !is.numeric(iris[,input$y_var])) {
      return(NULL)
    }
    ggplot(iris, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() + 
      ggtitle(paste("Scatter Plot of", input$x_var, "vs", input$y_var))
  })
  
  output$scatter_plot <- renderPlotly({
    gg <- ggplotly(scatter_plot())
    gg <- layout(gg, hoverlabel = list(bgcolor = "white", 
                                       font = list(family = "Arial", 
                                                   size = 12, color = "black")))
    gg
  })
}

shinyApp(ui, server)
