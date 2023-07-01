library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("Scatter Plot"),
  selectInput("x_var", "X-Variable:", choices = c("None", names(iris)), selected = names(iris)[1]),
  selectInput("y_var", "Y-Variable:", choices = c("None", names(iris)), selected = names(iris)[2]),
  selectInput("color_var", "Color:", choices = c("None", names(iris)), selected = "None"),
  plotlyOutput("plot1")
)

server <- function(input, output) {
  scatter_plot <- reactive({
    x_col <- input$x_var
    y_col <- input$y_var
    color_col <- input$color_var
    if (x_col == "None" || y_col == "None") {
      return(NULL)
    }
    p <- ggplot(iris, aes_string(x = x_col, y = y_col)) + 
      geom_point()
    if (color_col != "None") {
      if (is.numeric(iris[,color_col])) {
        p <- p + aes_string(color = color_col) + scale_color_gradient()
      } else {
        p <- p + aes_string(color = color_col) + scale_color_discrete(guide = guide_legend(title = NULL))
      }
    }
    ggplotly(p)
  })
  output$plot1 <- renderPlotly({
    scatter_plot()
  })
}

shinyApp(ui, server)
