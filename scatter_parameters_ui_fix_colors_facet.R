library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X Variable", names(iris), selected = "Sepal.Length"),
      selectInput("y_var", "Y Variable", names(iris), selected = "Petal.Length"),
      selectInput("color_var", "Color Variable", c(names(iris), "None"), selected = "None"),
      selectInput("facet_var", "Facet Variable", c(names(iris)[sapply(iris, is.factor)], "None"), selected = "None")
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

server <- function(input, output) {
  scatter_plot <- reactive({
    if (!is.null(input$color_var) && input$color_var != "None") {
      if (is.numeric(iris[,input$color_var])) {
        ggplot(iris, aes_string(x=input$x_var, y=input$y_var, color=input$color_var)) +
          geom_point() +
          scale_color_gradient(low = "blue", high = "red")
      } else {
        ggplot(iris, aes_string(x=input$x_var, y=input$y_var, color=as.factor(iris[,input$color_var]))) +
          geom_point() +
          scale_color_discrete(guide = FALSE)
      }
    } else {
      ggplot(iris, aes_string(x=input$x_var, y=input$y_var)) +
        geom_point()
    }
  })
  
  facet_plot <- reactive({
    if (!is.null(input$facet_var) && input$facet_var != "None") {
      if (is.numeric(iris[,input$facet_var])) {
        return(NULL)
      } else {
        scatter_plot() + 
          facet_wrap(~ as.factor(iris[,input$facet_var]), ncol = 2)
      }
    } else {
      scatter_plot()
    }
  })
  
  output$plot1 <- renderPlot({
    facet_plot()
  })
}

shinyApp(ui = ui, server = server)
