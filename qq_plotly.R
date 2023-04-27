library(shiny)
library(ggplot2)
library(ggpubr)
library(plotly)

# Save data
as.data.frame(mtcars) -> dataset

# ui
ui <- fluidPage(
  selectInput(inputId = "variable", label = "Select a variable", 
              choices = names(dataset), selected = "mpg"),
  selectInput(inputId = "facet_var", label = "Select a variable to facet wrap by", 
              choices = c("None", names(dataset)), selected = "None"),
  plotlyOutput(outputId = "qq_plot")
)


# Server
server <- function(input, output) {
  output$qq_plot <- renderPlotly({
    if (input$facet_var == "None") {
      p <- ggplot(dataset, aes(sample = eval(parse(text = input$variable)))) +
        geom_qq() +
        stat_qq_line(color = "red", linetype = "dashed") +
        xlab("Theoretical Quantiles") +
        ylab("Sample Quantiles")
    } else {
      p <- ggplot(dataset, aes(sample = eval(parse(text = input$variable)))) +
        geom_qq() +
        stat_qq_line(color = "red", linetype = "dashed") +
        facet_wrap(vars(.data[[input$facet_var]])) +
        xlab("Theoretical Quantiles") +
        ylab("Sample Quantiles")
    }
    ggplotly(p)
  })
}


# Run app
shinyApp(ui, server)
