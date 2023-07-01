library(shiny)
library(dplyr)
library(superheat)

ui <- fluidPage(
  checkboxInput("show_scatter", " Show Scatter Plot", value = TRUE),
  checkboxInput("show_bar", " Show Bar Plot", value = TRUE),
  selectInput("vars", "Variables:", 
              choices = names(mtcars)[-c(1,2)],
              selected = names(mtcars)[3:4],
              multiple = TRUE,
              selectize = TRUE
  ),
  plotOutput("superheatmap")
)

server <- function(input, output) {
  output$superheatmap <- renderPlot({
    selected_vars <- dplyr::select(mtcars, input$vars)
    yt_vals <- if (input$show_bar) cor(selected_vars, mtcars$mpg) else NULL
    if (length(selected_vars) == 0 || (length(mtcars$mpg) == 0 && input$show_scatter) || (length(yt_vals) == 0 && input$show_bar)) return()
    superheat(selected_vars, 
              scale = T,
              yr = if(input$show_scatter) mtcars$mpg else NULL,
              yr.axis.name = "miles per gallon",
              yr.axis.size = 14,
              yr.axis.name.size = 14,
              yr.plot.size = 0.8,
              yt = yt_vals,
              yt.plot.type = "bar",
              yt.axis.name = "Correlation with mpg",
              yt.axis.size = 14,
              yt.axis.name.size = 14,
              yt.plot.size = 0.7)
  })
}

shinyApp(ui, server, enableBookmarking = "server")
