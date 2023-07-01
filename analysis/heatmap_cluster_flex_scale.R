library(shiny)
library(heatmaply)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("variable1", "Select the first variable:",
                  c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
                  selected = "mpg"),
      selectInput("variable2", "Select the second variable:",
                  c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
                  selected = "cyl"),
      numericInput("k_row", "Number of clusters for the rows:", value = 4, min = 1, max = 8),
      numericInput("k_col", "Number of clusters for the columns:", value = 2, min = 1, max = 2),
      selectInput("scale", "Scaling method:",
                  c("None", "Percentile", "Normalize"),
                  selected = "None")
    ),
    mainPanel(
      plotlyOutput("heatmap")
    )
  )
)

server <- function(input, output) {
  output$heatmap <- renderPlotly({
    mtcars_2 <- mtcars[, c(input$variable1, input$variable2)]
    if (input$scale == "Percentile") {
      mtcars_2 <- percentize(mtcars_2)
    } else if (input$scale == "Normalize") {
      mtcars_2 <- normalize(mtcars_2)
    }
    heatmaply(mtcars_2, k_row = input$k_row, k_col = input$k_col)
  })
}

shinyApp(ui, server)
