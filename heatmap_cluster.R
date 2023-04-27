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
                  selected = "cyl")
    ),
    mainPanel(
      plotlyOutput("heatmap")
    )
  )
)

server <- function(input, output) {
  output$heatmap <- renderPlotly({
    mtcars_2 <- mtcars[, c(input$variable1, input$variable2)]
    mtcars_2 <- percentize(mtcars_2)
    heatmaply(mtcars_2, k_row = 4, k_col = 2)
  })
}

shinyApp(ui, server)
