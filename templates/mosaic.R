library(shiny)
library(vcd)

ui <- fluidPage(
  titlePanel("Hair and Eye Color Mosaic Plot"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("shade", "Shade cells", value = TRUE),
      checkboxInput("legend", "Show legend", value = TRUE)
    ),
    mainPanel(
      plotOutput("mosaicPlot")
    )
  )
)

server <- function(input, output) {
  output$mosaicPlot <- renderPlot({
    mosaic(HairEyeColor, shade = input$shade, legend = input$legend)
  })
}

shinyApp(ui, server)
