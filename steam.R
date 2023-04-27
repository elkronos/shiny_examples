library(shiny)
# devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)

# Define UI
ui <- fluidPage(
  
  titlePanel("Streamgraph App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Plot Type",
                  choices = c("Classic Streamgraph", "Stacked Area Graph", "Stacked Barplot"),
                  selected = "Classic Streamgraph")
    ),
    
    mainPanel(
      uiOutput("streamgraph")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create data
  data <- data.frame(
    year = rep(seq(1990, 2016), each = 10),
    name = rep(letters[1:10], 27),
    value = sample(seq(0, 1, 0.0001), 270)
  )
  
  # Render streamgraph based on selected plot type
  output$streamgraph <- renderUI({
    if (input$plotType == "Classic Streamgraph") {
      streamgraph(data, key = "name", value = "value", date = "year", width = "400px", height = "300px")
    } else if (input$plotType == "Stacked Area Graph") {
      streamgraph(data, key = "name", value = "value", date = "year", interpolate = "linear", width = "400px", height = "300px")
    } else if (input$plotType == "Stacked Barplot") {
      streamgraph(data, key = "name", value = "value", date = "year", interpolate = "step", width = "400px", height = "300px")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
