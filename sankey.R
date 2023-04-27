library(shiny)
library(networkD3)
library(jsonlite)

# Load energy projection data
URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)

# Define UI
ui <- fluidPage(
  sliderInput("width", "Node Width:", min = 10, max = 100, value = 30),
  sankeyNetworkOutput("plot")
)

# Define server
server <- function(input, output) {
  output$plot <- renderSankeyNetwork({
    # Render sankey plot with updated node width
    sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "TWh", fontSize = 12, nodeWidth = input$width)
  })
}

# Run app
shinyApp(ui, server)
