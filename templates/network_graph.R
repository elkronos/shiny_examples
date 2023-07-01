library(shiny)
library(visNetwork)
library(dplyr)

ui <- fluidPage(
  # Add a slider input for the weight
  sliderInput("weight", "Select weight:", min = 1, max = 10, value = 5),
  
  # Add the visNetwork output element
  visNetworkOutput("my_network")
)

server <- function(input, output) {
  # Load data (replace this with your own data)
  nodes <- data.frame(id = c(1, 2, 3, 4, 5), label = c("A", "B", "C", "D", "E"))
  edges <- data.frame(from = c(1, 1, 2, 2, 3, 4), to = c(2, 3, 3, 4, 5, 5), weight = c(1, 2, 3, 4, 5, 6))
  
  # Create a reactive expression that updates the edge widths based on the slider input
  edges_reactive <- reactive({
    mutate(edges, width = weight/input$weight + 1)
  })
  
  # Create the network and set the layout and edge settings
  my_network <- reactive({
    visNetwork(nodes, edges_reactive()) %>% 
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visEdges(arrows = "middle")
  })
  
  # Render the network
  output$my_network <- renderVisNetwork({
    my_network()
  })
}

# Run the app
shinyApp(ui, server)
