# Load required packages
library(shiny)
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)

# Define UI
ui <- fluidPage(
  titlePanel("Circlepack Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("color_palette", "Color Palette:",
                  choices = c("viridis", "RdPu"))
    ),
    mainPanel(
      plotOutput("circlepack_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
  edges <- flare$edges
  vertices <- flare$vertices
  mygraph <- graph_from_data_frame( edges, vertices=vertices )
  
  # Plot
  output$circlepack_plot <- renderPlot({
    p <- ggraph(mygraph, layout = 'circlepack', weight=size) + 
      geom_node_circle(aes(fill = depth)) +
      theme_void() + 
      theme(legend.position="FALSE")
    
    if(input$color_palette == "viridis") {
      p <- p + scale_fill_viridis()
    } else if(input$color_palette == "RdPu") {
      p <- p + scale_fill_distiller(palette = "RdPu")
    }
    
    p
  })
}

# Run the app
shinyApp(ui = ui, server = server)
