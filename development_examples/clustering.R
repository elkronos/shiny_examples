library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("K-Means Clustering"),
  sidebarLayout(
    sidebarPanel(
      numericInput("k", "Number of clusters:", 
                   value = 4, min = 1, max = 15),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Optimal Clusters", plotOutput("plot1")),
        tabPanel("Cluster Output", verbatimTextOutput("final")),
        tabPanel("Medoid Cluster", plotOutput("plot2"))
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    df <- iris[, 1:4]
    return(df)
  })
  
  wss <- reactive({
    function(k) {
      kmeans(data(), k, nstart = 25)$tot.withinss
    }
  })
  
  wss_values <- reactive({
    map_dbl(1:15, wss())
  })
  
  final <- reactive({
    kmeans(data(), input$k, nstart = 25)
  })
  
  output$plot1 <- renderPlot({
    plot(1:15, wss_values(), type = "b", pch = 19, frame = FALSE, 
         xlab = "Number of clusters K",
         ylab = "Total within-clusters sum of squares")
  })
  
  output$final <- renderPrint({
    final()
  })
  
  output$plot2 <- renderPlot({
    library(factoextra)
    fviz_cluster(final(), data = data())
  })
  
}

shinyApp(ui, server)
