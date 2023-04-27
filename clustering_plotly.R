library(shiny)
library(tidyverse)
library(plotly)
library(factoextra)

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
        tabPanel("Optimal Clusters", plotlyOutput("plot1")),
        tabPanel("Cluster Output", verbatimTextOutput("final")),
        tabPanel("Medoid Cluster", plotlyOutput("plot2"))
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
  
  output$plot1 <- renderPlotly({
    plot_ly(x = 1:15, y = wss_values(), type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Number of clusters K"),
             yaxis = list(title = "Total within-clusters sum of squares"))
  })
  
  output$final <- renderPrint({
    final()
  })
  
  output$plot2 <- renderPlotly({
    gg <- fviz_cluster(final(), data = data(), geom = "point", frame = FALSE)
    ggplotly(gg) %>% layout(xaxis = list(title = "PC1"),
                            yaxis = list(title = "PC2"))
  })
  
}

shinyApp(ui, server)
