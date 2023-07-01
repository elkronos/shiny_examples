library(shiny)
library(tidyverse)
library(timetk)

ui <- fluidPage(
  titlePanel("Anomaly Diagnostics Plot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your data (csv/xlsx):"),
      selectInput("group_var", "Select the grouping variable:",
                  choices = c("id")),
      sliderInput("alpha", "Select the ribbon alpha:",
                  min = 0, max = 1, value = 0.25, step = 0.05)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(walmart_sales_weekly)
    }
    read.csv(inFile$datapath)
  })
  
  output$plot <- renderPlot({
    data() %>%
      group_by(!!as.name(input$group_var)) %>%
      plot_anomaly_diagnostics(Date, Weekly_Sales,
                               .message = FALSE,
                               .facet_ncol = 3,
                               .ribbon_alpha = input$alpha,
                               .interactive = FALSE)
  })
}

shinyApp(ui, server)
