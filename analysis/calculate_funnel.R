# Load necessary libraries
library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(viridis)
library(lubridate)

# Define UI for application
ui <- fluidPage(
  titlePanel("Sales Funnel Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Please Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Funnel Chart", plotlyOutput("funnel")),
                  tabPanel("Sample Data", verbatimTextOutput("sampledata"))
      )
    )
  )
)

# Define server logic required to draw the funnel
server <- function(input, output) {
  
  # Sample data in original format
  sample_data <- data.frame(
    ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    Stage1 = as.Date(c('2023-01-01', '2023-04-03', '2023-05-20', '2023-06-01', '2023-07-01', '2023-07-11', '2023-05-19', '2023-06-01', '2023-06-15')),
    Stage2 = as.Date(c('2023-01-31', '2023-05-18', '2023-06-12', '2023-06-21', '2023-07-08', NA, '2023-06-15', '2023-06-21', '2023-06-30')),
    Stage3 = as.Date(c('2023-02-10', '2023-05-20', NA, '2023-07-01', NA, NA, '2023-07-05', NA, NA)),
    Stage4 = as.Date(c('2023-02-15', '2023-06-09', NA, '2023-07-08', NA, NA, NA, NA, NA))
  )
  
  data <- reactive({
    file1 <- input$file1
    if (is.null(file1)) {
      # If no file is uploaded, reshape the sample_data
      df <- sample_data %>% 
        pivot_longer(cols = -ID, names_to = "STAGE", values_to = "DATE") %>% 
        drop_na(DATE) %>%
        count(STAGE) %>%
        mutate(STAGE = factor(STAGE, levels = names(sample_data)[-1])) %>%
        arrange(desc(STAGE)) %>%
        rename(USERS = n)
    } else {
      # If file is uploaded, read and reshape the uploaded data
      df <- read.csv(file=file1$datapath, header = input$header, stringsAsFactors = TRUE) %>% 
        pivot_longer(cols = -everything()[1], names_to = "STAGE", values_to = "DATE") %>%
        drop_na(DATE) %>%
        count(STAGE) %>%
        mutate(STAGE = factor(STAGE, levels = names(df)[-1])) %>%
        arrange(desc(STAGE)) %>%
        rename(USERS = n)
    }
    return(df)
  })
  
  # Render plot
  output$funnel <- renderPlotly({
    df <- data()
    if (!is.null(df)) {
      plot_ly(df, y = ~STAGE, x = ~USERS, type = 'funnel', textposition = "inside", textinfo = "value+percent initial") %>%
        layout(title = "Sales Funnel Visualization")
    }
  })
  
  # Display sample data in original format
  output$sampledata <- renderPrint({
    df <- sample_data
    list(head(df), str(df))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
