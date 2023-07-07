# Load the necessary libraries
library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(viridis)

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
      checkboxInput("header", "Header", TRUE),
      uiOutput("group_selector")
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
  
  # Sample data
  sample_data <- data.frame(
    GROUP = c(rep("Group1", 8), rep("Group2", 8)),
    STAGE = rep(rep(c('Stage 01: Browsers','Stage 02: Unbounced Users','Stage 03: Email Signups','Stage 04: Email Confirmed'), 2), 2),
    GENDER = rep(rep(c('Male', 'Female'), each=4), 2),
    USERS = c(-14927618.71,-12862663.41,-11361896.41,-9411708.103, 14226434.29,12276042.59,10850385.59,8999931.897,
              -1245632.56,-1132452.34,-984526.31,-834527.83, 1236457.24,1123542.23,986547.33,834254.98)
  )
  
  data <- reactive({
    file1 <- input$file1
    if (is.null(file1)) {
      return(sample_data)
    }
    else {
      df <- read.csv(file=file1$datapath, header = input$header)
      if ("STAGE" %in% names(df) & "GENDER" %in% names(df) & "USERS" %in% names(df) & "GROUP" %in% names(df)){
        return(df)
      }
      else {
        return(NULL)
      }
    }
  })
  
  output$group_selector <- renderUI({
    df <- data()
    selectInput("group", "Select Group", choices = unique(df$GROUP))
  })
  
  # Render plot
  output$funnel <- renderPlotly({
    req(input$group)
    df <- data()
    df <- df[df$GROUP == input$group, ]
    if (!is.null(df)) {
      brks <- seq(min(df$USERS), max(df$USERS), length.out = 6)
      lbls <- round(brks / 1e6, 1) # Values in Millions
      lbls <- paste(lbls, "M")
      lbls <- lbls[1:length(brks)]
      p <- df %>% mutate(USERS = as.numeric(USERS)) %>%
        ggplot(aes(x = reorder(STAGE,abs(USERS)), y = USERS, fill = GENDER)) +
        geom_bar(stat = "identity", width = .6) +
        scale_y_continuous(breaks = brks, labels = lbls) +
        scale_fill_viridis(discrete = TRUE, option = "D") +
        coord_flip() +
        theme_minimal() +
        labs(title="Email Campaign Funnel") +
        theme(plot.title = element_text(hjust = .5),
              axis.title.y = element_blank(),
              axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.1),
              axis.ticks = element_blank())
      ggplotly(p)
    }
  })
  
  # Display sample data
  output$sampledata <- renderPrint({
    df <- sample_data
    list(head(df), str(df))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
