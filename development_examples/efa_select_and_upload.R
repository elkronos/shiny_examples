library(shiny)
library(psych)

# Define UI
ui <- fluidPage(
  titlePanel("Factor Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your own CSV"),
      selectInput("target_var", "Select target variable:", "gender", choices = names(bfi)),
      selectInput("factor_items", "Select items for factor analysis:", choices = names(bfi), multiple = TRUE),
      numericInput("num_factors", "Number of Factors:", 1, min = 1, max = 10),
      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      plotOutput("factor_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Load data from the psych package by default
  data <- bfi
  
  # Update dropdown lists based on the uploaded data
  observeEvent(input$file, {
    req(input$file)
    data <<- read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE)
    updateSelectInput(session, "target_var", choices = names(data))
    updateSelectInput(session, "factor_items", choices = names(data))
  })
  
  # Generate factor plot
  output$factor_plot <- renderPlot({
    req(input$analyze)
    items <- input$factor_items
    if (length(items) > 0) {
      fa <- fa(data[, c(input$target_var, items)], nfactors = input$num_factors)
      fa.diagram(fa)
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
