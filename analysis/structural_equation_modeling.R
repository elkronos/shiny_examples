# Load necessary libraries
library(shiny)
library(lavaan)
library(semPlot)
library(DT)
library(readxl)

# Define a function to create sample data
createSampleData <- function() {
  data <- data.frame(
    rep = factor(rep(c(1,2,3,4),25)),
    water = factor(rep(c("wet","dry"),50)),
    priming = factor(rep(c("primed","unprimed"),50)),
    aba = rnorm(100, mean = 5, sd = 2),
    apx = rnorm(100, mean = 7, sd = 2),
    pod = rnorm(100, mean = 6, sd = 2),
    til = rnorm(100, mean = 20, sd = 5),
    pl = rnorm(100, mean = 30, sd = 5),
    grp = rnorm(100, mean = 25, sd = 5),
    tgw = rnorm(100, mean = 1, sd = 0.2),
    gy = rnorm(100, mean = 50, sd = 10)
  )
  return(data)
}

# Define a function to run the analysis
runAnalysis <- function(data, formula) {
  data <- data.frame(lapply(data, function(x) {if(is.character(x)) as.factor(x) else x}))
  
  modelEstimate <- sem(formula, data = data)
  return(list("modelEstimate" = modelEstimate))
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Structural Equation Modelling"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      helpText("Or use example data loaded from the server:"),
      textInput("formula", "Formula", 
                value = 'EA =~ aba + apx + pod; YC =~ til + pl + grp + tgw; gy ~ EA + YC'),
      selectInput("layout", "Select plot layout:", choices = c("tree", "spring"), selected = "tree"),
      checkboxInput("curve", "Curve adjacent paths?", FALSE),
      actionButton("goButton", "Go!"),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("SEM Plot", plotOutput("semPlot")),
        tabPanel("Model Statistics", DT::dataTableOutput("modelStats"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Create sample data upon loading
  createSampleData() -> data
  
  dataInput <- reactive({
    if (is.null(input$file1)) {
      return(data)
    } else {
      return(read.csv(input$file1$datapath))
    }
  })
  
  results <- eventReactive(input$goButton, {
    runAnalysis(dataInput(), input$formula)
  })
  
  output$semPlot <- renderPlot({
    req(results())
    semPaths(results()$modelEstimate, layout = input$layout, curveAdjacent = input$curve)
  })
  
  output$modelStats <- DT::renderDataTable({
    req(results())
    variance_table <- lavaan::varTable(results()$modelEstimate)
    DT::datatable(variance_table, filter = "top", options = list(pageLength = 5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
