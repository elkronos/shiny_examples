library(shiny)
library(psych)
library(corrplot)
library(ggplot2)
library(car)

# Dataset
url <- "https://raw.githubusercontent.com/housecricket/data/main/efa/sample1.csv"
data_survey <- read.csv(url, sep = ",")

# Define UI
ui <- fluidPage(
  titlePanel("Exploratory Factor Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("method", "Method", c("Factor Analysis", "Factanal Method")),
      sliderInput("n_factors", "Number of factors:", min = 1, max = 12, value = 4)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Item Descriptives", tableOutput("item_descriptives")),
        tabPanel("Correlations", plotOutput("correlations")),
        tabPanel("KMO Results", verbatimTextOutput("kmo_results")),
        tabPanel("Sphericity", verbatimTextOutput("sphericity")),
        tabPanel("Scree Plot", plotOutput("scree_plot")),
        tabPanel("Parallel Analysis", plotOutput("parallel_plot")),
        tabPanel("Factor Analysis", verbatimTextOutput("factor_analysis")),
        tabPanel("Factor Loadings", plotOutput("factor_loadings"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Item descriptives table
  output$item_descriptives <- renderTable({
    describe(data_survey)
  })
  
  # Correlations
  output$correlations <- renderPlot({
    dat <- data_survey[, -1] 
    datamatrix <- cor(dat[, c(-13)])
    corrplot(datamatrix, method = "number")
  })
  
  # KMO Results
  output$kmo_results <- renderPrint({
    X <- data_survey[, -c(13)]
    KMO(r = cor(X))
  })
  
  # Sphericity
  output$sphericity <- renderPrint({
    X <- data_survey[, -c(13)]
    cortest.bartlett(X)
  })
  
  # Scree Plot
  output$scree_plot <- renderPlot({
    fafitfree <- fa(data_survey, nfactors = input$n_factors, rotate = "none")
    n_factors <- length(fafitfree$e.values)
    scree     <- data.frame(
      Factor_n =  as.factor(1:n_factors), 
      Eigenvalue = fafitfree$e.values)
    ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
      geom_point() + geom_line() +
      xlab("Number of factors") +
      ylab("Initial eigenvalue") +
      labs(title = "Scree Plot", subtitle = "(Based on the unreduced correlation matrix)")
  })
  
  # Parallel Analysis
  output$parallel_plot <- renderPlot({
    parallel <- fa.parallel(data_survey)
    plot(parallel, main = "Parallel Analysis")
  })
  
  # Factor Analysis
  output$factor_analysis <- renderPrint({
    if (input$method == "Factor Analysis") {
      fa_results <- fa(data_survey, nfactors = input$n_factors, rotate = "varimax")
      print(fa_results)
    } else {
      factanal_results <- factanal(data_survey, factors = input$n_factors, scores = c("regression"), rotation = "varimax")
      print(factanal_results)
    }
  })
  
  # Factor Loadings
  output$factor_loadings <- renderPlot({
    fa_results <- fa(data_survey, nfactors = input$n_factors, rotate = "varimax")
    fa.diagram(fa_results)
  })
}


shinyApp(ui, server)
    