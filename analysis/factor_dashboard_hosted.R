# Hosted: https://jchase.shinyapps.io/psychometrics/

## Create fake survey data set
# set.seed(123)

## Create a data frame with a user ID and 10 ordinal items
# df <- data.frame(user_id = 1:100,
#                  item1 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item2 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item3 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item4 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item5 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item6 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)),
#                  item7 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)),
#                  item8 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)),
#                  item9 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)),
#                  item10 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)))

## Save data for upload
# write.csv(df, "survey_data.csv", row.names = F)

## Locate data
# getwd()

# Hosted: https://jchase.shinyapps.io/psychometrics/

## Create fake survey data set
# set.seed(123)

## Create a data frame with a user ID and 10 ordinal items
# df <- data.frame(user_id = 1:100,
#                  item1 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item2 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item3 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item4 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item5 = sample(1:5, 100, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#                  item6 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)),
#                  item7 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)),
#                  item8 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)),
#                  item9 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)),
#                  item10 = sample(1:5, 100, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.2)))

## Save data for upload
# write.csv(df, "survey_data.csv", row.names = F)

## Locate data
# getwd()

# Load necessary libraries and functions
library(shiny)
library(correlation)
library(see)
library(psych)
library(DT)
library(GPArotation)
library(glue)
data(bfi)

# Define UI
ui <- fluidPage(
  
  titlePanel("Survey Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your data set"),
      selectInput("corr_method", "Correlation method", 
                  choices = c("Pearson", "Kendall", "Spearman"), selected = "Pearson"),
      numericInput("fa_n_factors", "Number of factors", 
                   1, min = 1, max = 10),
      selectInput("fa_rotation", "Rotation type",
                  choices = c("bentlerQ", "bentlerT", "bifactor", "biquartimin", "cluster", "equamax", "geominQ", "geominT", "none", "oblimin", "oblique", "promax", "quartimax", "simplimax", "varimax", "varimin"), selected = "varimax"),
      checkboxInput("polychoric", "Polychoric correlation", value = FALSE),
      uiOutput("items")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Correlations", plotOutput("correlations")),
        tabPanel("Descriptives", dataTableOutput("descriptives")),
        tabPanel("Factor Analysis", verbatimTextOutput("fa_results")),
        tabPanel("Assumptions", verbatimTextOutput("assumptions"), plotOutput("scree"), tableOutput("eigenvalues")),
        tabPanel("Factor Diagram", plotOutput("fa_diagram")),
        tabPanel("Reliability and Omega", verbatimTextOutput("omega"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Load data based on file input or use default data
  data <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath, header = TRUE)
    } else {
      na.omit(bfi[,c(1:9)])
    }
  })
  
  # Render the checkboxGroupInput for selecting items
  output$items <- renderUI({
    items <- names(data())
    checkboxGroupInput("items_selected", "Items to include",
                       choices = items, selected = items)
  })
  
  # Determine the correlation method
  corr_method <- reactive({
    switch(input$corr_method,
           "Pearson" = "pearson",
           "Kendall" = "kendall",
           "Spearman" = "spearman")
  })
  
  # Create a subset of the data based on selected items
  data_selected <- reactive({
    req(input$items_selected)
    data()[, input$items_selected, drop = FALSE]
  })
  
  # Calculate and display correlations
  corr_results <- reactive({
    correlation(data_selected(), method = corr_method())
  })
  
  output$correlations <- renderPlot({
    corr_results() %>%
      summary(redundant = TRUE) %>%
      plot()
  })
  
  # Calculate and display descriptives
  output$descriptives <- renderDataTable({
    round(as.data.frame(describe(data_selected())), 2)
  })
  
  # Perform factor analysis and display results
  fa_results <- reactive({
    mycorr <- cor(data_selected(), method = corr_method())
    fa(mycorr, nfactors = input$fa_n_factors, rotate = input$fa_rotation,
       cor = ifelse(input$polychoric, "polychoric", "correlation"))
  })
  
  output$fa_results <- renderPrint({
    fa_results()
  })
  
  # Check assumptions and display results
  output$assumptions <- renderPrint({
    kmo <- KMO(data_selected())
    bartlett <- cortest.bartlett(data_selected())
    glue("KMO: {round(kmo$MSA, 2)}",
         "Bartlett's Test of Sphericity: p-value = {bartlett$p.value}")
  })
  
  # Display scree plot and eigenvalues
  output$scree <- renderPlot({
    scree_data <- scree(data_selected(), pc = FALSE)
    plot(scree_data)
  })
  
  output$eigenvalues <- renderTable({
    mycorr <- cor(data_selected(), method = corr_method())
    ev <- eigen(mycorr)
    eigenvalues <- data.frame(Eigenvalue = ev$values)
    rownames(eigenvalues) <- paste0("Factor ", 1:length(eigenvalues$Eigenvalue))
    eigenvalues
  })
  
  # Display factor diagram
  output$fa_diagram <- renderPlot({
    loads <- fa_results()$loadings
    fa.diagram(loads)
  })
  
  # Calculate and display reliability and omega
  output$omega <- renderPrint({
    omega(data_selected())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

