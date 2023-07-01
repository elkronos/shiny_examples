# Load required libraries
library(BayesFactor)
library(data.table)
library(ggplot2)
library(rlang)
library(shiny)
library(plotly)

# Save functions
bayes_factor <- function(data, group_var, response_var, rm_na = FALSE, bf_type = "bf") {
  
  # Convert the data frame to a data table for efficient processing
  if (!inherits(data, "data.table")) {
    data <- data.table(data)
  }
  
  # Error check: validate input parameters
  if (!all(c(group_var, response_var) %in% names(data))) {
    stop("Please ensure both 'group_var' and 'response_var' are in 'data'.")
  }
  
  # Handle missing data
  if (rm_na) {
    data[, (c(group_var, response_var)) := lapply(.SD, na.omit), .SDcols = c(group_var, response_var)]
  }
  else if (any(data[, sum(is.na(.SD)), .SDcols = c(group_var, response_var)] > 0)) {
    stop("The data contains missing values. Please handle them before proceeding.")
  }
  
  # Check for non-numeric response variable
  if (!is.numeric(data[[response_var]])) {
    stop("The 'response_var' should be numeric.")
  }
  
  # Check there are at least two distinct groups
  if (uniqueN(data[[group_var]]) < 2) {
    stop("There must be at least two distinct groups in 'group_var'.")
  }
  
  # Perform Bayesian t-test
  bf_result <- BayesFactor::ttestBF(formula = as.formula(paste(response_var, "~", group_var)), data = data)
  
  # Return Bayes factor result based on the chosen type
  return(exp(bf_result@bayesFactor[[bf_type]]))
}

plot_group_means <- function(data, group_var, response_var, title = "Group Means", 
                             x_label = "Group", y_label = "Response", fill_color = "steelblue", 
                             notch = FALSE, rm_na = FALSE, bf_type = "bf", show_bf = TRUE) {
  
  # Convert the group variable to factor if it's not already
  if(!is.factor(data[[group_var]])) {
    data[[group_var]] <- as.factor(data[[group_var]])
  }
  
  # Create the boxplot
  p <- ggplot(data, aes(x = .data[[sym(group_var)]], y = .data[[sym(response_var)]], fill = .data[[sym(group_var)]])) +
    geom_boxplot(notch = notch) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    scale_fill_manual(values = fill_color)
  
  # Calculate Bayes Factor for each group comparison
  if(show_bf) {
    bf <- bayes_factor(data, group_var, response_var, rm_na, bf_type)
    
    # Add Bayes Factor as annotation to the plot
    p <- p + geom_text(x = 1.5, y = max(data[[response_var]]), 
                       label = paste("Bayes Factor =", round(bf, 2)), hjust = 0)
  }
  
  return(p)
}

data(mtcars)
mtcars$group_var <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 2)


# Define UI
ui <- fluidPage(
  titlePanel("Bayes Factor and Group Mean Plotting App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group_var", "Select the group variable:",
                  choices = names(mtcars)[sapply(mtcars, function(x) length(unique(x)) == 2)]),
      selectInput("response_var", "Select the response variable:", choices = names(mtcars)),
      checkboxInput("notch", "Notch", value = FALSE),
      selectInput("bf_type", "Bayes Factor Type:", choices = c("bf", "logbf", "lrt", "loglrt")),
      checkboxInput("show_bf", "Show Bayes Factor", value = TRUE)
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  observe({
    req(input$group_var, input$response_var)
    output$plot <- renderPlotly({
      p <- plot_group_means(mtcars, input$group_var, input$response_var,
                            title = "My Plot", fill_color = c("orange", "green"),
                            notch = input$notch, rm_na = FALSE,
                            bf_type = input$bf_type, show_bf = input$show_bf)
      ggplotly(p)
    })
  })
  
}

shinyApp(ui = ui, server = server)