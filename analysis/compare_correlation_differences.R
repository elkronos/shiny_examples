# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(MASS)
library(polycor)

# Save function
compare_correlations <- function(data, grouping_var, polychoric = FALSE) {
  
  # Check if grouping variable exists in the data
  if (!(grouping_var %in% colnames(data))) {
    stop(paste0("Error: Grouping variable ", grouping_var, " does not exist in the dataset."))
  }
  
  # Create a list to store the correlation matrices
  cor_list <- list()
  
  # Get unique values of the grouping variable
  group_values <- unique(data[[grouping_var]])
  
  # Calculate correlation matrix for each group
  for(i in seq_along(group_values)) {
    group_data <- data %>% 
      filter((!!sym(grouping_var)) == group_values[i]) %>%
      select_if(is.numeric)
    
    if (polychoric) {
      library(polycor)
      cor_list[[i]] <- hetcor(group_data, ML = TRUE)$correlations
    } else {
      cor_list[[i]] <- cor(group_data, use = "pairwise.complete.obs")
    }
  }
  
  # Get all permutations of two different groups
  permutations <- combn(length(cor_list), 2, simplify = FALSE)
  
  # Create a list to store the heatmaps
  heatmap_list <- list()
  
  # For each permutation, calculate the absolute difference between correlation matrices
  for (i in seq_along(permutations)) {
    # Get the two correlation matrices
    cor1 <- cor_list[[permutations[[i]][1]]]
    cor2 <- cor_list[[permutations[[i]][2]]]
    
    # Calculate the absolute difference
    difference <- abs(cor1 - cor2)
    
    # Melt the difference matrix to long format for ggplot2
    melted <- melt(difference)
    
    # Only keep lower triangle of the matrix for the plot
    melted$Var1 <- as.numeric(as.factor(melted$Var1))
    melted$Var2 <- as.numeric(as.factor(melted$Var2))
    melted <- melted[melted$Var1 > melted$Var2,]
    
    # Create a heatmap of the absolute differences
    heatmap_list[[i]] <- ggplot(data = melted, aes(x = factor(Var1), y = factor(Var2), fill = value)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C", direction = -1, aesthetics = "fill", limits = c(0,1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(title = paste("Difference between group", permutations[[i]][1], 
                         "and group", permutations[[i]][2]),
           fill = "Absolute\nDifference") +
      coord_fixed() +
      geom_text(aes(label = round(value, 2)), size = 5, color = "black")  # Added geom_text to display values on tiles
  }
  
  return(heatmap_list)
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Compare Correlations"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your Dataset", accept = c(".csv", ".tsv")),
      selectInput("group", "Select Group Variable", choices = NULL), 
      checkboxInput("polychoric", "Use Polychoric Correlations", FALSE),
      actionButton("goButton", "Go!")
    ),
    mainPanel(
      uiOutput("heatmap_ui")  # Change to uiOutput
    )
  )
)

# Define server logic 
# Define server logic 
server <- function(input, output, session) {
  
  # Default dataset
  set.seed(123)
  x1 <- rnorm(300)
  x2 <- rnorm(300)
  x3 <- x1 + rnorm(300, sd = 0.5)
  x4 <- x2 + rnorm(300, sd = 0.5)
  grouping_var <- sample(c("Group1", "Group2", "Group3"), 300, replace = TRUE)
  default_data <- data.frame(grouping_var, x1, x2, x3, x4)
  
  data <- reactive({
    if (is.null(input$file)) {
      return(default_data)
    } else {
      file <- input$file$datapath
      ext <- tools::file_ext(file)
      switch(ext,
             csv = read.csv(file, stringsAsFactors = FALSE),
             tsv = read.delim(file, stringsAsFactors = FALSE),
             stop("Invalid file type.")
      )
    }
  })
  
  observe({
    req(data())
    updateSelectInput(session, "group", choices = names(data()))
  })
  
  # Store the plot data in a reactiveValues
  plot_data <- reactiveValues()
  
  observeEvent(input$goButton, {
    data <- data()
    grouping_var <- input$group
    polychoric <- input$polychoric
    heatmaps <- compare_correlations(data, grouping_var, polychoric)
    
    # Store the plot data for each plot in plot_data
    for(i in seq_along(heatmaps)) {
      plot_data[[paste0("plot", i)]] <- heatmaps[[i]]
    }
  })
  
  # Dynamically generate the UI for the plots
  output$heatmap_ui <- renderUI({
    plot_output_list <- lapply(1:length(plot_data), function(i) {
      plotname <- paste0("plot", i)
      # Adjust the height and width of the plot
      plotOutput(plotname, height = 400, width = 400)
    })
    
    do.call(tagList, plot_output_list)
  })
  
  # Dynamically generate renderPlot for each plot
  observe({
    lapply(1:length(plot_data), function(i) {
      output[[paste0("plot", i)]] <- renderPlot({
        plot_data[[paste0("plot", i)]]
      })
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
