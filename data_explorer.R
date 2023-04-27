# Hosted: https://elkronos.shinyapps.io/data_explorer/

# Load the required packages
library(shiny)
library(ggplot2)
library(janitor)
library(plotly)
library(DT)
library(psych)
library(readxl)
library(haven)
library(jsonlite)
library(readr)
library(rjson)
library(R.matlab)
library(rvest)
library(dplyr)

# Define the user interface
ui <- fluidPage(
  titlePanel("Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data File (optional)", 
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx",
                           ".xls",
                           ".rds",
                           ".sav",
                           ".json",
                           ".html",
                           ".mat")),
      uiOutput("x_var_ui"),
      uiOutput("y_var_ui"),
      uiOutput("color_var_ui"),
      selectInput("plot_type", "Plot Type:", choices = c("Scatter Plot" = "scatter", "Bar Plot" = "bar", "Box Plot" = "box", "Histogram" = "histogram")),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        checkboxInput("loess", "Add LOESS Line", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        checkboxInput("jitter", "Add Jitter to Scatter Plot", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        checkboxInput("color_as_factor", "Treat Color Variable as Factor", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'histogram'",
        sliderInput("binwidth", "Bin Width", min = 1, max = 100, value = 10, step = 1),
        sliderInput("max_levels", "Max Levels for Y Variable", min = 2, max = 25, value = 12, step = 1)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'bar'",
        selectInput("agg_type", "Aggregation Type:", choices = c("Sum" = "sum", "Average" = "mean", "Median" = "median")),
        uiOutput("facet_var_ui")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'box'",
        sliderInput("max_levels_box", "Max Levels for X Variable", min = 2, max = 25, value = 12, step = 1)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("main_plot")),
        tabPanel("Data Table", DTOutput("data_table")),
        tabPanel("Descriptive Statistics", DTOutput("desc_stats"), downloadButton("download_desc_stats", "Download Descriptive Statistics"))
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    inFile <- input$file
    
    if (is.null(inFile)) {
      data <- mtcars
    } else {
      ext <- tools::file_ext(inFile$datapath)
      switch(ext,
             csv = { data <- read.csv(inFile$datapath) },
             xlsx = { data <- readxl::read_excel(inFile$datapath) },
             xls = { data <- readxl::read_excel(inFile$datapath) },
             rds = { data <- readRDS(inFile$datapath) },
             sav = { data <- haven::read_spss(inFile$datapath) },
             json = { data <- jsonlite::fromJSON(inFile$datapath, flatten = TRUE) },
             html = { data <- read_html(inFile$datapath) %>% html_table(fill = TRUE) %>% .[[1]] },
             mat = { data <- R.matlab::readMat(inFile$datapath)$Dataset },
             { showNotification("Error: Unsupported file type", type = "error"); return(NULL) }
      )
      
      if (!is.null(data)) {
        data <- clean_names(data)
      }
    }
    
    return(data)
  })
  
  x_var_choices <- reactive({
    data <- dataset()
    req(data)
    req(input$plot_type)
    
    choices <- colnames(data)
    
    if (input$plot_type == "box") {
      max_levels_box <- req(input$max_levels_box)
      choices <- choices[sapply(data[, choices, drop=FALSE], function(x) length(unique(x))) <= max_levels_box]
    }
    
    choices
  })
  
  color_var_choices <- reactive({
    req(input$max_levels)
    data <- dataset()
    color_choices <- colnames(data)
    
    if (input$plot_type == 'histogram') {
      color_choices <- color_choices[sapply(data[, color_choices, drop=FALSE], function(x) length(unique(x))) <= input$max_levels]
    }
    
    color_choices
  })
  
  output$x_var_ui <- renderUI({
    x_choices <- x_var_choices()
    selectInput("x_var", "X Variable:", choices = x_choices, selected = x_choices[1])
  })
  
  output$y_var_ui <- renderUI({
    y_choices <- colnames(dataset())
    selectInput("y_var", "Y Variable:", choices = y_choices, selected = y_choices[2])
  })
  
  output$color_var_ui <- renderUI({
    req(input$plot_type)
    
    if (input$plot_type != "box") {
      color_choices <- c("None", color_var_choices())
      selectInput("color_var", "Color By:", choices = color_choices, selected = "None")
    } else {
      NULL
    }
  })
  
  output$main_plot <- renderPlotly({
    req(input$x_var, input$plot_type)
    data <- dataset()
    req(data)
    x_var <- input$x_var
    y_var <- input$y_var
    plot_type <- input$plot_type
    
    if (plot_type == "box") {
      data[, x_var] <- as.factor(data[, x_var])
      color_var <- x_var
    } else if (plot_type == "histogram") {
      color_var <- input$color_var
      if (color_var != "None") {
        data[, color_var] <- as.factor(data[, color_var])
      }
    } else {
      color_var <- input$color_var
      if (color_var != "None") {
        if (is.numeric(data[[color_var]]) && input$color_as_factor) {
          data[, color_var] <- as.factor(data[, color_var])
        }
      }
    }
    
    if (plot_type == "scatter") {
      if (color_var != "None") {
        if (input$jitter) {
          p <- ggplot(data, aes_string(x = x_var, y = y_var, color = color_var, group = color_var)) +
            geom_point(position = position_jitter(width = 0.1, height = 0.1))
        } else {
          p <- ggplot(data, aes_string(x = x_var, y = y_var, color = color_var, group = color_var)) +
            geom_point()
        }
      } else {
        if (input$jitter) {
          p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
            geom_point(position = position_jitter(width = 0.1, height = 0.1))
        } else {
          p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
            geom_point()
        }
      }
      if (input$loess) {
        p <- p + geom_smooth(method = "loess", se = FALSE)
      }
    } else if (plot_type == "bar") {
      agg_type <- match.fun(input$agg_type)
      agg_data <- data %>%
        group_by(across(all_of(x_var))) %>%
        summarise(across(all_of(y_var), agg_type))
      if (color_var != "None") {
        p <- ggplot(agg_data, aes_string(x = x_var, y = y_var, fill = color_var, group = color_var)) +
          geom_col(position = "dodge")
      } else {
        p <- ggplot(agg_data, aes_string(x = x_var, y = y_var)) +
          geom_col(position = "dodge")
      }
    } else if (plot_type == "box") {
      if (color_var != "None") {
        p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = color_var, group = color_var)) + geom_boxplot()
      } else {
        p <- ggplot(data, aes_string(x = x_var, y = y_var)) + geom_boxplot()
      }
    } else if (plot_type == "histogram") {
      if (color_var != "None") {
        p <- ggplot(data, aes_string(x = x_var, fill = color_var, group = color_var)) +
          geom_histogram(alpha = 0.5, position = "identity", binwidth = input$binwidth)
      } else {
        p <- ggplot(data, aes_string(x = x_var)) +
          geom_histogram(alpha = 0.5, position = "identity", binwidth = input$binwidth)
      }
    }
    
    ggplotly(p)
  })
  
  output$data_table <- renderDT({
    data <- dataset()
    req(data)
    datatable(data, options = list(pageLength = 10, searchHighlight = TRUE), filter = "top")
  })
  
  output$desc_stats <- renderDT({
    data <- dataset()
    req(data)
    desc_stats <- describe(data)
    datatable(desc_stats, options = list(pageLength = 10, searchHighlight = TRUE), filter = "top")
  })
  
  output$download_desc_stats <- downloadHandler(
    filename = function() {
      paste("descriptive_statistics", ".csv", sep = "")
    },
    content = function(file) {
      data <- dataset()
      req(data)
      desc_stats <- describe(data)
      write.csv(desc_stats, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
