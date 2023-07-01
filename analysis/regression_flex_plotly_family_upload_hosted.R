# Hosted: https://jchase.shinyapps.io/Regression/

## Generate fake data to test upload functional
# set.seed(1234)
# student_athletes <- data.frame(
#  student_id = 1:100,
#  gender = sample(c("M", "F"), 100, replace = TRUE),
#  age = rnorm(100, mean = 18, sd = 1),
#  grade = sample(1:12, 100, replace = TRUE),
#  height = rnorm(100, mean = 68, sd = 3),
#  weight = rnorm(100, mean = 150, sd = 20)
# )

## Review
# head(student_athletes)

## Locate where it will save
# getwd()

## Save data
# write.csv(student_athletes, "student_athletes.csv", row.names = F)
library(performance)
library(tidyverse)
library(broom)
library(ggfortify)
library(shiny)
library(DT)
library(plotly)
library(mgcv)
library(see)
library(patchwork)
library(cowplot)

data(mtcars)

ui <- fluidPage(
  titlePanel("Model"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      selectInput('x', 'X', choices = names(mtcars), selected = names(mtcars)[1]),
      selectInput('y', 'Y', choices = names(mtcars), selected = names(mtcars)[2]),
      selectInput("family", "Family Type:",
                  choices = c("gaussian", "poisson", "binomial"),
                  selected = "gaussian"),
      checkboxInput("loess", "Show LOESS Fit", value = FALSE),
      checkboxInput("lm", "Show Linear Model Fit", value = FALSE),
      checkboxInput("gam", "Show GAM Fit", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", plotlyOutput("scatterplot")),
        tabPanel("Descriptive Stats", DT::dataTableOutput("desc_stats")),  # Add a new tabPanel for Descriptive Stats
        tabPanel("Results", DT::dataTableOutput("reg_table")),
        tabPanel("Summary", verbatimTextOutput("model_summary")),
        tabPanel("Assumptions", plotOutput("diagnostic"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath, header = TRUE)
    } else {
      mtcars
    }
  })
  
  y_choices <- reactive({
    if (input$family == "binomial") {
      binomial_vars <- data() %>%
        select_if(function(x) length(unique(x)) == 2) %>%
        names()
      if (length(binomial_vars) == 0) {
        return(setNames(character(0), character(0)))
      }
      return(binomial_vars)
    } else {
      return(names(data()))
    }
  })
  
  observe({
    if (!is.null(data())) {
      updateSelectInput(session, "x", choices = names(data()), selected = names(data())[1])
      
      current_y_choices <- y_choices()
      
      if (input$y %in% current_y_choices) {
        updateSelectInput(session, "y", choices = current_y_choices, selected = input$y)
      } else if (length(current_y_choices) > 0) {
        updateSelectInput(session, "y", choices = current_y_choices, selected = current_y_choices[1])
      } else {
        updateSelectInput(session, "y", choices = current_y_choices)
      }
    }
  })
  
  filtered_data <- reactive({
    data_subset <- data() %>%
      select(input$x, input$y)
    
    if (input$family == "binomial") {
      data_subset[input$y] <- as.numeric(as.factor(data_subset[[input$y]])) - 1
    }
    
    # Rename variables to "x" and "y", respectively
    names(data_subset) <- c("x", "y")
    
    data_subset %>%
      filter(!is.na(x) & !is.na(y))
  })
  
  model <- reactive({
    if (input$family == "gaussian") {
      if (input$gam) {
        gam(y ~ s(x), data = filtered_data(), family = gaussian())
      } else {
        lm(y ~ x, data = filtered_data())
      }
    } else if (input$family == "poisson") {
      if (input$gam) {
        gam(y ~ s(x), data = filtered_data(), family = poisson())
      } else {
        glm(y ~ x, data = filtered_data(), family = poisson())
      }
    } else if (input$family == "binomial") {
      glm(y ~ x, data = filtered_data(), family = binomial())
    }
  })
  
  # Scatter
  output$scatterplot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x, y)) +
      geom_point()
    
    if (input$loess) {
      p <- p + geom_smooth(method = "loess")
    }
    
    if (input$lm) {
      p <- p + geom_abline(intercept = coef(model())[1],
                           slope = coef(model())[2], col = "red")
    }
    
    if (input$gam) {
      p <- p + geom_smooth(method = "gam", formula = y ~ s(x))
    }
    
    gg <- ggplotly(p)
    gg
  })
  
  # Table
  output$desc_stats <- DT::renderDataTable({   # Create a new output for Descriptive Stats
    data() %>%
      psych::describe() %>%
      DT::datatable()
  })
  
  # Results
  output$reg_table <- DT::renderDataTable({
    if (input$gam) {
      broom::tidy(model(), exponentiate = TRUE) %>% 
        DT::datatable()
    } else {
      broom::tidy(model()) %>% 
        DT::datatable()
    }
  })
  
  # Model summary
  output$model_summary <- renderPrint({
    summary(model())
  })
  
  # Assumptions plot
  output$diagnostic <- renderPlot({
    if (input$family == "gaussian") {
      if (input$gam) {
        check_model(model())
      } else {
        autoplot(model())
      }
    } else if (input$family == "poisson" || input$family == "binomial") {
      deviance_residuals <- residuals(model(), type = "deviance")
      fitted_values <- fitted(model())
      
      p1 <- ggplot() +
        geom_point(aes(x = fitted_values, y = deviance_residuals)) +
        labs(x = "Fitted Values", y = "Deviance Residuals") +
        theme_minimal() +
        ggtitle("Deviance Residuals vs Fitted Values")
      
      if (input$gam) {
        p2 <- check_model(model(), type = "predicted")
      } else {
        p2 <- ggplot() +
          geom_point(aes(x = filtered_data()$x, y = deviance_residuals)) +
          labs(x = "x", y = "Deviance Residuals") +
          theme_minimal() +
          ggtitle("Deviance Residuals vs x")
      }
      
      plot_grid(p1, p2, nrow = 1)
    }
  })
  
}

shinyApp(ui, server)