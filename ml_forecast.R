# Load libraries
library(shiny)
library(DT)
library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("ML Forecasting"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV"),
      selectInput("models", "Select Models", choices = c(
        "ARIMA",
        "ARIMA Boosted",
        "ETS",
        "Prophet",
        "Linear Regression",
        "MARS"
      ), selected = c(
        "ARIMA",
        "ARIMA Boosted",
        "ETS",
        "Prophet",
        "Linear Regression",
        "MARS"
      ), multiple = TRUE),
      numericInput("horizon", "Forecast Horizon (Months)", value = 36, min = 1, max = 120),
    ),
    mainPanel(
      plotlyOutput("forecastPlot"),
      DT::dataTableOutput("accuracyTable")
    )
  )
)

server <- function(input, output) {
  inputData <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(m750)
    }
    read.csv(inFile$datapath)
  })
  
  modelData <- reactive({
    if (is.null(inputData())) {
      return(NULL)
    }
    
    splits <- initial_time_split(inputData(), prop = 0.9)
    
    model_fit_arima_no_boost <- arima_reg() %>%
      set_engine(engine = "auto_arima") %>%
      fit(value ~ date, data = training(splits))
    
    model_fit_arima_boosted <- arima_boost(
      min_n = 2,
      learn_rate = 0.015
    ) %>%
      set_engine(engine = "auto_arima_xgboost") %>%
      fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
          data = training(splits))
    
    model_fit_ets <- exp_smoothing() %>%
      set_engine(engine = "ets") %>%
      fit(value ~ date, data = training(splits))
    
    model_fit_prophet <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(value ~ date, data = training(splits))
    
    model_fit_lm <- linear_reg() %>%
      set_engine("lm") %>%
      fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
          data = training(splits))
    
    model_spec_mars <- mars(mode = "regression") %>%
      set_engine("earth") 
    
    recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
      step_date(date, features = "month", ordinal = FALSE) %>%
      step_mutate(date_num = as.numeric(date)) %>%
      step_normalize(date_num) %>%
      step_rm(date)
    
    wflw_fit_mars <- workflow() %>%
      add_recipe(recipe_spec) %>%
      add_model(model_spec_mars) %>%
      fit(training(splits))
    
    models_tbl <- modeltime_table(
      model_fit_arima_no_boost,
      model_fit_arima_boosted,
      model_fit_ets,
      model_fit_prophet,
      model_fit_lm,
      wflw_fit_mars
    )
    
    calibration_tbl <- models_tbl %>%
      modeltime_calibrate(new_data = testing(splits))
    
    refit_tbl <- calibration_tbl %>%
      modeltime_refit(data = inputData())
    
    list(
      forecast_data = refit_tbl %>%
        modeltime_forecast(h = paste0(input$horizon, " months"), actual_data = inputData()),
      accuracy_data = calibration_tbl %>%
        modeltime_accuracy() %>%
        table_modeltime_accuracy(.interactive = F)
    )
  })
  
  output$forecastPlot <- renderPlotly({
    if (is.null(modelData())) {
      return(NULL)
    }
    modelData()$forecast_data %>%
      plot_modeltime_forecast(.legend_max_width = 25, .interactive = T) %>%
      ggplotly()
  })
  
  output$accuracyTable <- DT::renderDataTable({
    if (is.null(modelData())) {
      return(NULL)
    }
    as.data.frame(modelData()$accuracy_data)
  })
}

shinyApp(ui, server)