# Forecasting Shiny app

library(shiny)
library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Time Series Plot", plotOutput("time_series_plot")),
    tabPanel("Forecast Plot", plotOutput("forecast_plot")),
    tabPanel("Accuracy Table", tableOutput("accuracy_table"))
  )
)

server <- function(input, output) {
  # Load the data
  m750 <- m4_monthly %>% filter(id == "M750")
  
  # Reactive expression for the time series plot
  time_series_plot <- reactive({
    m750 %>%
      plot_time_series(date, value, .interactive = FALSE)
  })
  
  output$time_series_plot <- renderPlot({
    time_series_plot()
  })
  
  # Reactive expression to store the models and forecast results
  models_and_forecasts <- reactive({
    # Split Data 80/20
    splits <- initial_time_split(m750, prop = 0.8)
    
    # Model 1: arima_boost ----
    model_fit_arima_boosted <- arima_boost(
      min_n = 2,
      learn_rate = 0.015
    ) %>%
      set_engine(engine = "auto_arima_xgboost") %>%
      fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
          data = training(splits))
    # Model 2: prophet ----
    model_fit_prophet <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(value ~ date, data = training(splits))
    # Model 3: earth ----
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
      model_fit_arima_boosted,
      model_fit_prophet,
      wflw_fit_mars
    )
    
    calibration_tbl <- models_tbl %>%
      modeltime_calibrate(new_data = testing(splits))
    
    refit_tbl <- calibration_tbl %>%
      modeltime_refit(data = m750)
    
    forecast_result <- refit_tbl %>%
      modeltime_forecast(h = "3 years", actual_data = m750)
    
    list(forecast_result, calibration_tbl)
  })
  
  output$forecast_plot <- renderPlot({
    models_and_forecasts()[[1]] %>%
      plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = FALSE
      )
  })
  
  output$accuracy_table <- renderTable({
    as.data.frame(models_and_forecasts()[[2]] %>%
                    modeltime_accuracy() %>%
                    table_modeltime_accuracy(
                      .interactive = FALSE
                    ))
  })
  
}

shinyApp(ui, server)
    

                            