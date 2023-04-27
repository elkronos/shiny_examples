library(shiny)
library(plotly)
library(dplyr)

data(mtcars)
mtcars$am <- as.factor(mtcars$am)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$carb <- as.factor(mtcars$carb)

ui <- fluidPage(
  titlePanel("Plot with Error Bars"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X variable:", choices = names(mtcars)[sapply(mtcars, is.factor)]),
      selectInput("y_var", "Select Y variable:", choices = names(mtcars)[sapply(mtcars, is.numeric)]),
      selectInput("error_bar_type", "Select Error Bar Type:",
                  choices = c("Standard Deviation", "Standard Error", "Confidence Interval"))
    ),
    mainPanel(
      plotlyOutput("my_plot")
    )
  )
)

server <- function(input, output) {
  my_sum <- reactive({
    data <- mtcars %>%
      select(!!input$x_var, !!input$y_var)
    group_var <- sym(input$x_var)
    data %>%
      group_by(!!group_var) %>%
      summarise(
        n = n(),
        mean = mean(!!sym(input$y_var)),
        sd = sd(!!sym(input$y_var))
      ) %>%
      mutate(se = sd / sqrt(n)) %>%
      mutate(ic = se * qt((1 - 0.05) / 2 + 0.5, n - 1))
  })
  
  output$my_plot <- renderPlotly({
    error_bar_data <- switch(input$error_bar_type,
                             "Standard Deviation" = list(error_y = list(type = "data", array = my_sum()$sd)),
                             "Standard Error" = list(error_y = list(type = "data", array = my_sum()$se)),
                             "Confidence Interval" = list(error_y = list(type = "data", array = my_sum()$ic))
    )
    
    plot_ly(my_sum(), x = as.formula(paste0("~", input$x_var)), y = ~mean, type = "bar", name = "mean", marker = list(color = "forestgreen", opacity = 0.5), error_y = error_bar_data$error_y) %>%
      layout(title = paste0("Using ", input$error_bar_type, " for Error Bars"))
  })
}

shinyApp(ui = ui, server = server)
