library(shiny)
library(ggplot2)
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
      plotOutput("my_plot")
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
  
  output$my_plot <- renderPlot({
    error_bar_data <- switch(input$error_bar_type,
                             "Standard Deviation" = aes(ymin = mean - sd, ymax = mean + sd),
                             "Standard Error" = aes(ymin = mean - se, ymax = mean + se),
                             "Confidence Interval" = aes(ymin = mean - ic, ymax = mean + ic))
    
    ggplot(my_sum(), aes_string(x = input$x_var, y = "mean")) +
      geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.5) +
      geom_errorbar(error_bar_data, width = 0.4, colour = "orange", alpha = 0.9, linewidth = 1.5) +
      ggtitle(paste0("Using ", input$error_bar_type, " for Error Bars"))
  })
}

shinyApp(ui = ui, server = server)
