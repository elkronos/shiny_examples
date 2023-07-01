library(tidyverse)
library(broom)
library(ggfortify)
library(shiny)
library(apaTables)
library(DT)

data("airquality")

ui <- fluidPage(
  titlePanel("Air Quality Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x", "Independent Variable:",
                  choices = names(airquality),
                  selected = "Wind"),
      selectInput("y", "Dependent Variable:",
                  choices = names(airquality),
                  selected = "Ozone"),
      checkboxInput("loess", "Show LOESS Fit", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", plotOutput("scatterplot")),
        tabPanel("Table", DT::dataTableOutput("reg_table")),
        tabPanel("Assumptions", plotOutput("diagnostic"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Filter data
  filtered_data <- reactive({
    airquality %>% 
      filter(!is.na(get(input$x)) & !is.na(get(input$y)))
  })
  
  # Construct model
  model <- reactive({
    lm(as.formula(paste(input$y, "~", input$x)), data = filtered_data())
  })
  
  # View model
  output$scatterplot <- renderPlot({
    p <- ggplot(filtered_data(), aes_string(input$x, input$y)) +
      geom_point()
    
    if (input$loess) {
      p <- p + geom_smooth(method = "loess")
    }
    
    p
  })
  
  # Regression table
  output$reg_table <- DT::renderDataTable({
    broom::tidy(model()) %>% 
      DT::datatable()
  })
  
  # Assumptions plot
  output$diagnostic <- renderPlot({
    autoplot(model())
  })
}

shinyApp(ui, server)
