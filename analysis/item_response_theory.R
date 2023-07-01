library(shiny)
library(tidyverse)
library(mirt)
library(ggmirt)

# Define UI
ui <- fluidPage(
  titlePanel("Item Response Theory Analysis"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("model_type", label = "Select a model type", 
                   choices = c("2PL", "3PL"), selected = "3PL")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Item Fit Plot", plotOutput("itemfit_plot")),
        tabPanel("Person Fit Plot", plotOutput("personfit_plot")),
        tabPanel("Item-Person Map", plotOutput("itemperson_map")),
        tabPanel("Trace Plot", plotOutput("trace_plot")),
        tabPanel("Item Information Plot", plotOutput("item_info_plot")),
        tabPanel("Test Information Plot", plotOutput("test_info_plot")),
        tabPanel("Scale Characteristic Plot", plotOutput("scale_char_plot"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  set.seed(42)
  d <- sim_irt(500, 10, discrimination = .25, seed = 42)
  
  # Model
  unimodel <- 'F1 = 1-10'
  
  model_type <- reactive({
    if (input$model_type == "2PL") {
      itemtype <- "2PL"
    } else {
      itemtype <- "3PL"
    }
    return(itemtype)
  })
  
  fit <- reactive({
    mirt(data = d, model = unimodel, itemtype = model_type(), verbose = FALSE)
  })
  
  output$itemfit_plot <- renderPlot({
    itemfitPlot(fit())
  })
  
  output$personfit_plot <- renderPlot({
    personfitPlot(fit())
  })
  
  output$itemperson_map <- renderPlot({
    itempersonMap(fit())
  })
  
  output$trace_plot <- renderPlot({
    tracePlot(fit(), facet = F, legend = T) + scale_color_brewer(palette = "Set3")
  })
  
  output$item_info_plot <- renderPlot({
    itemInfoPlot(fit()) + scale_color_brewer(palette = "Set3")
  })
  
  output$test_info_plot <- renderPlot({
    testInfoPlot(fit(), adj_factor = 2)
  })
  
  output$scale_char_plot <- renderPlot({
    scaleCharPlot(fit())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
