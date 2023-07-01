library(shiny)
library(ggplot2)
library(waterfalls)

# Define UI
ui <- fluidPage(
  
  titlePanel("Waterfall Chart App"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for Sales
      numericInput("sales", "Sales", value = 101000),
      # Input for Services
      numericInput("services", "Services", value = 52000),
      # Input for Fixed Costs
      numericInput("fixedcosts", "Fixed Costs", value = -23000),
      # Input for Variable Costs
      numericInput("variablecosts", "Variable Costs", value = -15000),
      # Input for Taxes
      numericInput("taxes", "Taxes", value = -10000)
    ),
    
    mainPanel(
      plotOutput("waterfallPlot")
    )
  )
)

# Define server
# Define server
server <- function(input, output) {
  
  # Create reactive data frame
  income <- reactive({
    category <- c("Sales", "Services", "Fixed Costs", 
                  "Variable Costs", "Taxes")
    amount <- c(input$sales, input$services, input$fixedcosts, input$variablecosts, input$taxes)
    data.frame(category, amount)
  })
  
  # Create waterfall chart
  output$waterfallPlot <- renderPlot({
    waterfall(income(), 
              calc_total=TRUE, 
              total_axis_text = "Net",
              total_rect_text_color="black",
              total_rect_color="goldenrod1") +
      scale_y_continuous(label=scales::dollar)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
