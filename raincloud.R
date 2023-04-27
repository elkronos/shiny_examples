# Load packages
library(shiny)
library(ggplot2)
library(ggdist)

# Define the UI
ui <- fluidPage(
  titlePanel("Raincloud plot"),
  sidebarLayout(
    sidebarPanel(
      # Input for selecting x variable
      selectInput("xvar", "Select x variable", 
                  names(mtcars[sapply(mtcars, function(x) length(unique(x))) <= 5]), 
                  selected = "cyl"),
      # Input for selecting y variable
      selectInput("yvar", "Select y variable", names(mtcars), selected = "mpg")
    ),
    mainPanel(
      # Output for displaying the plot
      plotOutput("plot")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Render the plot
  output$plot <- renderPlot({
    p <- ggplot(mtcars, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) + 
      stat_halfeye(
        adjust = .5,
        width = .6, 
        ## set slab interval to show IQR and 95% data range
        .width = c(.5, .95)
      ) + 
      stat_dots(
        side = "left", 
        dotsize = .8, 
        justification = 1.05, 
        binwidth = .3
      )
    
    if (length(unique(mtcars[[input$xvar]])) == 2) {
      p <- p + scale_x_continuous(breaks = unique(mtcars[[input$xvar]]), labels = levels(factor(mtcars[[input$xvar]])))
    } else {
      p <- p + coord_cartesian(xlim = c(1.2, NA))
    }
    
    # adjust Y axis limits and breaks based on data range
    data_range <- range(mtcars[[input$yvar]])
    y_limits <- c(data_range[1] - diff(data_range) * 0.1, data_range[2] + diff(data_range) * 0.1)
    y_breaks <- pretty(y_limits, n = 5)
    p <- p + scale_y_continuous(limits = y_limits, breaks = y_breaks)
    
    p
  })
}

# Run the app
shinyApp(ui, server)
