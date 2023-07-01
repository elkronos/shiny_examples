library(shiny)
library(ggplot2)
library(mgcv)
library(readxl)
library(flexdashboard)
library(DT)
library(webshot)
library(psych)
library(knitr)
library(kableExtra)
library(plotly)

options(DT.options = list(pageLength = 5, language = list(search = 'Filter:')))

ui <- fluidPage(
  
  # Inputs
  sidebarLayout(
    sidebarPanel(
      checkboxInput('jitter', 'Jitter', value = FALSE),
      checkboxInput('smooth', 'Smooth', value = FALSE),
      
      selectInput('x', 'X', names(mtcars)),
      selectInput('y', 'Y', names(mtcars), names(mtcars)[2]),
      
      selectInput('color', 'Color', c('None', names(mtcars))),
      selectInput('shape', 'Shape', c('None', names(mtcars))),
      selectInput('size', 'Size', c('None', names(mtcars))),
      selectInput('alpha', 'Transparency', c('None', names(mtcars)))
    ),
    
    # Output
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatter")),
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$scatter <- renderPlot({
    p <- ggplot(mtcars, aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$alpha != 'None')
      p <- p + aes_string(alpha=input$alpha)
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    if (input$shape != 'None')
      p <- p + aes_string(shape=input$shape)
    
    if (input$size != 'None')
      p <- p + aes_string(size=input$size)
    
    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    
    print(p)
  })
  
  output$table <- DT::renderDataTable({
    mtcars
  })
  
  output$summary <- renderPrint({
    describe(mtcars)
  })
}

shinyApp(ui, server)
