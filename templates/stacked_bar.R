library(shiny)
library(tidyverse)
library(scales)
data(mtcars)

# Define UI
ui <- fluidPage(
  titlePanel("Bar Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "xvar", label = "Select X variable", choices = names(mtcars), selected = "mpg"),
      selectInput(inputId = "yvar", label = "Select Y variable", choices = names(mtcars)[-1], selected = "cyl")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # create reactive data frame
  df_reactive <- reactive({
    if(input$xvar != input$yvar){
      mtcars %>%
        mutate_at(vars(input$xvar, input$yvar), factor) %>%
        group_by(!!sym(input$xvar), !!sym(input$yvar)) %>%
        summarize(n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        mutate(per = label_percent()(freq))
    }
  })
  
  # render plot
  output$plot <- renderPlot({
    if(!is.null(df_reactive())){
      ggplot(df_reactive(), aes(x = !!sym(input$xvar), fill = !!sym(input$yvar))) +
        geom_bar(position = "fill")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
