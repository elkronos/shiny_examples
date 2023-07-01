library(shiny)
library(ggplot2)

# Data
set.seed(123456)
df <- data.frame(group_1 = LETTERS[1:20],
                 group_2 = LETTERS[1:20],
                 value_1 = rnorm(20),
                 value_2 = rnorm(20))

df$group_1 <- factor(df$group_1)
df$group_2 <- factor(df$group_2)

# Define UI for shiny app
ui <- fluidPage(
  titlePanel("Divergent Bar Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X factor:", choices = names(df)[sapply(df, is.factor)]),
      selectInput("y_var", "Select Y numeric variable:", choices = names(df)[sapply(df, is.numeric)]),
      submitButton("Update plot")
    ),
    mainPanel(
      plotOutput("bar_plot")
    )
  )
)

# Define server for shiny app
server <- function(input, output) {
  output$bar_plot <- renderPlot({
    # Generate color based on value
    color <- ifelse(df[[input$y_var]] < 0, "pink", "lightblue")
    
    ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill = color, # Background color
               color = "white") + # Border color
      xlab(input$x_var) +
      ylab(input$y_var)
  })
}

# Run the shiny app
shinyApp(ui, server)
