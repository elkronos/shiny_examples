library(shiny)
library(ggplot2)
library(GGally)

df <- mtcars

ui <- fluidPage(
  titlePanel("mtcars Scatterplot Matrix"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variables", "Select variables:",
                  c("mpg" = "mpg", "cyl" = "cyl", "disp" = "disp", "hp" = "hp", "drat" = "drat", "wt" = "wt", "qsec" = "qsec"),
                  multiple = TRUE,
                  selected = c("mpg", "cyl", "disp", "hp", "drat", "wt")),
      selectInput("group", "Select grouping variable:",
                  c("None" = "None", "vs" = "vs", "am" = "am", "gear" = "gear"),
                  selected = "None")
    ),
    mainPanel(
      plotOutput("scatterplot_matrix")
    )
  )
)

server <- function(input, output) {
  output$scatterplot_matrix <- renderPlot({
    req(input$variables)
    
    if (input$group == "None") {
      ggpairs(df[, input$variables],
              columnLabels = input$variables,
              upper = list(continuous = wrap('cor', size = 3)),
              lower = list(combo = wrap("facethist", bins = 30)),
              diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
              title = "Scatterplot matrix of `mtcars`")
    } else {
      ggpairs(df[, c(input$group, input$variables)],
              columnLabels = c(input$group, input$variables),
              aes(color = factor(df[, input$group])),
              upper = list(continuous = wrap('cor', size = 3)),
              lower = list(combo = wrap("facethist", bins = 30)),
              diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
              title = paste("Scatterplot matrix of `mtcars` Grouped by", input$group))
    }
  })
}

shinyApp(ui, server)
