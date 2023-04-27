library(shiny)

ui <- fluidPage(
  titlePanel("Column and Row Selection with Aggregation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("col_var", "Select Column Variable:", c("None" = "", names(mtcars))),
      selectInput("row_var", "Select Row Variable:", c("None" = "", names(mtcars))),
      radioButtons("agg_func", "Aggregation Function:",
                   c("Count" = "length", "Sum" = "sum", "Average" = "mean"))
    ),
    mainPanel(
      tableOutput("selected_table")
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    if (input$col_var == "" & input$row_var == "") return(mtcars)
    
    if (input$row_var != "") {
      aggregate(mtcars[,input$col_var], by=list(mtcars[,input$row_var]), FUN=get(input$agg_func))
    } else {
      aggregate(mtcars[,input$col_var], FUN=get(input$agg_func))
    }
  })
  
  output$selected_table <- renderTable({
    selected_data()
  })
}

shinyApp(ui = ui, server = server)
