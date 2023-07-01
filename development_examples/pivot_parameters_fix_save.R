library(shiny)
library(writexl)
library(lubridate)

ui <- fluidPage(
  titlePanel("Column and Row Selection with Aggregation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("col_var", "Select Column Variables:", names(mtcars), 
                  multiple = TRUE),
      selectInput("row_var", "Select Row Variable:", c("None" = "", names(mtcars))),
      radioButtons("agg_func", "Aggregation Function:",
                   c("Count" = "length", "Sum" = "sum", "Average" = "mean")),
      downloadButton("download_selected", "Download Selected Data"),
      downloadButton("download_all", "Download All Data")
    ),
    mainPanel(
      tableOutput("selected_table")
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    if (length(input$col_var) == 0) return(mtcars)
    
    if (input$row_var != "") {
      aggregate(mtcars[,input$col_var], by=list(mtcars[,input$row_var]), FUN=get(input$agg_func))
    } else {
      mtcars[,input$col_var]
    }
  })
  
  output$selected_table <- renderTable({
    selected_data()
  })
  
  filename_selected <- reactive({
    paste0("selected_data_cols_", paste(input$col_var, collapse = "_"), 
           "_row_", input$row_var, "_agg_", input$agg_func, "_", 
           format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
  })
  
  output$download_selected <- downloadHandler(
    filename = filename_selected,
    content = function(file) {
      write_xlsx(selected_data(), file)
    }
  )
  
  filename_all <- reactive({
    paste0(deparse(substitute(mtcars)), "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
  })
  
  output$download_all <- downloadHandler(
    filename = filename_all,
    content = function(file) {
      write_xlsx(mtcars, file)
    }
  )
}

shinyApp(ui = ui, server = server)
