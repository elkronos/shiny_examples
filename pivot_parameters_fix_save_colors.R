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
      downloadButton("download_all", "Download All Data"),
      actionButton("toggle_colors", "Toggle Colorizing")
    ),
    mainPanel(
      tableOutput("selected_table")
    )
  )
)

server <- function(input, output, session) {
  colorize <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_colors, {
    colorize(!colorize())
  })
  
  selected_data <- reactive({
    if (length(input$col_var) == 0) return(mtcars)
    
    if (input$row_var != "") {
      aggregate(mtcars[,input$col_var], by=list(mtcars[,input$row_var]), FUN=get(input$agg_func))
    } else {
      mtcars[,input$col_var]
    }
  })
  
  output$selected_table <- renderTable({
    if (colorize()) {
      dat <- selected_data()
      if (!is.null(dat)) {
        max_val <- max(dat)
        min_val <- min(dat)
        dat[] <- lapply(dat, function(x) {
          if (!is.numeric(x)) return(x)
          b_scale <- 255 * (x - min_val) / (max_val - min_val)
          r_scale <- 255 * (1 - (x - min_val) / (max_val - min_val))
          g_scale <- 255 - (b_scale + r_scale) / 2
          paste0("<span style='background-color: rgb(", r_scale, ",", g_scale, ",", b_scale, ");'>", x, "</span>")
        })
      }
      dat
    } else {
      selected_data()
    }
  }, sanitize.text.function = function(x) {x}, 
  escape = FALSE)
  
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
