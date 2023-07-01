library(shiny)
library(ggpubr)
library(plotly)

ui <- fluidPage(
  titlePanel("Tooth Growth Comparison"),
  sidebarLayout(
    sidebarPanel(
      selectInput("test_method", "Select Test Method:", 
                  c("t.test" = "t.test", 
                    "anova" = "anova", 
                    "kruskal.test" = "kruskal.test")),
      selectInput("x_var", "Select X Variable:", 
                  c("supp" = "supp", 
                    "dose" = "dose")),
      selectInput("y_var", "Select Y Variable:", 
                  c("len" = "len"))
    ),
    mainPanel(
      plotlyOutput("tooth_plot")
    )
  )
)

server <- function(input, output) {
  
  output$tooth_plot <- renderPlotly({
    p <- ggboxplot(ToothGrowth, x = input$x_var, y = input$y_var,
                   color = input$x_var, palette = "jco",
                   add = "jitter")
    
    if (input$test_method == "t.test") {
      levels <- levels(ToothGrowth[[input$x_var]])
      if (length(levels) > 2) {
        p <- p + stat_compare_means(comparisons = list(c("OJ", "VC"), c("OJ", "CS"), c("VC", "CS")),
                                    method = "t.test")
        p <- annotate_pval(p, method = "t.test")
      } else {
        p <- p + stat_compare_means(method = "t.test")
      }
    } else if (input$test_method == "anova") {
      p <- p + stat_compare_means(method = "anova")
    } else if (input$test_method == "kruskal.test") {
      p <- p + stat_compare_means(method = "kruskal.test")
    }
    ggplotly(p)
  })
  
}

shinyApp(ui, server)
