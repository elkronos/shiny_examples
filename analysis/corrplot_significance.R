library(shiny)
library(corrplot)

ui <- fluidPage(
  titlePanel("Correlation Plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("method", "Method:", 
                  c("Circle" = "circle", "Pie" = "pie", "Color" = "color", "Number" = "number")),
      checkboxInput("add_pvals", "See P-Values?")
    ),
    
    mainPanel(
      plotOutput("corplot")
    )
  )
)

server <- function(input, output) {
  output$corplot <- renderPlot({
    M <- cor(mtcars)
    corrplot(M, method = input$method)
    if (input$add_pvals) {
      cor.mtest <- function(mat, ...) {
        mat <- as.matrix(mat)
        n <- ncol(mat)
        p.mat<- matrix(NA, n, n)
        diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
          }
        }
        colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
        p.mat
      }
      
      p.mat <- cor.mtest(mtcars)
      corrplot(M, type = "full", method = input$method, order = "hclust", p.mat = p.mat, sig.level = 0.01)
    }
  })
}

shinyApp(ui, server)
