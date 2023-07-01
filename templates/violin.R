# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(shiny)

# create a dataset
data <- data.frame(
  name = factor(c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  )),
  group = factor(c( rep("Fall",500), rep("Spring",500), rep("Winter",500), rep("Summer",100), rep('Unknown', 20))),
  performance = c(rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1)),
  value = c(rnorm(500, 25, 2), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1)) 
)

# sample size
sample_size = data %>% group_by(name) %>% summarize(num=n())

# UI
ui <- fluidPage(
  titlePanel("Violin wrapping a boxplot"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x_var", label = "Select X variable:",
                  choices = names(Filter(is.factor, data)), selected = names(Filter(is.factor, data))[1]),
      selectInput(inputId = "y_var", label = "Select Y variable:",
                  choices = names(Filter(is.numeric, data)), selected = names(Filter(is.numeric, data))[1])
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(is.numeric(!!sym(input$y_var))) %>%
      filter(is.factor(!!sym(input$x_var))) 
  })
  
  sample_size <- reactive({
    filtered_data() %>%
      group_by(!!sym(input$x_var)) %>%
      summarize(num = n())
  })
  
  output$plot <- renderPlot({
    filtered_data() %>%
      left_join(sample_size()) %>%
      mutate(myaxis = paste0(!!sym(input$x_var), "\n", "n=", num)) %>%
      ggplot(aes(x=myaxis, y=!!sym(input$y_var), fill=!!sym(input$x_var))) +
      geom_violin(width=1.4) +
      geom_boxplot(width=0.1, color="grey", alpha=0.2) +
      scale_fill_viridis(discrete = TRUE) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("A Violin wrapping a boxplot") +
      xlab("")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
