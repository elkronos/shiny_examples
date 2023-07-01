library(shiny)
library(ggplot2)
library(dplyr)
library(viridis)

# Confirm factors
diamonds <- diamonds %>%
  mutate(cut = factor(cut),
         color = factor(color),
         clarity = factor(clarity))

# Define UI
ui <- fluidPage(
  # Select X variable
  selectInput("x_var", "Select X variable", choices = names(diamonds), selected = "price"),
  
  # Select color/group/fill variable (only allow factor variables)
  selectInput("color_var", "Select color/group/fill variable",
              choices = names(diamonds)[sapply(diamonds, is.factor)],
              selected = "cut"),
  
  # Select density type
  radioButtons("density_type", "Select density type",
               choices = list("Density" = "density",
                              "Stacked density" = "stacked_density",
                              "Small multiple density" = "small_multiple_density")),
  
  # Output plot
  plotOutput("density_plot")
)

# Define server
server <- function(input, output) {
  output$density_plot <- renderPlot({
    # Create plot based on user inputs
    plot_data <- diamonds %>%
      ggplot(aes(x = !!sym(input$x_var), color = !!sym(input$color_var), fill = !!sym(input$color_var))) +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE)
    
    if (input$density_type == "density") {
      plot_data <- plot_data + geom_density(alpha = 0.6)
    } else if (input$density_type == "stacked_density") {
      plot_data <- plot_data + geom_density(adjust = 1.5, position = "fill")
    } else if (input$density_type == "small_multiple_density") {
      plot_data <- plot_data + geom_density(adjust = 1.5) +
        facet_wrap(vars(!!sym(input$color_var))) +
        theme(legend.position = "none",
              panel.spacing = unit(0.1, "lines"),
              axis.ticks.x = element_blank())
    }
    
    plot_data
  })
}

# Run the app
shinyApp(ui, server)
