library(shiny)
library(ggplot2)
library(usmap) 


ui <- fluidPage(
  titlePanel("US Poverty Percentage Estimates"),
  sidebarLayout(
    sidebarPanel(
      selectInput("abbr", "Select State:", 
                  c("AL" = "Alabama", 
                    "AR" = "Arkansas", 
                    "AZ" = "Arizona", 
                    "CA" = "California", 
                    "CO" = "Colorado", 
                    "CT" = "Connecticut", 
                    "FL" = "Florida", 
                    "GA" = "Georgia", 
                    "ID" = "Idaho", 
                    "KY" = "Kentucky", 
                    "LA" = "Louisiana", 
                    "ME" = "Maine",
                    "MD" = "Maryland",
                    "MA" = "Massachusetts", 
                    "MS" = "Mississippi", 
                    "NH" = "New Hampshire", 
                    "NJ" = "New Jersey", 
                    "NM" = "New Mexico", 
                    "NY" = "New York", 
                    "NV" = "Nevada", 
                    "NC" = "North Carolina", 
                    "PA" = "Pennsylvania", 
                    "SC" = "South Carolina", 
                    "TN" = "Tennessee", 
                    "UT" = "Utah", 
                    "VT" = "Vermont",
                    "VA" = "Virginia", 
                    "WV" = "West Virginia", 
                    "WY" = "Wyoming")
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    selected_abbr <- input$abbr
    plot_usmap(data = countypov, values = "pct_pov_2014", include = selected_abbr, color = "blue") + 
      scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 
      labs(title = "Poverty Percentage Estimates", subtitle = paste("for", selected_abbr, "in 2014")) +
      theme(legend.position = "right")
  })
}

shinyApp(ui, server)
