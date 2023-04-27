library(shiny)
library(ggplot2)
library(usmap)
library(plotly)

ui <- fluidPage(
  titlePanel("US Poverty Percentage Estimates"),
  sidebarLayout(
    sidebarPanel(
      selectInput("abbr", "Select State:", 
                  c("AL" = "Alabama", 
                    "AK" = "Alaska", 
                    "AR" = "Arkansas", 
                    "AZ" = "Arizona", 
                    "CA" = "California", 
                    "CO" = "Colorado", 
                    "CT" = "Connecticut", 
                    "DE" = "Delaware", 
                    "FL" = "Florida", 
                    "GA" = "Georgia", 
                    "HI" = "Hawaii", 
                    "ID" = "Idaho", 
                    "IL" = "Illinois", 
                    "IN" = "Indiana", 
                    "IA" = "Iowa", 
                    "KS" = "Kansas", 
                    "KY" = "Kentucky", 
                    "LA" = "Louisiana", 
                    "ME" = "Maine",
                    "MD" = "Maryland",
                    "MA" = "Massachusetts", 
                    "MI" = "Michigan", 
                    "MN" = "Minnesota",
                    "MS" = "Mississippi",
                    "MO" = "Missouri", 
                    "MT" = "Montana", 
                    "NE" = "Nebraska", 
                    "NH" = "New Hampshire", 
                    "NJ" = "New Jersey", 
                    "NM" = "New Mexico", 
                    "NY" = "New York", 
                    "NV" = "Nevada", 
                    "NC" = "North Carolina", 
                    "ND" = "North Dakota", 
                    "OH" = "Ohio", 
                    "OK" = "Oklahoma",
                    "OR" = "Oregon",
                    "PA" = "Pennsylvania", 
                    "RI" = "Rhode Island", 
                    "SC" = "South Carolina",
                    "SD" = "South Dakota", 
                    "TN" = "Tennessee", 
                    "TX" = "Texas", 
                    "UT" = "Utah", 
                    "VT" = "Vermont",
                    "VA" = "Virginia", 
                    "WA" = "Washington",
                    "WV" = "West Virginia", 
                    "WI" = "Wisconsin", 
                    "WY" = "Wyoming")
      )
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    selected_abbr <- input$abbr
    gg <- plot_usmap(data = countypov, values = "pct_pov_2014", include = selected_abbr, color = "blue") + 
      scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 
      labs(title = "Poverty Percentage Estimates", subtitle = paste("for", selected_abbr, "in 2014")) +
      theme(legend.position = "right")
    
    ggplotly(gg)
  })
}

shinyApp(ui, server)
