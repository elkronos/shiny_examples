library(shiny)
library(tidyverse)
library(simaerep)

ui <- fluidPage(
  titlePanel("Simulation Analysis of Incidents"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_patients", "Number of Patients:", value = 1000, min = 1),
      numericInput("n_sites", "Number of Sites:", value = 100, min = 1),
      numericInput("frac_site_ur", "Fraction of Sites Under Reporting:", value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("ur_rate", "Rate of Under Reporting:", value = 0.4, min = 0, max = 1, step = 0.01),
      numericInput("ae_mean", "Mean AE per Patient Visit:", value = 0.5, min = 0),
      actionButton("generate", "Generate Simulation")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$generate, {
    # Generate the simulation based on user input
    df_visit <- sim_test_data_study(
      n_pat = input$n_patients,
      n_sites = input$n_sites,
      frac_site_with_ur = input$frac_site_ur,
      ur_rate = input$ur_rate,
      ae_per_visit_mean = input$ae_mean
    )
    df_visit$study_id <- "A"
    
    # Run the simulation and generate the plot
    aerep <- simaerep(df_visit)
    output$plot <- renderPlot({
      plot(aerep, study = "A")
    })
  })
}

shinyApp(ui = ui, server = server)