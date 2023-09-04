library(shiny)
library(tidyverse)
library(geofacet)
library(ggh4x)
library(DT)

# Load the data
animal_rescues <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv"
) %>% 
  mutate(animal_group_parent = str_to_sentence(animal_group_parent))

# Load the grid layout data and select the relevant columns
borough_names <- geofacet::gb_london_boroughs_grid %>% 
  select(borough_code = code_ons, name)

# Process the data as per your initial script
rescues_borough <- animal_rescues %>% 
  filter(cal_year < 2021) %>% 
  mutate(animal_group_parent = if_else(animal_group_parent == "Cat", "Cat", "Not_Cat")) %>% 
  count(cal_year, borough_code, animal_group_parent) %>% 
  pivot_wider(names_from = animal_group_parent, values_from = n) %>% 
  left_join(borough_names) %>% 
  filter(!is.na(name)) 

# Define UI
ui <- fluidPage(
  titlePanel("Animal Rescues Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      hr(),
      selectInput("borough", "Select Borough", choices = c("All", unique(rescues_borough$name)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Data View", 
                 verbatimTextOutput("data_view"),
                 DT::dataTableOutput("sample_data"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  data <- reactiveVal(rescues_borough)
  
  observeEvent(input$file1, {
    df <- read.csv(input$file1$datapath)
    data(df)
    updateSelectInput(session, "borough", choices = c("All", unique(df$name)))
  })
  
  output$data_view <- renderPrint({
    print("Sample data structure:")
    print("Your uploaded data must match this structure and contain a column called 'name' (case sensitive).")
    str(data())
  })
  
  output$sample_data <- DT::renderDataTable({
    head(data())
  })
  
  output$plot <- renderPlot({
    req(data())
    
    df <- data()
    
    if (input$borough != "All") {
      df <- df %>% filter(name == input$borough)
    }
    
    ggplot(df, aes(x = cal_year)) +
      geom_line(aes(y = Cat, color = "cats")) +
      geom_line(aes(y = Not_Cat, color = "other")) +
      stat_difference(aes(ymin = Not_Cat, ymax = Cat), alpha = 0.3) +
      if (input$borough == "All") facet_geo(vars(name), grid = "gb_london_boroughs_grid") else NULL
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
