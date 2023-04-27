library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(geofacet)
library(plotly)
library(ggrepel)
library(ggforce)

animal_rescues <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv"
) %>% 
  # Capitalize the type of animal
  mutate(animal_group_parent = str_to_sentence(animal_group_parent))

borough_names <- gb_london_boroughs_grid %>% 
  select(borough_code = code_ons, name)

rescues_borough <- animal_rescues %>% 
  filter(cal_year < 2021) %>% 
  mutate(animal_group_parent = if_else(animal_group_parent == "Cat", "Cat", "Not_Cat")) %>% 
  count(cal_year, borough_code, animal_group_parent) %>% 
  pivot_wider(names_from = animal_group_parent, values_from = n) %>% 
  left_join(borough_names) %>% 
  filter(!is.na(name)) 

ui <- fluidPage(
  titlePanel("London Animal Rescues"),
  sidebarLayout(
    sidebarPanel(
      selectInput("borough", "Select a borough:", choices = unique(rescues_borough$name))
    ),
    mainPanel(
      plotlyOutput("rescue_plot")
    )
  )
)

server <- function(input, output) {
  output$rescue_plot <- renderPlotly({
    df <- rescues_borough %>%
      filter(name == input$borough)
    plot_ly(df, x = ~cal_year) %>% 
      add_lines(y = ~Cat, name = "cats", line = list(color = "darkblue")) %>% 
      add_lines(y = ~Not_Cat, name = "other", line = list(color = "yellow")) %>% 
      add_ribbons(ymin = ~Not_Cat, ymax = ~Cat, name = "above_cat", fillcolor = "orange", opacity = 0.3, line = list(color = NA), showlegend = FALSE) %>% 
      add_ribbons(ymin = 0, ymax = ~Not_Cat, name = "below_cat", fillcolor = "green", opacity = 0.3, line = list(color = NA), showlegend = FALSE) %>% 
      layout(title = input$borough, xaxis = list(title = "Year"), yaxis = list(title = "Number of rescues"))
  })
}


shinyApp(ui = ui, server = server)
