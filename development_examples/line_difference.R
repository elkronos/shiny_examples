library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(geofacet)
library(ggplot2)
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
      plotOutput("rescue_plot")
    )
  )
)

server <- function(input, output) {
  output$rescue_plot <- renderPlot({
    df <- rescues_borough %>%
      filter(name == input$borough)
    ggplot(df, aes(x = cal_year)) +
      geom_line(aes(y = Cat, color = "cats")) +
      geom_line(aes(y = Not_Cat, color = "other")) +
      geom_ribbon(aes(ymin = Not_Cat, ymax = Cat, fill = "difference"), alpha = 0.3) +
      scale_fill_manual(values = "grey") +
      ggtitle(input$borough)
  })
}

shinyApp(ui = ui, server = server)
