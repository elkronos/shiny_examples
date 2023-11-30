# Get data here: https://www.crossref.org/blog/news-crossref-and-retraction-watch/
# See analysis script here: https://github.com/elkronos/public_examples/blob/main/examples/retraction_watch_analysis.R
# Hosted here: https://elkronos.shinyapps.io/Retractions/

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)
library(stringr)
library(forcats)
library(rlang)
library(plotly)
library(shinyWidgets)

setwd("C:/your/path")
data <- read_csv("data.csv")

# Ensure required columns are present
required_columns <- c('RetractionDate', 'Country', 'Reason')
if (!all(required_columns %in% colnames(data))) {
  stop("Required column not found in the dataset")
}

# Data processing
data <- data %>%
  mutate(Country = str_split(Country, ";")) %>%
  unnest(Country) %>%
  filter(Country != "Unknown")

data$RetractionYear <- year(mdy_hm(data$RetractionDate, tz="UTC", quiet = TRUE))

# Define categories and their corresponding keywords
categories <- list(
  Plagiarism_and_Duplication = c("+Duplication of Article", "+Plagiarism of Data", "+Plagiarism of Text",
                                 "+Euphemisms for Plagiarism", "+Euphemisms for Duplication",
                                 "+Concerns about Referencing/Attributions", "+Taken from Dissertation/Thesis",
                                 "+Duplication of Text"),
  Data_Integrity = c("+Falsification/Fabrication of Data", "+Unreliable Data", "+Error in Data",
                     "+Original Data not Provided", "+Duplication of Image", "+Manipulation of Images"),
  Authorship_and_Ethical_Concerns = c("+Concerns about Authorship", "+Objections by Author(s)", "+Withdrawal",
                                      "+False/Forged Authorship", "+False Affiliation", "+Complaints about Author",
                                      "+Conflict of Interest", "+Ethical Violations by Author",
                                      "+Lack of IRB/IACUC Approval", "+Informed/Patient Consent - None/Withdrawn"),
  Publication_and_Review_Processes = c("+Duplicate Publication through Error by Journal/Publisher",
                                       "+Publishing Ban", "+Complaints about Company/Institution",
                                       "+Complaints about Third Party", "+Fake Peer Review", "+Paper Mill",
                                       "+Concerns/Issues with Peer Review"),
  Research_Quality_and_Integrity = c("+Error in Text", "+Unreliable Results", "+Concerns/Issues About Results",
                                     "+Contamination of Materials", "+Error in Analyses", "+Results Not Reproducible",
                                     "+Error in Results and/or Conclusions", "+Hoax Paper", "+Bias Issues or Lack of Balance",
                                     "+Randomly Generated Content"),
  Legal_and_Official_Investigations = c("+Investigation by Journal/Publisher", "+Investigation by Company/Institution",
                                        "+Investigation by Third Party", "+Criminal Proceedings", "+Civil Proceedings",
                                        "+Investigation by ORI", "+Legal Reasons/Legal Threats", "+Misconduct - Official Investigation/Finding"),
  External_Influences_and_Rights = c("+Lack of Approval from Third Party", "+Transfer of Copyright/Ownership",
                                     "+Copyright Claims", "+Rogue Editor")
)

# Create new categories with counts for each article
for (category in names(categories)) {
  data[[category]] <- sapply(data$Reason, function(x) {
    keyword_in_x <- sapply(categories[[category]], function(keyword) grepl(keyword, x))
    as.integer(any(keyword_in_x))
  })
}

recent_data <- data %>% 
  filter(RetractionYear >= max(RetractionYear, na.rm = TRUE) - 10)

country_counts <- recent_data %>% count(Country) %>% filter(n >= 250)
data_filtered <- recent_data %>% filter(Country %in% country_counts$Country)

grouped_data <- data_filtered %>% 
  group_by(Country, RetractionYear) %>% 
  summarise(across(all_of(names(categories)), sum, na.rm = TRUE)) %>%
  group_by(Country, RetractionYear) %>%
  mutate(total = rowSums(across(all_of(names(categories))))) %>%
  mutate(across(all_of(names(categories)), ~ . / total * 100)) %>%
  select(-total)

melted_data <- melt(grouped_data, id.vars = c("Country", "RetractionYear"))

# Calculate Grand Median for Each Category
grand_medians <- melted_data %>%
  group_by(variable) %>%
  summarise(grand_median = median(value, na.rm = TRUE))

# Join the grand medians with the melted data
melted_data <- left_join(melted_data, grand_medians, by = "variable")

# Calculate the difference from the grand median
melted_data$diff_from_grand_median <- melted_data$value - melted_data$grand_median

melted_data$variable <- gsub("_", " ", melted_data$variable)
melted_data$variable <- factor(melted_data$variable, levels = sort(unique(melted_data$variable)))

ordered_categories <- c("Research Quality and Integrity", "Publication and Review Processes", 
                        "Plagiarism and Duplication", "Legal and Official Investigations", 
                        "External Influences and Rights", "Data Integrity", 
                        "Authorship and Ethical Concerns")
melted_data$variable <- factor(melted_data$variable, levels = ordered_categories)

# Create a vector of countries for dropdown
country_list <- unique(melted_data$Country)

# saveRDS(melted_data, file = "melted_data.rds")
# saveRDS(country_list, file = "country_list.rds")

# Shiny UI
ui <- fluidPage(
  titlePanel("Retraction Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Choose a Country:", choices = unique(melted_data$Country)),
      checkboxInput("toggleView", "Toggle Grand Median Difference View", FALSE),
      pickerInput(
        "yearInput",
        "Select Years:",
        choices = unique(melted_data$RetractionYear),
        selected = unique(melted_data$RetractionYear)[(length(unique(melted_data$RetractionYear))-4):length(unique(melted_data$RetractionYear))],
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Deselect all", `select-all-text` = "Select all")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", plotlyOutput("countryPlot", width = "100%", height = "600px")),
        tabPanel("Analysis Details", 
                 HTML("<h3>Dashboard Overview</h3>
                 <p>This dashboard, leveraging the Retraction Watch database, visualizes the distribution of retracted journal articles across different countries, categorized by reasons for retraction and year. The data, updated as of November 27, 2023, encompasses over 48,000 retractions. More details about the Retraction Watch initiative can be found <a href='https://www.crossref.org/blog/news-crossref-and-retraction-watch/' target='_blank'>here</a>.</p>
                 <p>The default view shows the percentage of retractions, color-coded by the median value for each country. A toggle feature enables comparison with the grand median across the entire dataset.</p>
                 <h3>Key Considerations</h3>
                 <ol>
                  <li>The categorization of retraction reasons, while subjective, can be customized. The ETL and Shiny app code are available <a href='https://github.com/elkronos/shiny_examples/blob/main/analysis/retraction_watch_by_country.R' target='_blank'>here</a> for further exploration.</li>
                  <li>Analysis excludes 'null' or 'unknown' countries. Only countries with 250+ retractions in the last decade are included, balancing aesthetic and UI simplicity.</li>
                  <li>The purpose of this dashboard is analytical, not punitive. It aims to uncover patterns in retraction reasons, which may vary by country due to diverse factors.</li>
                  <li>Each article is attributed to all listed countries, assumed to be the countries of the affiliated authors. Future enhancements may include weighting by author order.</li>
                </ol>")
        )
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  output$countryPlot <- renderPlotly({
    # Filter data based on selected country and years
    country_data <- melted_data %>% 
      filter(Country == input$countryInput, RetractionYear %in% input$yearInput)
    
    # Decide which value to use based on toggle
    value_column <- ifelse(input$toggleView, "diff_from_grand_median", "value")
    label_format <- ifelse(input$toggleView, "%.2f", "%.1f%%")
    fill_midpoint <- if(input$toggleView) 0 else median(country_data[[value_column]], na.rm = TRUE)
    
    p <- ggplot(country_data, aes(x = RetractionYear, y = variable, fill = .data[[value_column]])) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf(label_format, .data[[value_column]])), vjust = 1, size = 3, color = "black") +
      scale_fill_gradient2(low = "steelblue", mid = "white", high = "salmon", midpoint = fill_midpoint) +
      labs(
        title = paste("Retraction Reason Distribution for", input$countryInput),
        x = "Year",
        y = "Category",
        fill = "Percentage"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      )
    
    ggplotly(p) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)