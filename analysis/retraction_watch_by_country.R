# Get data here: https://www.crossref.org/blog/news-crossref-and-retraction-watch/
# See analysis script here: https://github.com/elkronos/public_examples/blob/main/examples/retraction_watch_analysis.R

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)
library(stringr)
library(forcats)

# Load your data
data <- read_csv("C:/Users/JChas/OneDrive/Desktop/exampleshiny/data.csv",
                 show_col_types = FALSE)

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

melted_data$variable <- gsub("_", " ", melted_data$variable)
melted_data$variable <- factor(melted_data$variable, levels = sort(unique(melted_data$variable)))

ordered_categories <- c("Research Quality and Integrity", "Publication and Review Processes", 
                        "Plagiarism and Duplication", "Legal and Official Investigations", 
                        "External Influences and Rights", "Data Integrity", 
                        "Authorship and Ethical Concerns")
melted_data$variable <- factor(melted_data$variable, levels = ordered_categories)

# Create a vector of countries for dropdown
country_list <- unique(melted_data$Country)

# UI Section
ui <- fluidPage(
  titlePanel("Retraction Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Choose a Country:", choices = country_list)
    ),
    mainPanel(
      plotOutput("countryPlot")
    )
  )
)

# Server Section
server <- function(input, output) {
  output$countryPlot <- renderPlot({
    country_data <- melted_data %>% filter(Country == input$countryInput)
    median_value <- median(country_data$value, na.rm = TRUE)
    
    ggplot(country_data, aes(x = RetractionYear, y = variable, fill = value)) +
      geom_tile(color = "white") +  # Add borders to the tiles
      geom_text(aes(label = sprintf("%.1f%%", value)), vjust = 1, size = 3, color = "black") +
      scale_fill_gradient2(low = "steelblue", mid = "white", high = "salmon", midpoint = median_value) +
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
  })
}

# Run the application
shinyApp(ui = ui, server = server)