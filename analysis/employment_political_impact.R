library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(shinyjs)
library(shinyWidgets)

# Load and preprocess the data for both apps
file_path <- "PATH TO DATA" #Copied into an Excel workbook from here: https://www.bls.gov/web/empsit/cesnaicsrev.htm
data <- read_excel(file_path, skip = 3, col_names = FALSE)

valid_months <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")
data <- data %>% filter(...1 %in% valid_months)
colnames(data) <- c("Month", "Year", 
                    "SA_1st", "SA_2nd", "SA_3rd", 
                    "SA_2nd_minus_1st", "SA_3rd_minus_2nd", "SA_3rd_minus_1st", 
                    "NSA_1st", "NSA_2nd", "NSA_3rd", 
                    "NSA_2nd_minus_1st", "NSA_3rd_minus_2nd", "NSA_3rd_minus_1st")

data <- data %>%
  mutate(
    Year = as.numeric(gsub("[^0-9]", "", Year)),
    across(starts_with("SA_"), ~ as.numeric(gsub("[^0-9\\-]", "", .))),
    across(starts_with("NSA_"), ~ as.numeric(gsub("[^0-9\\-]", "", .)))
  ) %>%
  filter(!is.na(Year) & !is.na(Month)) %>%
  filter(Year != 2024) # Filter out the year 2024

final_data <- data %>%
  pivot_longer(cols = -c(Month, Year),
               names_to = c("Adjustment", "Estimate"),
               names_pattern = "(SA|NSA)_(.*)",
               values_to = "Value") %>%
  mutate(
    Estimate = case_when(
      Estimate == "1st" ~ "First",
      Estimate == "2nd" ~ "Second",
      Estimate == "3rd" ~ "Third",
      TRUE ~ Estimate
    )
  )

# Data for the first app (Month-over-Month Job Estimate Changes)
final_third_estimate <- final_data %>%
  filter(Estimate == "Third" & !is.na(Value)) %>%
  arrange(Year, match(Month, valid_months)) %>%
  group_by(Adjustment) %>%
  mutate(
    Monthly_Change = case_when(
      Month == "Jan." ~ Value - lag(Value, order_by = Year, default = NA),  
      TRUE ~ Value - lag(Value)
    ),
    Monthly_Change = ifelse(Month == "Jan." & is.na(Monthly_Change), 
                            Value - lag(Value, 12, order_by = Year), 
                            Monthly_Change)
  ) %>%
  ungroup()

yearly_totals <- final_third_estimate %>%
  group_by(Year, Adjustment) %>%
  summarise(Total_Change = sum(Monthly_Change, na.rm = TRUE)) %>%
  ungroup()

# Data for the second app (Heatmap)
presidents_data <- read_excel("YOUR PATH HERE") # Made an Excel from wikipedia: https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States
final_third_estimate <- final_third_estimate %>%
  left_join(presidents_data, by = "Year")

global_max_abs <- max(abs(final_third_estimate$Value), na.rm = TRUE)
final_third_estimate <- final_third_estimate %>%
  mutate(NormalizedValue = sign(Value) * log10(1 + abs(Value)) / log10(1 + global_max_abs))

common_scale <- scale_fill_gradient2(
  low = "blue", 
  mid = "white", 
  high = "red", 
  midpoint = 0, 
  name = "Normalized Value", 
  limits = c(-1, 1),  
  na.value = "grey"
)

# Combined Shiny UI
ui <- fluidPage(
  titlePanel("Comprehensive Economic Data Analysis"),
  tabsetPanel(
    tabPanel("Job Estimate Changes",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_years", "Select Year(s):", 
                             choices = unique(final_third_estimate$Year), 
                             selected = unique(final_third_estimate$Year)[1], 
                             multiple = TRUE),
                 selectInput("selected_adjustment", "Select Adjustment:", 
                             choices = c("SA" = "SA", "NSA" = "NSA"), 
                             selected = "SA", multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput("job_plot", height = "600px", width = "100%"),
                 textOutput("annual_total"),
                 p("Note: All values are in thousands.")
               )
             )
    ),
    tabPanel("Heatmap Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("presidentInput", "Choose Presidents:", 
                             choices = unique(final_third_estimate$President), 
                             selected = unique(final_third_estimate$President),
                             multiple = TRUE),
                 checkboxInput("facetParty", "Separate Plots by Political Party", TRUE),
                 selectInput("adjustmentInput", "Select Adjustment Type:", 
                             choices = c("SA", "NSA"), 
                             selected = "SA"),
                 sliderInput("thresholdInput", "Change Display Threshold:",
                             min = 0, max = 500, value = 100, step = 10)
               ),
               mainPanel(
                 plotlyOutput("economicPlotTop", width = "100%", height = "800px")
               )
             )
    ),
    tabPanel("Description",
             fluidRow(
               column(12,
                      HTML("
        <h3>Data Sources</h3>
        <ul>
          <li>Employment Change Data: <a href='https://www.bls.gov/web/empsit/cesnaicsrev.htm' target='_blank'>Bureau of Labor Statistics</a></li>
          <li>Political Party Data: <a href='https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States' target='_blank'>Wikipedia: List of U.S. Presidents</a></li>
        </ul>
        
        <h3>ETL Process</h3>
        <p>The data is extracted from two sources: employment change data from the Bureau of Labor Statistics and presidential data from Wikipedia. The employment data is filtered to remove unnecessary rows, and the columns are renamed for clarity. The data is then pivoted to a long format, making it easier to handle different estimates (1st, 2nd, and 3rd) and adjustments (SA and NSA).</p>
        <p>SA (Seasonally Adjusted) data accounts for seasonal patterns in employment, providing a clearer view of the underlying trends. NSA (Not Seasonally Adjusted) data reflects raw numbers, without adjustments for seasonal employment patterns.</p>
        
        <h3>Calculations</h3>
        <p>For the first visualization, the month-over-month change in employment is calculated using the third estimate of the data. The change for January is calculated using the previous year's December value to account for the transition between years. Additionally, the total yearly change is calculated by summing these monthly changes.</p>
        <p>For the second visualization, the data is normalized using a log transformation to facilitate comparison across months and years. The data is then joined with the presidential data to incorporate political party information.</p>
        
        <h3>UI Parameters for the Job Estimate Changes Tab</h3>
        <ul>
          <li><strong>Select Year(s):</strong> Choose one or more years to analyze job estimate changes over that period. The plot will display the changes month by month, and each year selected will have its own plot.</li>
          <li><strong>Select Adjustment:</strong> Choose between Seasonally Adjusted (SA) and Not Seasonally Adjusted (NSA) data. This choice affects how the data is displayed, with SA data smoothing out seasonal fluctuations and NSA data showing raw changes.</li>
        </ul>
        
        <h3>UI Parameters for the Heatmap Analysis Tab</h3>
        <ul>
          <li><strong>Choose Presidents:</strong> Select one or more U.S. Presidents to focus the analysis on their terms. The data for each selected president will be highlighted in the heatmap.</li>
          <li><strong>Separate Plots by Political Party:</strong> When enabled, this option separates the heatmap into different plots based on the political party of the presidents selected. This allows for a clearer comparison between parties.</li>
          <li><strong>Select Adjustment Type:</strong> Choose between SA and NSA data, similar to the first tab, to see the effect of seasonal adjustments on the heatmap.</li>
          <li><strong>Change Display Threshold:</strong> Adjust the threshold to filter out minor changes, showing only those monthly changes that exceed the specified value. This helps to highlight significant employment changes.</li>
        </ul>
        
        <h3>Raw Data Structure</h3>
        <p>The raw data used in this analysis includes the following columns:</p>
        <ul>
          <li><strong>Month:</strong> The month of the year (e.g., Jan., Feb.).</li>
          <li><strong>Year:</strong> The corresponding year of the data point.</li>
          <li><strong>SA_1st, SA_2nd, SA_3rd:</strong> First, second, and third estimates of seasonally adjusted data.</li>
          <li><strong>NSA_1st, NSA_2nd, NSA_3rd:</strong> First, second, and third estimates of not seasonally adjusted data.</li>
          <li><strong>SA_2nd_minus_1st, SA_3rd_minus_2nd, SA_3rd_minus_1st:</strong> Differences between subsequent estimates for seasonally adjusted data.</li>
          <li><strong>NSA_2nd_minus_1st, NSA_3rd_minus_2nd, NSA_3rd_minus_1st:</strong> Differences between subsequent estimates for not seasonally adjusted data.</li>
        </ul>
      ")
      )
     )
    )
  )
)

# Combined Shiny Server
server <- function(input, output, session) {
  
  # First Tab: Job Estimate Changes
  observe({
    if (length(input$selected_years) == 0) {
      updateSelectInput(session, "selected_years", 
                        selected = unique(final_third_estimate$Year)[1])
    }
  })
  
  observe({
    if (length(input$selected_adjustment) == 0) {
      updateSelectInput(session, "selected_adjustment", 
                        selected = "SA")
    }
  })
  
  output$job_plot <- renderPlotly({
    selected_data <- final_third_estimate %>%
      filter(Year %in% input$selected_years & Adjustment %in% input$selected_adjustment) %>%
      mutate(Change_Direction = ifelse(Monthly_Change > 0, "Overstated", "Understated"),
             Legend_Label = paste(Adjustment, "-", Change_Direction))
    
    color_mapping <- c(
      "SA - Overstated" = "#0072B2", "SA - Understated" = "#56B4E9",
      "NSA - Overstated" = "#D55E00", "NSA - Understated" = "#E69F00"
    )
    
    p <- ggplot(selected_data, aes(x = Month, y = Monthly_Change, fill = Legend_Label, text = paste("Month:", Month, "<br>Change:", Monthly_Change, "<br>Type:", Adjustment))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = color_mapping, name = "Adjustment & Status") +
      geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
      labs(title = "Month-over-Month Job Estimate Changes",
           x = "Month",
           y = "Change in Jobs (Third Estimate)") +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold")) +
      facet_wrap(~ Year, scales = "free_x")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(title = list(text = "Adjustment & Status")),
             margin = list(l = 50, r = 50, b = 100, t = 100),
             hoverlabel = list(font = list(size = 12)))
  })
  
  output$annual_total <- renderText({
    selected_totals <- yearly_totals %>%
      filter(Year %in% input$selected_years & Adjustment %in% input$selected_adjustment) %>%
      summarise(Total_Change = sum(Total_Change)) %>%
      pull(Total_Change)
    
    total_change_rounded <- sprintf("%.1f", round(selected_totals, 1))
    
    paste("Grand Total Change:", total_change_rounded, "K")
  })
  
  # Second Tab: Heatmap Analysis
  observe({
    selected_parties <- unique(final_third_estimate$Party[final_third_estimate$President %in% input$presidentInput])
    
    if (length(selected_parties) <= 1) {
      updateCheckboxInput(session, "facetParty", value = FALSE)
    }
  })
  
  filtered_data <- reactive({
    final_third_estimate %>%
      filter(President %in% input$presidentInput,
             Adjustment == input$adjustmentInput)
  })
  
  output$economicPlotTop <- renderPlotly({
    data <- filtered_data()
    
    data$Month <- factor(data$Month, levels = valid_months)
    
    n_rows <- length(unique(data$SeqID))
    plot_height <- 800 + n_rows * 60  
    
    if(input$facetParty && length(unique(data$Party)) > 1) {
      parties <- unique(data$Party)
      data_party1 <- data %>% filter(Party == parties[1])
      data_party2 <- data %>% filter(Party == parties[2])
      
      complete_grid <- expand.grid(
        SeqID = unique(data$SeqID),
        Month = factor(valid_months, levels = valid_months)
      )
      
      data_party1 <- left_join(complete_grid, data_party1, by = c("SeqID", "Month")) %>%
        arrange(SeqID, Month)
      
      data_party2 <- left_join(complete_grid, data_party2, by = c("SeqID", "Month")) %>%
        arrange(SeqID, Month)
      
      p1 <- ggplot(data_party1, aes(x = Month, y = SeqID, fill = NormalizedValue)) +
        geom_tile(color = "white", height = 0.9) +  
        geom_text(aes(label = ifelse(!is.na(Monthly_Change) & abs(Monthly_Change) > input$thresholdInput, 
                                     sprintf("%.0f", abs(Monthly_Change)), "")), 
                  vjust = 0.5, size = 3.5, color = "black") +  
        common_scale +  
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        labs(
          x = "Month",
          y = "SeqID",
          fill = "Normalized Value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.position = "right"
        )
      
      p2 <- ggplot(data_party2, aes(x = Month, y = SeqID, fill = NormalizedValue)) +
        geom_tile(color = "white", height = 0.9) +  
        geom_text(aes(label = ifelse(!is.na(Monthly_Change) & abs(Monthly_Change) > input$thresholdInput, 
                                     sprintf("%.0f", abs(Monthly_Change)), "")), 
                  vjust = 0.5, size = 3.5, color = "black") +  
        common_scale +  
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        labs(
          x = "Month",
          y = "SeqID",
          fill = "Normalized Value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "none"
        )
      
      combined_plot <- subplot(
        ggplotly(p1, height = plot_height),
        ggplotly(p2, height = plot_height),
        nrows = 2, margin = 0.05, shareX = TRUE, shareY = TRUE
      ) %>%
        layout(
          autosize = TRUE,
          annotations = list(
            list(
              x = 0.5,
              y = 1.02,
              text = paste0("Economic Data Analysis - ", parties[1]),
              showarrow = FALSE,
              xref = 'paper',
              yref = 'paper',
              font = list(size = 16)
            ),
            list(
              x = 0.5,
              y = 0.51,
              text = paste0("Economic Data Analysis - ", parties[2]),
              showarrow = FALSE,
              xref = 'paper',
              yref = 'paper',
              font = list(size = 16)
            )
          ),
          margin = list(l = 50, r = 50, b = 100, t = 80, pad = 4),
          showlegend = FALSE
        )
      
      combined_plot
      
    } else {
      p <- ggplot(data, aes(x = Month, y = Year, fill = NormalizedValue)) +
        geom_tile(color = "white", height = 0.9) +  
        geom_text(aes(label = ifelse(abs(Monthly_Change) > input$thresholdInput, 
                                     sprintf("%.0f", abs(Monthly_Change)), "")), 
                  vjust = 0.5, size = 3.5, color = "black") +  
        common_scale +  
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        labs(
          title = "Economic Data Analysis by Month and Year",
          x = "Month",
          y = "Year",
          fill = "Normalized Value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.position = "right"
        )
      
      ggplotly(p, height = plot_height) %>%
        layout(
          autosize = TRUE,
          margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4),
          showlegend = TRUE
        )
    }
  })
}

# Run the combined app
shinyApp(ui = ui, server = server)
