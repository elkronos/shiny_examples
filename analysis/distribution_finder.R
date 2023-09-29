# Load packages
library(MASS)
library(ggplot2)
library(fitdistrplus)
library(shiny)

# Make example which can be reproduced
set.seed(123)

# Create a data frame with multiple columns to upload
# sampleData <- data.frame(
#   gamma = rgamma(1000, shape = 2, scale = 2),
#   exponential = rexp(1000, rate = 0.5),
#   poisson = rpois(1000, lambda = 3)
# )
# Write the data frame to a CSV file
# write.csv(sampleData, "sampleData.csv", row.names = FALSE)

# Density function
density_fun <- function(dist, x, params) {
  x <- x[!is.na(x)]
  if (dist == "gaussian") {
    return(dnorm(x, mean = params[1], sd = params[2]))
  } else if (dist == "poisson") {
    return(dpois(x, lambda = params[1]))
  } else if (dist == "binomial") {
    return(dbinom(x, size = params[1], prob = params[2]))
  } else if (dist == "exponential") {
    return(dexp(x, rate = params[1]))
  } else if (dist == "gamma") {
    return(dgamma(x, shape = params[1], rate = params[2]))
  } else if (dist == "log-normal") {
    return(dlnorm(x, meanlog = params[1], sdlog = params[2]))
  }
}

r_fun <- function(dist, n, params) {
  if (dist == "gaussian") {
    return(rnorm(n, mean = params[1], sd = params[2]))
  } else if (dist == "poisson") {
    if (is.na(params[1]) || params[1] <= 0) return(rep(NA, n))  # Return NAs if the lambda parameter is invalid
    return(rpois(n, lambda = params[1]))
  } else if (dist == "binomial") {
    return(rbinom(n, size = params[1], prob = params[2]))
  } else if (dist == "exponential") {
    return(rexp(n, rate = params[1]))
  } else if (dist == "geometric") {
    return(rgeom(n, prob = params[1]))
  } else if (dist == "gamma") {
    return(rgamma(n, shape = params[1], rate = params[2]))
  } else if (dist == "log-normal") {
    return(rlnorm(n, meanlog = params[1], sdlog = params[2]))
  }
}

plot_overlapping_histograms <- function(data, fitted_data, dist_name) {
  hist_df <- data.frame(Data = c(data, fitted_data),
                        Type = factor(rep(c("Observed", "Fitted"), c(length(data), length(fitted_data)))))
  
  plot <- ggplot(hist_df, aes(x = Data, fill = Type)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, position = "identity", alpha = 0.6) +
    scale_fill_manual(values = c("Observed" = "lightblue", "Fitted" = "red")) +
    theme_bw() +
    theme(
      text = element_text(size = 14),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 20, hjust = 0.5)
    ) +
    labs(title = paste("Best Distribution: ", dist_name))
  
  print(plot)  # Explicitly print the plot
}

find_distribution <- function(data_frame, column, plot = FALSE, use_bic = FALSE, return_all_info = FALSE) {
  data <- data_frame[[column]]
  distributions <- c("gaussian", "poisson", "exponential", "gamma", "log-normal")
  
  fits <- list()
  gof <- data.frame(dist = character(),
                    loglik = numeric(),
                    aic = numeric(),
                    bic = numeric(),
                    stringsAsFactors = FALSE)
  
  if (all(data >= 0)) {
    is_discrete <- all(data == round(data))
    has_zeros <- any(data == 0)
    
    for (dist in distributions) {
      if (dist == "gaussian") {
        fits[[dist]] <- fitdist(data, "norm")
      } else if (dist == "poisson") {
        if (is_discrete) {
          fits[[dist]] <- fitdist(data, "pois")
        }
      } else if (dist == "exponential") {
        fits[[dist]] <- fitdist(data, "exp")
      } else if (dist == "gamma") {
        if (!is_discrete) {
          fits[[dist]] <- fitdist(data, "gamma")
        }
      } else if (dist == "log-normal") {
        if (!has_zeros) {
          fits[[dist]] <- fitdist(data, "lnorm")
        }
      }
      
      if (!is.null(fits[[dist]])) {
        gof <- rbind(gof, data.frame(dist = dist,
                                     loglik = fits[[dist]]$loglik,
                                     aic = fits[[dist]]$aic,
                                     bic = fits[[dist]]$bic))
      }
    }
    
    if (nrow(gof) > 0) {
      if (use_bic) {
        best_dist <- gof[which.min(gof$bic), "dist"]
      } else {
        best_dist <- gof[which.min(gof$aic), "dist"]
      }
      
      if (plot) {
        fitted_data <- r_fun(best_dist, length(data), coef(fits[[best_dist]]))
        plot_overlapping_histograms(data, fitted_data, best_dist)
      }
      
      if (return_all_info) {
        return(list(best_distribution = best_dist, fits = fits, gof = gof))
      } else {
        return(best_dist)
      }
    }
  } else {
    stop("Data must be non-negative.")
  }
}

# Fake data
syntheticData <- data.frame(value = rgamma(1000, shape = 2, scale = 2))

ui <- fluidPage(
  titlePanel("Find Best Distribution for Dataset"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      uiOutput("varSelectUI")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(syntheticData)
  
  observeEvent(input$file1, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL) # Added this check
    
    newData <- read.csv(inFile$datapath, header = input$header)
    if (length(grep("numeric", sapply(newData, class))) == 0) return(NULL) # Check if there's at least one numeric column
    data(newData)
  })
  
  output$varSelectUI <- renderUI({
    df <- data()
    numCols <- sapply(df, is.numeric)
    if (length(names(df)[numCols]) == 0) return(NULL) # Check if there are numeric columns available
    selectInput("var", "Select variable:", names(df)[numCols])
  })
  
  output$distPlot <- renderPlot({
    df <- data()
    var <- req(input$var)
    if (!is.null(var) && var %in% names(df)) { # Check if var is not NULL and exists in df
      find_distribution(df, var, plot = TRUE)
    }
  }, res = 96)
}

shinyApp(ui = ui, server = server)
