library(MASS)
library(ggplot2)
library(fitdistrplus)

set.seed(123)

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

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Distribution Finder"),
  dashboardSidebar(
    selectInput("dataset", "Select a dataset:", choices = c("Gaussian", "Poisson", "Exponential", "Gamma", "Log-normal")),
    radioButtons("info_criterion", "Information Criterion:", choices = c("AIC", "BIC"))
  ),
  dashboardBody(
    box(plotOutput("distPlot", width = "100%")),
    box(textOutput("bestDist"))
  )
)

server <- function(input, output) {
  # Define a reactive expression for the selected dataset
  dataset <- reactive({
    switch(input$dataset,
           "Gaussian" = rnorm(1000, mean = 10, sd = 2),
           "Poisson" = rpois(1000, lambda = 4),
           "Exponential" = rexp(1000, rate = 0.5),
           "Gamma" = rgamma(1000, shape = 2, rate = 0.5),
           "Log-normal" = rlnorm(1000, meanlog = 1, sdlog = 0.5)
    )
  })
  
  # Find the best distribution and output the plot
  output$distPlot <- renderPlot({
    df <- data.frame(data = dataset())
    best_dist <- find_distribution(df, "data", plot = FALSE, use_bic = input$info_criterion == "BIC", return_all_info = TRUE)
    fitted_data <- r_fun(best_dist$best_distribution, length(df$data), coef(best_dist$fits[[best_dist$best_distribution]]))
    plot_overlapping_histograms(df$data, fitted_data, best_dist$best_distribution)
  })
  
  # Output the name of the best distribution
  output$bestDist <- renderText({
    df <- data.frame(data = dataset())
    best_dist <- find_distribution(df, "data", plot = FALSE, use_bic = input$info_criterion == "BIC")
    paste("The best distribution for the", tolower(input$dataset), "data is:", best_dist)
  })
}

shinyApp(ui = ui, server = server)
