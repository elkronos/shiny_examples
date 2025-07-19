library(shiny)
library(bslib)
library(DT)
library(fitdistrplus)
library(ggplot2)
library(plotly)
library(shinyWidgets)

# ------------- Helper Functions --------------

.dist_map <- list(
  gaussian     = "norm",
  poisson      = "pois",
  binomial     = "binom",
  exponential  = "exp",
  geometric    = "geom",
  gamma        = "gamma",
  `log-normal` = "lnorm"
)

get_dist_name <- function(dist) {
  dist <- tolower(dist)
  if (!dist %in% names(.dist_map)) stop("Unsupported distribution.")
  .dist_map[[dist]]
}

check_params <- function(dist, params) {
  switch(get_dist_name(dist),
         norm  = if (length(params)!=2 || params[2]<=0)                      stop("Gaussian needs mean & sd>0."),
         pois  = if (length(params)!=1 || params[1]<=0)                      stop("Poisson needs λ>0."),
         binom = if (length(params)!=2 || params[1]<0 || params[2]<0 || params[2]>1)
           stop("Binomial needs size≥0 & 0≤p≤1."),
         exp   = if (length(params)!=1 || params[1]<=0)                      stop("Exponential needs rate>0."),
         geom  = if (length(params)!=1 || params[1]<=0 || params[1]>1)
           stop("Geometric needs 0<p≤1."),
         gamma = if (length(params)!=2 || any(params<=0))                    stop("Gamma needs shape>0 & rate>0."),
         lnorm = if (length(params)!=2 || params[2]<=0)                      stop("Log-normal needs meanlog & sdlog>0.")
  )
}

r_fun <- function(dist, n, params, ...) {
  n <- as.integer(n)
  check_params(dist, params)
  do.call(paste0("r", get_dist_name(dist)),
          c(list(n = n), as.list(params), list(...)))
}

plot_overlay <- function(obs, sim, name, bins = 30) {
  df <- data.frame(
    x    = c(obs, sim),
    type = rep(c("Observed","Fitted"), c(length(obs), length(sim)))
  )
  gg <- ggplot(df, aes(x, fill = type)) +
    { if (all(obs == floor(obs)))
      geom_bar(aes(y = after_stat(prop)), position="dodge", alpha=0.6)
      else
        geom_histogram(aes(y = after_stat(density)), bins = bins, alpha=0.6)
    } +
    scale_fill_manual(values = c("Observed"="steelblue","Fitted"="tomato")) +
    theme_minimal(base_size = 14) +
    labs(title = paste("Data vs", name), x = NULL, y = NULL)
  ggplotly(gg)
}

find_distribution <- function(df, col, use_bic = FALSE, candidates = NULL) {
  data <- df[[col]]
  if (!is.numeric(data)) stop("Selected column must be numeric.")
  if (is.null(candidates)) {
    candidates <- c("gaussian","poisson","exponential","gamma","log-normal")
    if (all(data == floor(data)) && min(data) >= 0)
      candidates <- c(candidates, "binomial","geometric")
    if (min(data) <= 0) candidates <- setdiff(candidates, c("gamma","log-normal"))
    if (min(data) < 0) candidates <- "gaussian"
  }
  results <- lapply(candidates, function(dist) {
    fit <- tryCatch(fitdistrplus::fitdist(data, get_dist_name(dist)), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    list(
      dist   = dist,
      fit    = fit,
      aic    = fit$aic,
      bic    = fit$bic,
      params = coef(fit),
      gof    = fitdistrplus::gofstat(fit, fitnames = dist)
    )
  })
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) stop("No fits succeeded.")
  scores <- sapply(results, function(r) if (use_bic) r$bic else r$aic)
  best   <- results[[which.min(scores)]]
  list(best = best, all = results)
}

# ---------------------- UI ----------------------

ui <- fluidPage(
  theme = bs_theme(bootswatch="flatly", base_font="Helvetica"),
  titlePanel(
    tagList(
      "Distribution Fitting Explorer",
      dropdownButton(
        inputId = "settings", label = NULL, icon = icon("gear"), circle = FALSE,
        sliderInput("bins", "Number of bins", min = 5, max = 100, value = 30, step = 1)
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload CSV", accept = ".csv"),
      uiOutput("col_ui"),
      checkboxGroupInput(
        "dists","Candidate Distributions",
        choices = c(
          "Gaussian"    = "gaussian",
          "Poisson"     = "poisson",
          "Exponential" = "exponential",
          "Gamma"       = "gamma",
          "Log-Normal"  = "log-normal",
          "Binomial"    = "binomial",
          "Geometric"   = "geometric"
        ),
        selected = c("gaussian","poisson","exponential","gamma","log-normal")
      ),
      radioButtons("crit","Criterion", c("AIC"=FALSE,"BIC"=TRUE), selected=FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 plotlyOutput("main_plot", height="300px"),
                 br(),
                 h5("Best-Fit Statistics"),
                 tableOutput("best_stats"),
                 br(),
                 h5("All Fit AIC/BIC"),
                 DTOutput("fit_table")
        ),
        tabPanel("Diagnostics",
                 fluidRow(
                   column(6, h5("Q–Q Plot"), plotlyOutput("qq_plot", height="250px")),
                   column(6, h5("CDF Plot"),  plotlyOutput("cdf_plot", height="250px"))
                 ),
                 br(),
                 h5("All Distribution Overlays"),
                 uiOutput("all_overlays")
        ),
        tabPanel("About",
                 h3("Distribution Fitting Explorer"),
                 p("This Shiny app automatically fits a suite of distributions to your data and selects the best by AIC or BIC."),
                 h4("Features"),
                 tags$ul(
                   tags$li("Auto‑fits on load for the first numeric column"),
                   tags$li("Upload your CSV or use the demo dataset"),
                   tags$li("Choose candidate distributions & selection criterion"),
                   tags$li("Summary tab: overlay plot + fit statistics"),
                   tags$li("Diagnostics tab: Q–Q, CDF, and all overlays")
                 ),
                 h4("Usage"),
                 tags$ol(
                   tags$li("Launch: demo data fitted immediately."),
                   tags$li("Upload a CSV with a numeric column to replace it."),
                   tags$li("Pick the column and distributions."),
                   tags$li("Switch between AIC/BIC."),
                   tags$li("Explore results in Summary and Diagnostics.")
                 ),
                 hr(),
                 p("Built with R, Shiny, ggplot2, plotly, shinyWidgets, and fitdistrplus.")
        )
      )
    )
  )
)

# -------------------- Server ----------------------

server <- function(input, output, session) {
  demo_data <- reactiveVal(data.frame(Value = rnorm(500, mean=10, sd=2)))
  
  data_in <- reactive({
    if (is.null(input$file)) {
      demo_data()
    } else {
      df <- tryCatch(read.csv(input$file$datapath), error = function(e) NULL)
      if (is.null(df) || !any(sapply(df, is.numeric))) {
        showNotification("Invalid CSV — using demo data.", type="warning")
        demo_data()
      } else df
    }
  })
  
  output$col_ui <- renderUI({
    nums <- names(data_in())[sapply(data_in(), is.numeric)]
    selectInput("col","Numeric Column", nums, nums[1])
  })
  
  fit_res <- reactive({
    req(input$col)
    withProgress(message = "Fitting distributions...", value = 0.3, {
      res <- find_distribution(
        data_in(),
        input$col,
        use_bic    = input$crit,
        candidates = input$dists
      )
      incProgress(0.7)
      res
    })
  })
  
  output$main_plot <- renderPlotly({
    res <- fit_res(); req(res)
    obs <- data_in()[[input$col]]
    sim <- r_fun(res$best$dist, length(obs), res$best$params)
    plot_overlay(obs, sim, res$best$dist, bins = input$bins)
  })
  
  output$best_stats <- renderTable({
    res <- fit_res(); req(res)
    g <- res$best$gof
    data.frame(
      `K–S`             = round(g$ks,   2),
      `Cramer-von-Mises`= round(g$cvm,  2),
      `Anderson-Darling`= round(g$ad,   2),
      check.names = FALSE
    )
  }, rownames = FALSE)
  
  output$fit_table <- renderDT({
    res <- fit_res(); req(res)
    df <- do.call(rbind, lapply(res$all, function(a)
      data.frame(Dist = a$dist, AIC = a$aic, BIC = a$bic)
    ))
    datatable(df, options = list(dom = 't'), rownames = FALSE) %>%
      formatRound(c('AIC','BIC'), 2)
  })
  
  output$qq_plot <- renderPlotly({
    res <- fit_res(); req(res)
    obs <- data_in()[[input$col]]
    qfun <- get(paste0("q", get_dist_name(res$best$dist)))
    df <- data.frame(sample = obs)
    p <- ggplot(df, aes(sample = sample)) +
      stat_qq(distribution = qfun, dparams = res$best$params) +
      stat_qq_line(distribution = qfun, dparams = res$best$params) +
      theme_minimal(base_size = 14) +
      labs(title = paste("Q–Q Plot:", res$best$dist), x = NULL, y = NULL)
    ggplotly(p)
  })
  
  output$cdf_plot <- renderPlotly({
    res <- fit_res(); req(res)
    obs <- data_in()[[input$col]]
    pfun <- get(paste0("p", get_dist_name(res$best$dist)))
    df <- data.frame(sample = obs)
    p <- ggplot(df, aes(x = sample)) +
      stat_ecdf(geom = "step") +
      stat_function(fun = pfun, args = as.list(res$best$params)) +
      theme_minimal(base_size = 14) +
      labs(title = paste("CDF Plot:", res$best$dist), x = NULL, y = NULL)
    ggplotly(p)
  })
  
  output$all_overlays <- renderUI({
    res <- fit_res(); req(res)
    obs <- data_in()[[input$col]]
    tagList(
      lapply(res$all, function(a) {
        sim <- r_fun(a$dist, length(obs), a$params)
        nm  <- a$dist
        plotname <- paste0("ov_", nm)
        output[[plotname]] <- renderPlotly({ plot_overlay(obs, sim, nm, bins = input$bins) })
        tagList(
          h6(nm),
          plotlyOutput(plotname, height = "150px"),
          br()
        )
      })
    )
  })
}

shinyApp(ui, server)
