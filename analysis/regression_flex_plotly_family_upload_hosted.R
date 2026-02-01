# See hosted version: https://jchase.shinyapps.io/Regression/

library(shiny)
library(DT)
library(plotly)
library(mgcv)

data(mtcars)

ui <- fluidPage(
  titlePanel("Model"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file", accept = c(".csv")),
      selectInput("x", "X", choices = names(mtcars), selected = names(mtcars)[1]),
      selectInput("y", "Y", choices = names(mtcars), selected = names(mtcars)[2]),
      selectInput(
        "family",
        "Family Type:",
        choices = c("gaussian", "poisson", "binomial"),
        selected = "gaussian"
      ),
      checkboxInput("loess", "Show LOESS Fit", value = FALSE),
      checkboxInput("lm", "Show Linear/GLM Fit", value = FALSE),
      checkboxInput("gam", "Show GAM Fit", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", plotlyOutput("scatterplot")),
        tabPanel("Descriptive Stats", DT::dataTableOutput("desc_stats")),
        tabPanel("Results", DT::dataTableOutput("reg_table")),
        tabPanel("Summary", verbatimTextOutput("model_summary")),
        tabPanel("Assumptions", plotOutput("diagnostic"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Example dataset is used until a user uploads a CSV
  dataset <- reactive({
    if (is.null(input$file)) {
      return(mtcars)
    }
    read.csv(
      input$file$datapath,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = TRUE
    )
  })
  
  # Restrict Y choices to two-level variables when Binomial family is selected
  y_choices <- reactive({
    df <- dataset()
    if (input$family == "binomial") {
      keep <- vapply(df, function(col) {
        u <- unique(col[!is.na(col)])
        length(u) == 2
      }, logical(1))
      names(df)[keep]
    } else {
      names(df)
    }
  })
  
  # Keep X and Y selectors synchronized with the active dataset and family type
  observeEvent(list(dataset(), input$family), {
    df <- dataset()
    if (ncol(df) == 0) return()
    
    x_selected <- if (!is.null(input$x) && input$x %in% names(df)) input$x else names(df)[1]
    updateSelectInput(session, "x", choices = names(df), selected = x_selected)
    
    yc <- y_choices()
    if (length(yc) == 0) {
      updateSelectInput(session, "y", choices = character(0), selected = character(0))
    } else {
      y_selected <- if (!is.null(input$y) && input$y %in% yc) input$y else yc[1]
      updateSelectInput(session, "y", choices = yc, selected = y_selected)
    }
  }, ignoreNULL = FALSE)
  
  # Assemble x and y for modeling and plotting
  filtered_data <- reactive({
    df <- dataset()
    
    validate(
      need(!is.null(input$x) && nzchar(input$x), "Select an X variable."),
      need(!is.null(input$y) && nzchar(input$y), "Select a Y variable."),
      need(all(c(input$x, input$y) %in% names(df)), "Selected variables are not available in the dataset.")
    )
    
    out <- df[, c(input$x, input$y), drop = FALSE]
    names(out) <- c("x", "y")
    
    out <- out[!is.na(out$x) & !is.na(out$y), , drop = FALSE]
    validate(need(nrow(out) >= 2, "At least two complete rows are required."))
    
    if (input$family == "binomial") {
      y_non_na <- out$y[!is.na(out$y)]
      validate(need(length(unique(y_non_na)) == 2, "Y must have exactly two levels for Binomial family."))
      out$y <- as.integer(as.factor(out$y)) - 1L
    }
    
    if (input$family %in% c("gaussian", "poisson")) {
      validate(need(is.numeric(out$y), "Y must be numeric for Gaussian or Poisson models."))
    }
    
    if (input$family == "poisson") {
      validate(need(all(out$y >= 0), "Y must be non-negative for Poisson models."))
    }
    
    out
  })
  
  # Fit the active model used by Results, Summary, and Assumptions
  model <- reactive({
    df <- filtered_data()
    
    fam <- switch(
      input$family,
      gaussian = gaussian(),
      poisson  = poisson(),
      binomial = binomial()
    )
    
    if (input$gam) {
      validate(need(is.numeric(df$x), "X must be numeric to fit a GAM smoother."))
      mgcv::gam(y ~ s(x), data = df, family = fam)
    } else {
      if (input$family == "gaussian") {
        stats::lm(y ~ x, data = df)
      } else {
        stats::glm(y ~ x, data = df, family = fam)
      }
    }
  })
  
  # Combine data frames with different columns by aligning missing fields
  bind_rows_fill <- function(a, b) {
    if (is.null(a) && is.null(b)) return(NULL)
    if (is.null(a)) return(b)
    if (is.null(b)) return(a)
    
    cols <- union(names(a), names(b))
    
    for (nm in setdiff(cols, names(a))) a[[nm]] <- NA
    for (nm in setdiff(cols, names(b))) b[[nm]] <- NA
    
    a <- a[, cols, drop = FALSE]
    b <- b[, cols, drop = FALSE]
    
    rbind(a, b)
  }
  
  # Build a Results table for GAM objects from parametric and smooth summaries
  gam_results_table <- function(m) {
    sm <- summary(m)
    
    param_df <- NULL
    if (!is.null(sm$p.table)) {
      ptab <- as.data.frame(sm$p.table)
      ptab$term <- rownames(ptab)
      rownames(ptab) <- NULL
      
      stat_col <- intersect(c("t value", "z value"), names(ptab))
      p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), names(ptab))
      
      out <- data.frame(
        component = "parametric",
        term = ptab$term,
        estimate = if ("Estimate" %in% names(ptab)) ptab[["Estimate"]] else NA_real_,
        std_error = if ("Std. Error" %in% names(ptab)) ptab[["Std. Error"]] else NA_real_,
        statistic = if (length(stat_col) == 1) ptab[[stat_col]] else NA_real_,
        p_value = if (length(p_col) == 1) ptab[[p_col]] else NA_real_,
        edf = NA_real_,
        ref_df = NA_real_,
        stringsAsFactors = FALSE
      )
      
      param_df <- out
    }
    
    smooth_df <- NULL
    if (!is.null(sm$s.table)) {
      stab <- as.data.frame(sm$s.table)
      stab$term <- rownames(stab)
      rownames(stab) <- NULL
      
      stat_col <- intersect(c("F", "Chi.sq"), names(stab))
      p_col <- intersect(c("p-value", "p.value"), names(stab))
      
      out <- data.frame(
        component = "smooth",
        term = stab$term,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = if (length(stat_col) == 1) stab[[stat_col]] else NA_real_,
        p_value = if (length(p_col) == 1) stab[[p_col]] else NA_real_,
        edf = if ("edf" %in% names(stab)) stab[["edf"]] else NA_real_,
        ref_df = if ("Ref.df" %in% names(stab)) stab[["Ref.df"]] else NA_real_,
        stringsAsFactors = FALSE
      )
      
      smooth_df <- out
    }
    
    bind_rows_fill(param_df, smooth_df)
  }
  
  # Scatterplot rendered directly in plotly (no ggplot2/ggplotly path)
  output$scatterplot <- renderPlotly({
    df <- filtered_data()
    
    plt <- plot_ly(
      data = df,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers",
      name = "Data"
    )
    
    x_is_numeric <- is.numeric(df$x)
    y_is_numeric <- is.numeric(df$y)
    
    if (x_is_numeric) {
      x_grid <- seq(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE), length.out = 200)
      grid_df <- data.frame(x = x_grid)
    }
    
    if (input$loess && x_is_numeric && y_is_numeric) {
      lo <- tryCatch(stats::loess(y ~ x, data = df), error = function(e) NULL)
      if (!is.null(lo)) {
        y_hat <- stats::predict(lo, newdata = grid_df)
        plt <- add_lines(plt, x = x_grid, y = y_hat, name = "LOESS")
      }
    }
    
    if (input$lm && x_is_numeric) {
      fam <- switch(
        input$family,
        gaussian = gaussian(),
        poisson  = poisson(),
        binomial = binomial()
      )
      
      fit <- tryCatch({
        if (input$family == "gaussian") {
          stats::lm(y ~ x, data = df)
        } else {
          stats::glm(y ~ x, data = df, family = fam)
        }
      }, error = function(e) NULL)
      
      if (!is.null(fit)) {
        y_hat <- stats::predict(fit, newdata = grid_df, type = "response")
        plt <- add_lines(plt, x = x_grid, y = y_hat, name = "Linear/GLM")
      }
    }
    
    if (input$gam && x_is_numeric) {
      fam <- switch(
        input$family,
        gaussian = gaussian(),
        poisson  = poisson(),
        binomial = binomial()
      )
      
      gfit <- tryCatch(mgcv::gam(y ~ s(x), data = df, family = fam), error = function(e) NULL)
      if (!is.null(gfit)) {
        y_hat <- stats::predict(gfit, newdata = grid_df, type = "response")
        plt <- add_lines(plt, x = x_grid, y = y_hat, name = "GAM")
      }
    }
    
    plt
  })
  
  # Descriptive statistics with completeness and numeric summaries
  output$desc_stats <- DT::renderDataTable({
    df <- dataset()
    vars <- names(df)
    
    type <- vapply(df, function(x) class(x)[1], character(1))
    n <- vapply(df, function(x) sum(!is.na(x)), integer(1))
    n_missing <- vapply(df, function(x) sum(is.na(x)), integer(1))
    n_unique <- vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1))
    
    mean_val <- rep(NA_real_, length(vars))
    sd_val <- rep(NA_real_, length(vars))
    min_val <- rep(NA_real_, length(vars))
    med_val <- rep(NA_real_, length(vars))
    max_val <- rep(NA_real_, length(vars))
    
    for (i in seq_along(vars)) {
      x <- df[[i]]
      if (is.numeric(x)) {
        mean_val[i] <- mean(x, na.rm = TRUE)
        sd_val[i] <- sd(x, na.rm = TRUE)
        min_val[i] <- min(x, na.rm = TRUE)
        med_val[i] <- median(x, na.rm = TRUE)
        max_val[i] <- max(x, na.rm = TRUE)
      }
    }
    
    desc <- data.frame(
      variable = vars,
      type = type,
      n = n,
      n_missing = n_missing,
      n_unique = n_unique,
      mean = mean_val,
      sd = sd_val,
      min = min_val,
      median = med_val,
      max = max_val,
      stringsAsFactors = FALSE
    )
    
    DT::datatable(desc, options = list(pageLength = 25))
  })
  
  # Results table for lm/glm/gam
  output$reg_table <- DT::renderDataTable({
    m <- model()
    
    if (inherits(m, "gam")) {
      out <- gam_results_table(m)
      validate(need(!is.null(out) && nrow(out) > 0, "No results are available for the current GAM fit."))
      DT::datatable(out, options = list(pageLength = 25))
    } else {
      sm <- summary(m)
      coefs <- as.data.frame(sm$coefficients)
      coefs$term <- rownames(coefs)
      rownames(coefs) <- NULL
      
      est <- coefs[["Estimate"]]
      se <- coefs[["Std. Error"]]
      z <- 1.96
      
      conf_low <- est - z * se
      conf_high <- est + z * se
      
      out <- data.frame(
        term = coefs$term,
        estimate = est,
        std_error = se,
        statistic = coefs[[grep("value$", names(coefs), value = TRUE)[1]]],
        p_value = coefs[[grep("^Pr\\(", names(coefs), value = TRUE)[1]]],
        conf_low = conf_low,
        conf_high = conf_high,
        stringsAsFactors = FALSE
      )
      
      if (input$family %in% c("poisson", "binomial")) {
        out$estimate <- exp(out$estimate)
        out$conf_low <- exp(out$conf_low)
        out$conf_high <- exp(out$conf_high)
      }
      
      DT::datatable(out, options = list(pageLength = 25))
    }
  })
  
  # Summary output for the active model object
  output$model_summary <- renderPrint({
    summary(model())
  })
  
  # Diagnostic panels suitable for lm/glm/gam objects
  output$diagnostic <- renderPlot({
    m <- model()
    
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(2, 2))
    
    if (inherits(m, "lm") && !inherits(m, "gam")) {
      plot(m)
      return(invisible(NULL))
    }
    
    res_type <- if (inherits(m, "glm") || inherits(m, "gam")) "deviance" else "pearson"
    r <- tryCatch(residuals(m, type = res_type), error = function(e) residuals(m))
    f <- fitted(m)
    
    plot(f, r,
         xlab = "Fitted values",
         ylab = "Residuals",
         main = "Residuals vs Fitted"
    )
    abline(h = 0, lty = 2)
    
    qqnorm(r, main = "Normal Q-Q")
    qqline(r)
    
    plot(f, sqrt(abs(r)),
         xlab = "Fitted values",
         ylab = "Sqrt(|Residuals|)",
         main = "Scale-Location"
    )
    
    if (inherits(m, "gam")) {
      hist(r, main = "Residuals Histogram", xlab = "Residuals")
    } else {
      h <- tryCatch(hatvalues(m), error = function(e) NULL)
      cd <- tryCatch(cooks.distance(m), error = function(e) NULL)
      
      if (!is.null(h) && !is.null(cd)) {
        plot(h, cd,
             xlab = "Leverage",
             ylab = "Cook's distance",
             main = "Influence"
        )
      } else {
        hist(r, main = "Residuals Histogram", xlab = "Residuals")
      }
    }
  })
}

shinyApp(ui, server)
