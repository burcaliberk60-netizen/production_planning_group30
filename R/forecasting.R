forecasting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      # TAB 1: CAUSAL MODEL & LEAST SQUARES METHOD
      tabPanel(
        "1. Linear Regression (Least Squares)",
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Data Input (Causal / Trend Model)"),
            helpText("Enter comma-separated values. Default values represent the Mortgage Rate (X) and Home Sales (Y) example."),
            textInput(
              ns("reg_x"),
              "Explanatory Variable X (or Time t):",
              value = "6, 7, 8, 9, 10"
            ),
            textInput(
              ns("reg_y"),
              "Dependent Variable Y (Demand / Sales):",
              value = "250, 235, 215, 200, 180"
            ),
            numericInput(
              ns("target_x"),
              "Target X to Forecast (e.g., 11% rate):",
              value = 11,
              step = 0.5
            ),
            hr(),
            h5("Formulas (Least Squares Method):"),
            helpText("b = Sxy / Sxx = Σ(X - X̄)(Y - Ȳ) / Σ(X - X̄)²"),
            helpText("a = Ȳ - b·X̄"),
            helpText("Ŷ = a + b·X")
          ),
          mainPanel(
            h4("Model Results and Forecast"),
            verbatimTextOutput(ns("reg_summary")),
            fluidRow(
              column(6, plotOutput(ns("reg_plot"), height = "320px")),
              column(6, plotOutput(ns("res_plot"), height = "320px"))
            ),
            h4("Least Squares Deviation Table"),
            tableOutput(ns("reg_table"))
          )
        )
      ),

      # TAB 2: TIME SERIES - MOVING AVERAGE & OPTIMUM n (MAPE)
      tabPanel(
        "2. Moving Average (MA) & MAPE Optimization",
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Time Series Demand Data (Dt)"),
            helpText("Enter past demand values (D1, D2, ..., Dt) separated by commas:"),
            textAreaInput(
              ns("ma_d"),
              "Demand Series (D1, D2, ..., Dt):",
              value = "250, 235, 240, 220, 215, 230, 205, 200, 195, 180, 190, 185",
              rows = 3
            ),
            checkboxInput(
              ns("auto_n"),
              "Automatically select optimum n that minimizes MAPE",
              value = TRUE
            ),
            sliderInput(
              ns("ma_n"),
              "Manual Window Size (n):",
              min = 1,
              max = 6,
              value = 3,
              step = 1
            ),
            hr(),
            h5("Notation Conventions & Criteria:"),
            helpText("Ft = (Dt-1 + Dt-2 + ... + Dt-n) / n"),
            helpText("et = Dt - Ft"),
            helpText("MAPE = (1/m · Σ |et / Dt|) × 100")
          ),
          mainPanel(
            h4("Optimum n Simulation (MAPE Comparison)"),
            verbatimTextOutput(ns("ma_summary")),
            tableOutput(ns("mape_comp_table")),
            plotOutput(ns("ma_plot"), height = "320px"),
            h4("Period-by-Period Forecast and Error Table"),
            tableOutput(ns("ma_detail_table"))
          )
        )
      )
    )
  )
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- PART 1: SIMPLE LINEAR REGRESSION (LEAST SQUARES) ---
    reg_data <- reactive({
      x <- as.numeric(unlist(strsplit(input$reg_x, "[,;\\s]+")))
      y <- as.numeric(unlist(strsplit(input$reg_y, "[,;\\s]+")))
      x <- x[!is.na(x)]
      y <- y[!is.na(y)]

      validate(
        need(length(x) >= 2 && length(y) >= 2, "Please enter at least 2 observations."),
        need(length(x) == length(y), "X and Y must have the same number of observations.")
      )

      x_bar <- mean(x)
      y_bar <- mean(y)
      dx <- x - x_bar
      dy <- y - y_bar
      dx_dy <- dx * dy
      dx2 <- dx^2

      s_xy <- sum(dx_dy)
      s_xx <- sum(dx2)
      validate(need(s_xx > 0, "X values cannot all be identical."))

      b <- s_xy / s_xx
      a <- y_bar - b * x_bar

      y_hat <- a + b * x
      e <- y - y_hat
      sse <- sum(e^2)
      mape <- mean(abs(e / y)) * 100
      y_pred <- a + b * input$target_x

      list(
        x = x, y = y, x_bar = x_bar, y_bar = y_bar,
        dx = dx, dy = dy, dx_dy = dx_dy, dx2 = dx2,
        s_xy = s_xy, s_xx = s_xx, a = a, b = b,
        y_hat = y_hat, e = e, sse = sse, mape = mape,
        y_pred = y_pred
      )
    })

    output$reg_summary <- renderPrint({
      d <- reg_data()
      sign_str <- ifelse(d$b >= 0, "+", "-")
      cat(sprintf("Mean X (X̄) = %.2f          |   Mean Y (Ȳ) = %.2f\n", d$x_bar, d$y_bar))
      cat(sprintf("Sxy = %.2f                |   Sxx = %.2f\n", d$s_xy, d$s_xx))
      cat(sprintf("Slope (b) = Sxy / Sxx = %.4f\n", d$b))
      cat(sprintf("Intercept (a) = Ȳ - b·X̄ = %.4f\n", d$a))
      cat("------------------------------------------------------------\n")
      cat(sprintf("Fitted Forecasting Equation : Ŷ = %.2f %s %.2f * X\n", d$a, sign_str, abs(d$b)))
      cat(sprintf("Forecast for X = %.2f (Ŷ)  : %.2f (≈ %.0f units)\n",
                  input$target_x, d$y_pred, round(d$y_pred)))
      cat(sprintf("SSE (Sum of Squared Errors) : %.2f   |   MAPE : %.2f%%\n", d$sse, d$mape))
    })

    output$reg_table <- renderTable({
      d <- reg_data()
      df <- data.frame(
        Observation = as.character(seq_along(d$x)),
        X = d$x,
        Y = d$y,
        `X - X̄` = d$dx,
        `Y - Ȳ` = d$dy,
        `(X - X̄)(Y - Ȳ)` = d$dx_dy,
        `(X - X̄)²` = d$dx2,
        `Fitted (Ŷ)` = d$y_hat,
        `Residual (e = Y - Ŷ)` = d$e,
        check.names = FALSE
      )
      total_row <- data.frame(
        Observation = "Total",
        X = sum(d$x),
        Y = sum(d$y),
        `X - X̄` = sum(d$dx),
        `Y - Ȳ` = sum(d$dy),
        `(X - X̄)(Y - Ȳ)` = d$s_xy,
        `(X - X̄)²` = d$s_xx,
        `Fitted (Ŷ)` = sum(d$y_hat),
        `Residual (e = Y - Ŷ)` = sum(d$e),
        check.names = FALSE
      )
      rbind(df, total_row)
    }, digits = 2)

    output$reg_plot <- renderPlot({
      d <- reg_data()
      x_all <- c(d$x, input$target_x)
      y_all <- c(d$y, d$y_pred)
      plot(
        d$x, d$y,
        pch = 19, col = "#1f77b4", cex = 1.4,
        xlim = range(x_all) + c(-0.5, 0.5),
        ylim = range(y_all) + c(-15, 15),
        xlab = "X (Explanatory Variable)",
        ylab = "Y (Demand / Sales)",
        main = "Least Squares Regression Line"
      )
      grid()
      abline(a = d$a, b = d$b, col = "#d62728", lwd = 2)
      points(input$target_x, d$y_pred, pch = 17, col = "#2ca02c", cex = 1.6)
      segments(d$x, d$y, d$x, d$y_hat, col = "gray50", lty = 2)
      legend(
        "topright",
        legend = c("Observed (Y)", "Fitted Line (Ŷ)", "Forecast"),
        col = c("#1f77b4", "#d62728", "#2ca02c"),
        pch = c(19, NA, 17),
        lty = c(NA, 1, NA),
        lwd = c(NA, 2, NA),
        bty = "n"
      )
    })

    output$res_plot <- renderPlot({
      d <- reg_data()
      plot(
        d$x, d$e,
        pch = 19, col = "#ff7f0e", cex = 1.4,
        xlab = "X",
        ylab = "Residual (e = Y - Ŷ)",
        main = "Residual Plot (Fit Check)"
      )
      grid()
      abline(h = 0, col = "black", lwd = 2, lty = 2)
    })

    # --- PART 2: MOVING AVERAGE (MA) & MAPE OPTIMIZATION ---
    ma_sim <- reactive({
      D <- as.numeric(unlist(strsplit(input$ma_d, "[,;\\s]+")))
      D <- D[!is.na(D)]
      N <- length(D)
      validate(need(N >= 4, "Please enter at least 4 periods of demand data for simulation."))

      max_n <- max(2, min(floor(N - 1), 10))

      comp_list <- lapply(1:max_n, function(k) {
        F_vec <- rep(NA, N)
        for (t in (k + 1):N) {
          F_vec[t] <- mean(D[(t - k):(t - 1)])
        }
        err <- D - F_vec
        valid <- !is.na(err)
        mad_val <- mean(abs(err[valid]))
        mse_val <- mean(err[valid]^2)
        mape_val <- mean(abs(err[valid] / D[valid])) * 100
        next_forecast <- mean(D[(N - k + 1):N])

        list(
          n = k, F_vec = F_vec, err = err,
          MAD = mad_val, MSE = mse_val, MAPE = mape_val,
          next_F = next_forecast
        )
      })

      mape_vec <- sapply(comp_list, function(x) x$MAPE)
      best_n <- which.min(mape_vec)

      chosen_n <- if (isTRUE(input$auto_n)) {
        best_n
      } else {
        min(input$ma_n, max_n)
      }

      list(
        D = D, N = N, max_n = max_n,
        comp = comp_list, best_n = best_n, chosen_n = chosen_n
      )
    })

    output$ma_summary <- renderPrint({
      res <- ma_sim()
      best_info <- res$comp[[res$best_n]]
      chosen_info <- res$comp[[res$chosen_n]]

      cat(sprintf("Total Observations (T)          : %d periods\n", res$N))
      cat(sprintf("Optimum n Minimizing MAPE       : n = %d (MAPE = %.2f%%)\n",
                  res$best_n, best_info$MAPE))
      cat("------------------------------------------------------------\n")
      cat(sprintf("Selected Model                  : MA(%d)\n", res$chosen_n))
      cat(sprintf("Next Period Forecast (F_%d,%d = F_%d) : %.2f\n",
                  res$N, res$N + 1, res$N + 1, chosen_info$next_F))
      cat(sprintf("Selected Model Errors -> MAPE: %.2f%% | MAD: %.2f | MSE: %.2f\n",
                  chosen_info$MAPE, chosen_info$MAD, chosen_info$MSE)) })      output$
