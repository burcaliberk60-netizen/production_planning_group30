exponential_seasonal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      # ========================================================================
      # TAB 1: SIMPLE EXPONENTIAL SMOOTHING (SES) & MA AGE MATCHING (Slides 1-5)
      # ========================================================================
      tabPanel(
        "1. Exponential Smoothing (SES)",
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Simple Exponential Smoothing (SES)"),
            helpText("Default data: Aircraft engine failures over 8 quarters (Slide 2)."),
            textAreaInput(
              ns("ses_d"),
              "Demand Series (D1, D2, ..., Dt):",
              value = "200, 250, 175, 186, 225, 285, 305, 190",
              rows = 2
            ),
            fluidRow(
              column(6, actionButton(ns("load_ses_aircraft"), "Ex 1: Aircraft (Slide 2)", class = "btn-sm")),
              column(6, actionButton(ns("load_ses_cars"), "Ex 2: Car Model (Slide 2)", class = "btn-sm"))
            ),
            br(),
            checkboxInput(
              ns("ses_f1_auto"),
              "Use F1 = D1 as initial forecast",
              value = FALSE
            ),
            numericInput(
              ns("ses_f1"),
              "Initial Forecast for Period 1 (F1):",
              value = 200,
              step = 1
            ),
            radioButtons(
              ns("ses_alpha_mode"),
              "Smoothing Constant (alpha) Selection:",
              choices = c(
                "Manual alpha" = "manual",
                "Match Simple MA(N) Average Age: alpha = 2 / (N + 1)" = "ma_match",
                "Match Weighted MA Average Age: alpha = 1 / sum(i * w_i)" = "wma_match",
                "Optimize alpha (Minimize MAD)" = "opt_mad"
              ),
              selected = "manual"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'manual'", ns("ses_alpha_mode")),
              sliderInput(
                ns("ses_alpha"),
                "Smoothing Constant (alpha):",
                min = 0.05,
                max = 0.99,
                value = 0.10,
                step = 0.05
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'ma_match'", ns("ses_alpha_mode")),
              numericInput(
                ns("ses_ma_n"),
                "Simple MA Window Size (N):",
                value = 4,
                min = 1,
                max = 20,
                step = 1
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'wma_match'", ns("ses_alpha_mode")),
              textInput(
                ns("ses_wma_weights"),
                "Weights from newest (lag 1) to oldest (lag N) (Slide 4 Wallace Garden):",
                value = "3, 2, 1"
              )
            ),
            hr(),
            h5("Formulas (Slides 1-5):"),
            helpText("Ft = alpha * Dt-1 + (1 - alpha) * Ft-1 = Ft-1 + alpha * et-1"),
            helpText("Avg Age (ES) = 1 / alpha   |   Avg Age (MA) = (N + 1) / 2"),
            helpText("Matching MA(N): alpha = 2 / (N + 1)"),
            helpText("Matching WMA: Avg Age = sum(i * w_i) => alpha = 1 / sum(i * w_i)")
          ),
          mainPanel(
            h4("Exponential Smoothing Summary & Average Age Equivalency"),
            verbatimTextOutput(ns("ses_summary")),
            plotOutput(ns("ses_plot"), height = "320px"),
            h4("Period-by-Period SES Forecast & Error Table"),
            tableOutput(ns("ses_table"))
          )
        )
      ),

      # ========================================================================
      # TAB 2: TREND REGRESSION & HOLT'S METHOD (Slides 6-11)
      # ========================================================================
      tabPanel(
        "2. Trend Regression & Holt's Method",
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Double Exponential Smoothing (Holt's Method) & Trend Line"),
            helpText("Default data: Aircraft engine failures (Slides 9-11). You can also load the Spare Part Weekly Demand example (Slide 8)."),
            textAreaInput(
              ns("holt_d"),
              "Demand Series (D1, D2, ..., Dt):",
              value = "200, 250, 175, 186, 225, 285, 305, 190",
              rows = 2
            ),
            fluidRow(
              column(6, actionButton(ns("load_holt_aircraft"), "Aircraft (Slide 10)", class = "btn-sm")),
              column(6, actionButton(ns("load_holt_spare"), "Spare Part (Slide 8)", class = "btn-sm"))
            ),
            br(),
            radioButtons(
              ns("holt_init_mode"),
              "Initialization Method for (S0, G0) (Slides 10-11):",
              choices = c(
                "Manual S0 and G0 (Slide 10: S0 = 200, G0 = 10)" = "manual",
                "Least Squares Trend Regression (S0 = a, G0 = b)" = "regression",
                "Two-Group Average Slope (Slide 11 Notes: G0 = V2 - V1)" = "twogroup",
                "Simple Start (S0 = D1, G0 = 0)" = "simple"
              ),
              selected = "manual"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'manual'", ns("holt_init_mode")),
              fluidRow(
                column(6, numericInput(ns("holt_s0"), "Initial Level (S0):", value = 200, step = 5)),
                column(6, numericInput(ns("holt_g0"), "Initial Slope (G0):", value = 10, step = 1))
              )
            ),
            fluidRow(
              column(6, sliderInput(ns("holt_alpha"), "Level Constant (alpha):", min = 0.05, max = 0.95, value = 0.10, step = 0.05)),
              column(6, sliderInput(ns("holt_beta"), "Trend Constant (beta):", min = 0.05, max = 0.95, value = 0.10, step = 0.05))
            ),
            fluidRow(
              column(6, numericInput(ns("holt_origin"), "Multi-Step Origin (t):", value = 2, min = 1, step = 1)),
              column(6, numericInput(ns("holt_tau"), "Steps Ahead (tau):", value = 3, min = 1, max = 10, step = 1))
            ),
            hr(),
            h5("Formulas (Slides 7-11):"),
            helpText("Trend Regression: Sxy = n*sum(t*Dt) - (n(n+1)/2)*sum(Dt)"),
            helpText("Sxx = n^2(n+1)(2n+1)/6 - n^2(n+1)^2/4 ,  b = Sxy/Sxx ,  a = D_bar - b*(n+1)/2"),
            helpText("Holt Level: St = alpha * Dt + (1 - alpha) * (St-1 + Gt-1)"),
            helpText("Holt Slope: Gt = beta * (St - St-1) + (1 - beta) * Gt-1"),
            helpText("Forecast: Ft,t+tau = St + tau * Gt")
          ),
          mainPanel(
            h4("Holt's Method & Trend Regression Summary"),
            verbatimTextOutput(ns("holt_summary")),
            fluidRow(
              column(7, plotOutput(ns("holt_plot"), height = "320px")),
              column(5, plotOutput(ns("holt_mad_plot"), height = "320px"))
            ),
            h4("Holt's Method & Trend Table (Including t = 0 Initialization)"),
            tableOutput(ns("holt_table"))
          )
        )
      ),

      # ========================================================================
      # TAB 3: SEASONAL DECOMPOSITION (QUICK & DIRTY + CENTERED MA) (Slides 12-18)
      # ========================================================================
      tabPanel(
        "3. Seasonal Decomposition",
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Seasonal Decomposition Methods"),
            helpText("Load one of the 3 textbook examples from Slides 13-18 or enter custom seasonal data:"),
            fluidRow(
              column(4, actionButton(ns("load_seas_q4"), "Ex 2.7: N=4 (Slide 14)", class = "btn-sm")),
              column(4, actionButton(ns("load_seas_s3"), "Shifts: N=3 (Slide 18)", class = "btn-sm")),
              column(4, actionButton(ns("load_seas_b5"), "Ex 2.6: N=5 (Slide 13)", class = "btn-sm"))
            ),
            br(),
            textAreaInput(
              ns("seas_d"),
              "Seasonal Demand Series (D1, D2, ..., Dt):",
              value = "10, 20, 26, 17, 12, 23, 30, 22",
              rows = 3
            ),
            numericInput(
              ns("seas_n"),
              "Season Length (N) (e.g., 4 quarters, 3 shifts, 5 weekdays):",
              value = 4,
              min = 2,
              max = 12,
              step = 1
            ),
            radioButtons(
              ns("seas_method"),
              "Decomposition Method:",
              choices = c(
                "Moving Average Decomposition (Centered MA + Trend Line, Slides 14-18)" = "cma",
                "Quick and Dirty Method (Stationary Overall Mean, Slide 13)" = "qd"
              ),
              selected = "cma"
            ),
            numericInput(
              ns("seas_h"),
              "Number of Future Periods to Forecast:",
              value = 4,
              min = 1,
              max = 12,
              step = 1
            ),
            hr(),
            h5("5-Step Plan (Slides 14-18):"),
            helpText("1. Smooth season with N-period MA (center if N is even; fill ends)."),
            helpText("2. Ratio each demand to smooth trend: ratio_t = Dt / CMA_t."),
            helpText("3. Average like periods & rescale so sum(c_i) = N."),
            helpText("4. Deseasonalize demand: d_t = Dt / c_i."),
            helpText("5. Fit trend line d_hat_t = a + b*t, then multiply by c_i.")
          ),
          mainPanel(
            h4("Seasonal Decomposition Summary"),
            verbatimTextOutput(ns("seas_summary")),
            fluidRow(
              column(6, h4("Seasonal Factors (Sum = N)"), tableOutput(ns("seas_factors_table"))),
              column(6, h4("Future Seasonal Forecasts"), tableOutput(ns("seas_forecast_table")))
            ),
            fluidRow(
              column(7, plotOutput(ns("seas_plot"), height = "300px")),
              column(5, plotOutput(ns("seas_factor_plot"), height = "300px"))
            ),
            h4("Full Decomposition & Deseasonalization Table"),
            tableOutput(ns("seas_detail_table"))
          )
        )
      ),

      # ========================================================================
      # TAB 4: WINTERS'S METHOD (TRIPLE EXPONENTIAL SMOOTHING) (Slides 19-21)
      # ========================================================================
      tabPanel(
        "4. Winters's Method",
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Winters's Triple Exponential Smoothing"),
            helpText("Requires at least 2 full seasons (2N observations) to initialize V1, V2, G0, S0, and c1..cN (Slide 21)."),
            fluidRow(
              column(6, actionButton(ns("load_win_2s"), "2 Seasons (N=4)", class = "btn-sm")),
              column(6, actionButton(ns("load_win_3s"), "3 Seasons (N=4)", class = "btn-sm"))
            ),
            br(),
            textAreaInput(
              ns("win_d"),
              "Demand Series (at least 2N observations):",
              value = "10, 20, 26, 17, 12, 23, 30, 22, 15, 27, 35, 25",
              rows = 3
            ),
            numericInput(
              ns("win_n"),
              "Season Length (N):",
              value = 4,
              min = 2,
              max = 12,
              step = 1
            ),
            sliderInput(ns("win_alpha"), "Level Constant (alpha):", min = 0.05, max = 0.95, value = 0.20, step = 0.05),
            sliderInput(ns("win_beta"), "Trend Constant (beta):", min = 0.05, max = 0.95, value = 0.15, step = 0.05),
            sliderInput(ns("win_gamma"), "Seasonal Constant (gamma):", min = 0.05, max = 0.95, value = 0.15, step = 0.05),
            numericInput(
              ns("win_tau"),
              "Forecast Horizon Ahead (tau periods):",
              value = 4,
              min = 1,
              max = 12,
              step = 1
            ),
            hr(),
            h5("Initialization & Forecast Formulas (Slides 20-21):"),
            helpText("G0 = (V2 - V1) / N  |  S0 = V2 + G0 * (N - 1) / 2"),
            helpText("c_ij = D_ij / (V_i + (j - (N + 1)/2) * G0), normalized to sum to N"),
            helpText("St = alpha*(Dt / ct-N) + (1 - alpha)*(St-1 + Gt-1)"),
            helpText("Gt = beta*(St - St-1) + (1 - beta)*Gt-1"),
            helpText("ct = gamma*(Dt / St) + (1 - gamma)*ct-N"),
            helpText("Ft,t+tau = (St + tau * Gt) * c_(t + tau - N)")
          ),
          mainPanel(
            h4("Winters's Initialization (Step 1) & Forecast Summary (Step 2)"),
            verbatimTextOutput(ns("win_summary")),
            fluidRow(
              column(6, h4("Initial Seasonal Factors (Slide 21)"), tableOutput(ns("win_init_table"))),
              column(6, h4("Future Forecasts (Ft,t+tau)"), tableOutput(ns("win_future_table")))
            ),
            plotOutput(ns("win_plot"), height = "320px"),
            h4("Winters's Updating Table"),
            tableOutput(ns("win_table"))
          )
        )
      )
    )
  )
}

exponential_seasonal_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ==========================================================================
    # PART 1: SIMPLE EXPONENTIAL SMOOTHING (SES)
    # ==========================================================================
    observeEvent(input$load_ses_aircraft, {
      updateTextAreaInput(session, "ses_d", value = "200, 250, 175, 186, 225, 285, 305, 190")
      updateCheckboxInput(session, "ses_f1_auto", value = FALSE)
      updateNumericInput(session, "ses_f1", value = 200)
      updateRadioButtons(session, "ses_alpha_mode", selected = "manual")
      updateSliderInput(session, "ses_alpha", value = 0.10)
    })

    observeEvent(input$load_ses_cars, {
      updateTextAreaInput(session, "ses_d", value = "153, 136")
      updateCheckboxInput(session, "ses_f1_auto", value = FALSE)
      updateNumericInput(session, "ses_f1", value = 142)
      updateRadioButtons(session, "ses_alpha_mode", selected = "manual")
      updateSliderInput(session, "ses_alpha", value = 0.20)
    })

    ses_calc <- reactive({
      D <- as.numeric(unlist(strsplit(input$ses_d, "[,;\\s]+")))
      D <- D[!is.na(D)]
      T_len <- length(D)
      validate(need(T_len >= 2, "Please enter at least 2 observations."))

      f1_val <- if (isTRUE(input$ses_f1_auto)) D[1] else input$ses_f1
      validate(need(!is.na(f1_val), "Please provide a valid initial forecast F1."))

      run_ses <- function(alpha_val) {
        F_vec <- numeric(T_len)
        F_vec[1] <- f1_val
        for (t in 2:T_len) {
          F_vec[t] <- alpha_val * D[t - 1] + (1 - alpha_val) * F_vec[t - 1]
        }
        next_F <- alpha_val * D[T_len] + (1 - alpha_val) * F_vec[T_len]
        err <- D - F_vec
        list(
          alpha = alpha_val,
          F_vec = F_vec,
          next_F = next_F,
          err = err,
          MAD = mean(abs(err)),
          MSE = mean(err^2),
          MAPE = mean(abs(err / D)) * 100
        )
      }

      extra_note <- ""
      if (input$ses_alpha_mode == "manual") {
        alpha_used <- input$ses_alpha
        extra_note <- "Manual selection"
      } else if (input$ses_alpha_mode == "ma_match") {
        N_ma <- max(1, input$ses_ma_n)
        alpha_used <- 2 / (N_ma + 1)
        extra_note <- sprintf("Matched to MA(%d) average age = (N+1)/2 = %.2f", N_ma, (N_ma + 1) / 2)
      } else if (input$ses_alpha_mode == "wma_match") {
        w_raw <- as.numeric(unlist(strsplit(input$ses_wma_weights, "[,;\\s]+")))
        w_raw <- w_raw[!is.na(w_raw) & w_raw > 0]
        validate(need(length(w_raw) >= 1, "Enter at least one positive weight."))
        w_norm <- w_raw / sum(w_raw)
        lags <- seq_along(w_norm)
        avg_age <- sum(lags * w_norm)
        alpha_used <- 1 / avg_age
        extra_note <- sprintf("Matched to WMA (normalized weights: %s) -> Avg Age = %.4f",
                              paste(round(w_norm, 3), collapse = ", "), avg_age)
      } else {
        grid_a <- seq(0.01, 0.99, by = 0.01)
        mads <- vapply(grid_a, function(a) run_ses(a)$MAD, numeric(1))
        alpha_used <- grid_a[which.min(mads)]
        extra_note <- "Optimized to minimize MAD"
      }

      res <- run_ses(alpha_used)
      res$D <- D
      res$T_len <- T_len
      res$avg_age <- 1 / alpha_used
      res$equiv_N <- (2 - alpha_used) / alpha_used
      res$extra_note <- extra_note
      res
    })

    output$ses_summary <- renderPrint({
      r <- ses_calc()
      cat(sprintf("Smoothing Constant (alpha)         : %.4f (%s)\n", r$alpha, r$extra_note))
      cat(sprintf("Average Age of Data (1 / alpha)    : %.2f periods\n", r$avg_age))
      cat(sprintf("Equivalent Simple MA Window (N)    : %.2f periods (from alpha = 2/(N+1))\n", r$equiv_N))
      cat("------------------------------------------------------------------\n")
      cat(sprintf("Next Period Forecast (F_%d)         : %.2f (approx. %.0f units)\n",
                  r$T_len + 1, r$next_F, round(r$next_F)))
      cat(sprintf("Error Metrics -> MAD: %.2f  |  MSE: %.2f  |  MAPE: %.2f%%\n",
                  r$MAD, r$MSE, r$MAPE))
    })

    output$ses_plot <- renderPlot({
      r <- ses_calc()
      t_all <- seq_len(r$T_len + 1)
      f_all <- c(r$F_vec, r$next_F)
      y_rng <- range(c(r$D, f_all), na.rm = TRUE)

      plot(
        seq_len(r$T_len), r$D,
        type = "b", pch = 19, col = "#1f77b4", lwd = 2,
        xlim = c(1, r$T_len + 1),
        ylim = y_rng + c(-15, 15),
        xlab = "Period (t)",
        ylab = "Demand (Dt) & Forecast (Ft)",
        main = sprintf("Simple Exponential Smoothing (alpha = %.2f)", r$alpha)
      )
      grid()
      lines(t_all, f_all, type = "b", pch = 17, col = "#d62728", lwd = 2, lty = 2)
      points(r$T_len + 1, r$next_F, pch = 15, col = "#2ca02c", cex = 1.6)
      legend(
        "topleft",
        legend = c("Actual Demand (Dt)", "SES Forecast (Ft)", "Next Forecast (Ft+1)"),
        col = c("#1f77b4", "#d62728", "#2ca02c"),
        pch = c(19, 17, 15),
        lty = c(1, 2, NA),
        lwd = c(2, 2, NA),
        bty = "n"
      )
    })

    output$ses_table <- renderTable({
      r <- ses_calc()
      df <- data.frame(
        Period = seq_len(r$T_len),
        Demand = r$D,
        Forecast = r$F_vec,
        Error = r$err,
        AbsError = abs(r$err),
        SqError = r$err^2,
        APE = abs(r$err / r$D) * 100
      )
      colnames(df) <- c(
        "Period (t)", "Demand (Dt)", "Forecast (Ft)",
        "Error (et = Dt - Ft)", "|et|", "et^2", "|et| / Dt (%)"
      )
      df
    }, digits = 2)

    # ==========================================================================
    # PART 2: TREND REGRESSION & HOLT'S METHOD (DOUBLE EXPONENTIAL SMOOTHING)
    # ==========================================================================
    observeEvent(input$load_holt_aircraft, {
      updateTextAreaInput(session, "holt_d", value = "200, 250, 175, 186, 225, 285, 305, 190")
      updateRadioButtons(session, "holt_init_mode", selected = "manual")
      updateNumericInput(session, "holt_s0", value = 200)
      updateNumericInput(session, "holt_g0", value = 10)
      updateSliderInput(session, "holt_alpha", value = 0.10)
      updateSliderInput(session, "holt_beta", value = 0.10)
      updateNumericInput(session, "holt_origin", value = 2)
      updateNumericInput(session, "holt_tau", value = 3)
    })

    observeEvent(input$load_holt_spare, {
      updateTextAreaInput(session, "holt_d", value = "46, 48, 56, 59, 67, 69")
      updateRadioButtons(session, "holt_init_mode", selected = "regression")
      updateSliderInput(session, "holt_alpha", value = 0.20)
      updateSliderInput(session, "holt_beta", value = 0.20)
      updateNumericInput(session, "holt_origin", value = 6)
      updateNumericInput(session, "holt_tau", value = 2)
    })

    holt_calc <- reactive({
      D <- as.numeric(unlist(strsplit(input$holt_d, "[,;\\s]+")))
      D <- D[!is.na(D)]
      n <- length(D)
      validate(need(n >= 4, "Please enter at least 4 periods of demand data."))

      t_vec <- seq_len(n)
      sum_D <- sum(D)
      sum_tD <- sum(t_vec * D)
      D_bar <- mean(D)

      # Time-coded Sxy and Sxx formulas from Slide 7 & Slide 8
      S_xy_scaled <- n * sum_tD - (n * (n + 1) / 2) * sum_D
      S_xx_scaled <- (n^2 * (n + 1) * (2 * n + 1)) / 6 - (n^2 * (n + 1)^2) / 4
      b_reg <- S_xy_scaled / S_xx_scaled
      a_reg <- D_bar - b_reg * ((n + 1) / 2)
      reg_hat <- a_reg + b_reg * t_vec

      # Initialization of S0 and G0
      if (input$holt_init_mode == "manual") {
        S0 <- input$holt_s0
        G0 <- input$holt_g0
      } else if (input$holt_init_mode == "regression") {
        S0 <- a_reg
        G0 <- b_reg
      } else if (input$holt_init_mode == "twogroup") {
        half <- floor(n / 2)
        v1 <- mean(D[1:half])
        v2 <- mean(D[(half + 1):(2 * half)])
        G0 <- (v2 - v1) / half
        S0 <- D[1]
      } else {
        S0 <- D[1]
        G0 <- 0
      }

      alpha <- input$holt_alpha
      beta <- input$holt_beta

      S <- numeric(n)
      G <- numeric(n)
      F_vec <- numeric(n)

      for (t in seq_len(n)) {
        prev_S <- if (t == 1) S0 else S[t - 1]
        prev_G <- if (t == 1) G0 else G[t - 1]
        F_vec[t] <- prev_S + prev_G
        S[t] <- alpha * D[t] + (1 - alpha) * (prev_S + prev_G)
        G[t] <- beta * (S[t] - prev_S) + (1 - beta) * prev_G
      }

      abs_err <- abs(D - F_vec)
      mad_all <- mean(abs_err)
      next_F <- S[n] + G[n]

      # Multi-step forecast Ft,t+tau (Slide 11)
      orig_t <- min(max(1, input$holt_origin), n)
      tau <- max(1, input$holt_tau)
      multi_F <- S[orig_t] + tau * G[orig_t]

      # Comparison on periods 4..n against SES(alpha) and MA(3) (Slide 11)
      ses_F <- numeric(n)
      ses_F[1] <- D[1]
      for (t in 2:n) ses_F[t] <- alpha * D[t - 1] + (1 - alpha) * ses_F[t - 1]

      ma3_F <- rep(NA_real_, n)
      if (n >= 4) {
        for (t in 4:n) ma3_F[t] <- mean(D[(t - 3):(t - 1)])
      }

      comp_idx <- if (n >= 4) 4:n else seq_len(n)
      mad_holt_sub <- mean(abs(D[comp_idx] - F_vec[comp_idx]))
      mad_ses_sub <- mean(abs(D[comp_idx] - ses_F[comp_idx]))
      mad_ma3_sub <- if (n >= 4) mean(abs(D[comp_idx] - ma3_F[comp_idx])) else NA_real_

      list(
        D = D, n = n, t_vec = t_vec,
        sum_D = sum_D, sum_tD = sum_tD, D_bar = D_bar,
        S_xy_scaled = S_xy_scaled, S_xx_scaled = S_xx_scaled,
        a_reg = a_reg, b_reg = b_reg, reg_hat = reg_hat,
        S0 = S0, G0 = G0, S = S, G = G, F_vec = F_vec,
        abs_err = abs_err, mad_all = mad_all, next_F = next_F,
        orig_t = orig_t, tau = tau, multi_F = multi_F,
        mad_holt_sub = mad_holt_sub, mad_ses_sub = mad_ses_sub, mad_ma3_sub = mad_ma3_sub
      )
    })

    output$holt_summary <- renderPrint({
      h <- holt_calc()
      cat("=== 1. TREND-BASED REGRESSION (Least Squares, Slides 7-8) ===\n")
      cat(sprintf("sum(Dt) = %.2f  |  sum(t*Dt) = %.2f  |  Mean D = %.2f\n",
                  h$sum_D, h$sum_tD, h$D_bar))
      cat(sprintf("Scaled Sxy = %.2f  |  Scaled Sxx = %.2f\n", h$S_xy_scaled, h$S_xx_scaled))
      cat(sprintf("Trend Equation: D_hat_t = %.2f + %.2f * t   (Next: D_hat_%d = %.2f, D_hat_%d = %.2f)\n\n",
                  h$a_reg, h$b_reg, h$n + 1, h$a_reg + h$b_reg * (h$n + 1),
                  h$n + 2, h$a_reg + h$b_reg * (h$n + 2)))

      cat("=== 2. HOLT'S METHOD RESULTS (Double Exponential Smoothing, Slides 9-11) ===\n")
      cat(sprintf("Initial Values : S0 = %.2f, G0 = %.2f  -->  First Forecast F1 = S0 + G0 = %.2f\n",
                  h$S0, h$G0, h$S0 + h$G0))
      cat(sprintf("Overall MAD (Periods 1..%d) : %.2f   (Sum |e_t| = %.2f)\n",
                  h$n, h$mad_all, sum(h$abs_err)))
      cat(sprintf("Next Period Forecast (F_%d = S_%d + G_%d) : %.2f + %.2f = %.2f\n",
                  h$n + 1, h$n, h$n, h$S[h$n], h$G[h$n], h$next_F))
      cat(sprintf("Multi-Step Forecast (F_%d,%d = S_%d + %d*G_%d) : %.2f + %d*(%.2f) = %.2f\n",
                  h$orig_t, h$orig_t + h$tau, h$orig_t, h$tau, h$orig_t,
                  h$S[h$orig_t], h$tau, h$G[h$orig_t], h$multi_F))
      if (!is.na(h$mad_ma3_sub)) {
        cat(sprintf("Slide 11 Comparison (Periods 4..%d MAD) -> Holt: %.2f | ES: %.2f | MA(3): %.2f\n",
                    h$n, h$mad_holt_sub, h$mad_ses_sub, h$mad_ma3_sub))
      }
    })

    output$holt_plot <- renderPlot({
      h <- holt_calc()
      t_ext <- seq_len(h$n + h$tau)
      holt_future <- h$S[h$n] + seq_len(h$tau) * h$G[h$n]
      reg_future <- h$a_reg + h$b_reg * t_ext
      y_rng <- range(c(h$D, h$F_vec, holt_future, reg_future), na.rm = TRUE)

      plot(
        h$t_vec, h$D,
        type = "b", pch = 19, col = "#1f77b4", lwd = 2,
        xlim = c(1, h$n + h$tau),
        ylim = y_rng + c(-15, 15),
        xlab = "Period (t)",
        ylab = "Demand & Forecasts",
        main = "Holt's Method & Linear Trend Forecast"
      )
      grid()
      lines(t_ext, reg_future, col = "gray50", lwd = 2, lty = 3)
      lines(c(h$t_vec, (h$n + 1):(h$n + h$tau)), c(h$F_vec, holt_future),
            type = "b", pch = 17, col = "#d62728", lwd = 2, lty = 2)
      points((h$n + 1):(h$n + h$tau), holt_future, pch = 15, col = "#2ca02c", cex = 1.4)
      legend(
        "topleft",
        legend = c("Actual (Dt)", "Holt Forecast (Ft)", "Trend Line (a + bt)", "Future Holt (Ft,t+tau)"),
        col = c("#1f77b4", "#d62728", "gray50", "#2ca02c"),
        pch = c(19, 17, NA, 15),
        lty = c(1, 2, 3, NA),
        lwd = c(2, 2, 2, NA),
        bty = "n"
      )
    })

    output$holt_mad_plot <- renderPlot({
      h <- holt_calc()
      vals <- c(h$mad_holt_sub, h$mad_ses_sub, h$mad_ma3_sub)
      names_vec <- c("Holt", sprintf("ES(%.1f)", input$holt_alpha), "MA(3)")
      valid <- !is.na(vals)
      bp <- barplot(
        vals[valid],
        names.arg = names_vec[valid],
        col = c("#2ca02c", "#1f77b4", "#ff7f0e")[valid],
        ylim = c(0, max(vals[valid]) * 1.25),
        ylab = "MAD (Periods 4..n)",
        main = "Method Comparison (Slide 11)"
      )
      text(bp, vals[valid], labels = round(vals[valid], 1), pos = 3, font = 2)
    })

    output$holt_table <- renderTable({
      h <- holt_calc()
      df0 <- data.frame(
        t = "0",
        Dt = NA_real_,
        Ft = NA_real_,
        St = h$S0,
        Gt = h$G0,
        AbsErr = NA_real_,
        TrendHat = h$a_reg,
        stringsAsFactors = FALSE
      )
      df_main <- data.frame(
        t = as.character(h$t_vec),
        Dt = h$D,
        Ft = h$F_vec,
        St = h$S,
        Gt = h$G,
        AbsErr = h$abs_err,
        TrendHat = h$reg_hat,
        stringsAsFactors = FALSE
      )
      res <- rbind(df0, df_main)
      colnames(res) <- c(
        "t", "Demand (Dt)", "Holt Forecast (Ft = St-1 + Gt-1)",
        "Level (St)", "Slope (Gt)", "|et|", "Trend Regression (a + bt)"
      )
      res
    }, digits = 2, na = "-")

    # ==========================================================================
    # PART 3: SEASONAL DECOMPOSITION (QUICK & DIRTY + CENTERED MA)
    # ==========================================================================
    observeEvent(input$load_seas_q4, {
      updateTextAreaInput(session, "seas_d", value = "10, 20, 26, 17, 12, 23, 30, 22")
      updateNumericInput(session, "seas_n", value = 4)
      updateRadioButtons(session, "seas_method", selected = "cma")
      updateNumericInput(session, "seas_h", value = 4)
    })

    observeEvent(input$load_seas_s3, {
      updateTextAreaInput(session, "seas_d", value = "30, 42, 54, 33, 45, 54, 36, 45, 60, 36, 48, 63")
      updateNumericInput(session, "seas_n", value = 3)
      updateRadioButtons(session, "seas_method", selected = "cma")
      updateNumericInput(session, "seas_h", value = 3)
    })

    observeEvent(input$load_seas_b5, {
      updateTextAreaInput(
        session, "seas_d",
        value = "16.2, 12.2, 14.2, 17.3, 22.5, 17.3, 11.5, 15.0, 17.6, 23.5, 14.6, 13.1, 13.0, 16.9, 21.9, 16.1, 11.8, 12.9, 16.6, 24.3"
      )
      updateNumericInput(session, "seas_n", value = 5)
      updateRadioButtons(session, "seas_method", selected = "qd")
      updateNumericInput(session, "seas_h", value = 5)
    })

    seas_calc <- reactive({
      D <- as.numeric(unlist(strsplit(input$seas_d, "[,;\\s]+")))
      D <- D[!is.na(D)]
      N <- input$seas_n
      T_len <- length(D)

      validate(
        need(N >= 2, "Season length N must be at least 2."),
        need(T_len >= 2 * N, sprintf("Please enter at least 2 full seasons (%d observations).", 2 * N))
      )

      season_pos <- ((seq_len(T_len) - 1) %% N) + 1

      if (input$seas_method == "qd") {
        # Quick and Dirty Method (Slide 13)
        overall_mean <- mean(D)
        cma_filled <- rep(overall_mean, T_len)
        cma_unfilled <- cma_filled
        ratios <- D / overall_mean
      } else {
        # Moving Average Decomposition (Slides 14-18)
        cma_unfilled <- rep(NA_real_, T_len)
        if (N %% 2 == 0) {
          # Even N: centered 2xN MA (Slide 15 & 16)
          ma_len <- T_len - N + 1
          ma_raw <- numeric(ma_len)
          for (k in seq_len(ma_len)) {
            ma_raw[k] <- mean(D[k:(k + N - 1)])
          }
          half <- N / 2
          for (j in seq_len(ma_len - 1)) {
            t_idx <- half + j
            cma_unfilled[t_idx] <- (ma_raw[j] + ma_raw[j + 1]) / 2
          }
          cma_filled <- cma_unfilled
          valid_idx <- which(!is.na(cma_unfilled))
          # Slide 16 rule: fill ends by averaging the two nearest interior CMA values
          if (length(valid_idx) >= 2) {
            start_fill <- mean(cma_unfilled[valid_idx[1:2]])
            end_fill <- mean(cma_unfilled[tail(valid_idx, 2)])
          } else {
            start_fill <- cma_unfilled[valid_idx[1]]
            end_fill <- cma_unfilled[valid_idx[1]]
          }
          cma_filled[1:half] <- start_fill
          cma_filled[(T_len - half + 1):T_len] <- end_fill
        } else {
          # Odd N: MA lands directly on integer period (Slide 18)
          half <- (N - 1) / 2
          for (t_idx in (half + 1):(T_len - half)) {
            cma_unfilled[t_idx] <- mean(D[(t_idx - half):(t_idx + half)])
          }
          cma_filled <- cma_unfilled
          valid_idx <- which(!is.na(cma_unfilled))
          # Slide 18 rule: fill ends from nearest interior MA value
          cma_filled[1:half] <- cma_unfilled[valid_idx[1]]
          cma_filled[(T_len - half + 1):T_len] <- cma_unfilled[tail(valid_idx, 1)]
        }
        ratios <- D / cma_filled
      }

      raw_factors <- vapply(seq_len(N), function(s) {
        mean(ratios[season_pos == s])
      }, numeric(1))

      scale_mult <- N / sum(raw_factors)
      final_factors <- raw_factors * scale_mult

      c_vec <- final_factors[season_pos]
      deseas <- D / c_vec

      # Fit Least Squares Trend Line on Deseasonalized Series (Slide 17 & 18)
      t_vec <- seq_len(T_len)
      fit <- lm(deseas ~ t_vec)
      a_tr <- unname(coef(fit)[1])
      b_tr <- unname(coef(fit)[2])

      if (input$seas_method == "qd") {
        trend_in <- rep(mean(D), T_len)
      } else {
        trend_in <- a_tr + b_tr * t_vec
      }
      fitted_F <- trend_in * c_vec

      # Future forecasts
      h <- max(1, input$seas_h)
      future_t <- (T_len + 1):(T_len + h)
      future_pos <- ((future_t - 1) %% N) + 1
      future_c <- final_factors[future_pos]
      future_trend <- if (input$seas_method == "qd") rep(mean(D), h) else (a_tr + b_tr * future_t)
      future_F <- future_trend * future_c

      list(
        D = D, N = N, T_len = T_len, season_pos = season_pos,
        cma_unfilled = cma_unfilled, cma_filled = cma_filled,
        ratios = ratios, raw_factors = raw_factors,
        scale_mult = scale_mult, final_factors = final_factors,
        c_vec = c_vec, deseas = deseas,
        a_tr = a_tr, b_tr = b_tr, trend_in = trend_in, fitted_F = fitted_F,
        future_t = future_t, future_pos = future_pos,
        future_c = future_c, future_trend = future_trend, future_F = future_F
      )
    })

    output$seas_summary <- renderPrint({
      s <- seas_calc()
      cat(sprintf("Season Length (N) = %d   |   Total Observations (T) = %d (%d seasons)\n",
                  s$N, s$T_len, s$T_len / s$N))
      cat(sprintf("Sum of Raw Averages = %.4f   |   Rescale Multiplier (N / Sum) = %.5f\n",
                  sum(s$raw_factors), s$scale_mult))
      cat(sprintf("Normalized Seasonal Factors (c1..c%d) : %s  (Sum = %.4f)\n",
                  s$N, paste(round(s$final_factors, 4), collapse = ", "), sum(s$final_factors)))
      cat("------------------------------------------------------------------\n")
      cat(sprintf("Deseasonalized Trend Line (Slide 17) : D_hat_t = %.2f + %.4f * t\n",
                  s$a_tr, s$b_tr))
      cat(sprintf("Overall Mean of Demand (D_bar)       : %.3f\n", mean(s$D)))
    })

    output$seas_factors_table <- renderTable({
      s <- seas_calc()
      df <- data.frame(
        Season = paste0("Season ", seq_len(s$N)),
        RawAvg = s$raw_factors,
        FinalFactor = s$final_factors,
        stringsAsFactors = FALSE
      )
      tot <- data.frame(
        Season = "Sum",
        RawAvg = sum(s$raw_factors),
        FinalFactor = sum(s$final_factors),
        stringsAsFactors = FALSE
      )
      res <- rbind(df, tot)
      colnames(res) <- c("Season Position (i)", "Raw Average Ratio", "Rescaled Factor (ci)")
      res
    }, digits = 4)

    output$seas_forecast_table <- renderTable({
      s <- seas_calc()
      df <- data.frame(
        Period = s$future_t,
        Season = paste0("S", s$future_pos),
        Trend = s$future_trend,
        Factor = s$future_c,
        Forecast = s$future_F
      )
      colnames(df) <- c("Future Period (t)", "Season", "Baseline / Trend", "Factor (ci)", "Forecast (Ft)")
      df
    }, digits = 2)

    output$seas_plot <- renderPlot({
      s <- seas_calc()
      t_all <- c(seq_len(s$T_len), s$future_t)
      f_all <- c(s$fitted_F, s$future_F)
      tr_all <- c(s$trend_in, s$future_trend)
      y_rng <- range(c(s$D, s$deseas, f_all, tr_all), na.rm = TRUE)

      plot(
        seq_len(s$T_len), s$D,
        type = "b", pch = 19, col = "#1f77b4", lwd = 2,
        xlim = range(t_all),
        ylim = y_rng + c(-5, 5),
        xlab = "Period (t)",
        ylab = "Demand",
        main = "Seasonal Decomposition: Actual, Deseasonalized & Forecast"
      )
      grid()
      lines(seq_len(s$T_len), s$deseas, type = "b", pch = 1, col = "#ff7f0e", lty = 3, lwd = 1.8)
      lines(t_all, tr_all, col = "gray40", lty = 2, lwd = 2)
      lines(t_all, f_all, type = "b", pch = 17, col = "#d62728", lty = 2, lwd = 2)
      points(s$future_t, s$future_F, pch = 15, col = "#2ca02c", cex = 1.5)
      legend(
        "topleft",
        legend = c("Actual (Dt)", "Deseasonalized (Dt/ci)", "Trend Line", "Seasonal Forecast"),
        col = c("#1f77b4", "#ff7f0e", "gray40", "#d62728"),
        pch = c(19, 1, NA, 17),
        lty = c(1, 3, 2, 2),
        lwd = 2,
        bty = "n"
      )
    })

    output$seas_factor_plot <- renderPlot({
      s <- seas_calc()
      bp <- barplot(
        s$final_factors,
        names.arg = paste0("S", seq_len(s$N)),
        col = "#1f77b4",
        ylim = c(0, max(s$final_factors) * 1.25),
        ylab = "Seasonal Factor (ci)",
        main = "Seasonal Multipliers (Baseline = 1.0)"
      )
      abline(h = 1, col = "#d62728", lwd = 2, lty = 2)
      text(bp, s$final_factors, labels = round(s$final_factors, 3), pos = 3, font = 2)
    })

    output$seas_detail_table <- renderTable({
      s <- seas_calc()
      df <- data.frame(
        Period = seq_len(s$T_len),
        Season = paste0("S", s$season_pos),
        Demand = s$D,
        CMA_Raw = s$cma_unfilled,
        CMA_Filled = s$cma_filled,
        Ratio = s$ratios,
        Factor = s$c_vec,
        Deseas = s$deseas,
        Trend = s$trend_in,
        Forecast = s$fitted_F
      )
      colnames(df) <- c(
        "Period (t)", "Season", "Demand (Dt)", "Interior CMA",
        "Filled CMA / Mean", "Ratio (Dt/CMA)", "Factor (ci)",
        "Deseasonalized (Dt/ci)", "Trend (d_hat)", "Fitted Forecast"
      )
      df
    }, digits = 3, na = "-")

    # ==========================================================================
    # PART 4: WINTERS'S METHOD (TRIPLE EXPONENTIAL SMOOTHING)
    # ==========================================================================
    observeEvent(input$load_win_2s, {
      updateTextAreaInput(session, "win_d", value = "10, 20, 26, 17, 12, 23, 30, 22")
      updateNumericInput(session, "win_n", value = 4)
    })

    observeEvent(input$load_win_3s, {
      updateTextAreaInput(session, "win_d", value = "10, 20, 26, 17, 12, 23, 30, 22, 15, 27, 35, 25")
      updateNumericInput(session, "win_n", value = 4)
    })

    win_calc <- reactive({
      D <- as.numeric(unlist(strsplit(input$win_d, "[,;\\s]+")))
      D <- D[!is.na(D)]
      N <- input$win_n
      T_len <- length(D)

      validate(
        need(N >= 2, "Season length N must be at least 2."),
        need(T_len >= 2 * N, sprintf("Winters's initialization requires at least 2 full seasons (%d observations).", 2 * N))
      )

      # --- STEP 1: INITIALIZATION (Slide 21 Exact Formulas) ---
      D_s1 <- D[1:N]
      D_s2 <- D[(N + 1):(2 * N)]
      V1 <- mean(D_s1)
      V2 <- mean(D_s2)

      G0 <- (V2 - V1) / N
      # Level at end of 2nd initialization season (time 0 in Slide 21)
      S0_end2 <- V2 + G0 * ((N - 1) / 2)
      # Equivalent level at t = 0 before Period 1 so we can also smooth across all t = 1..T
      S0_start <- V1 - G0 * ((N + 1) / 2)

      j_seq <- seq_len(N)
      c1_j <- D_s1 / (V1 + (j_seq - (N + 1) / 2) * G0)
      c2_j <- D_s2 / (V2 + (j_seq - (N + 1) / 2) * G0)
      raw_c <- (c1_j + c2_j) / 2
      init_c <- raw_c * (N / sum(raw_c))

      # --- STEP 2: RECURSIVE UPDATING & FORECASTING (Slide 20) ---
      alpha <- input$win_alpha
      beta <- input$win_beta
      gamma <- input$win_gamma

      S <- numeric(T_len)
      G <- numeric(T_len)
      c_up <- numeric(T_len)
      F_vec <- numeric(T_len)

      for (t in seq_len(T_len)) {
        prev_S <- if (t == 1) S0_start else S[t - 1]
        prev_G <- if (t == 1) G0 else G[t - 1]
        prev_c <- if (t <= N) init_c[t] else c_up[t - N]

        F_vec[t] <- (prev_S + prev_G) * prev_c
        S[t] <- alpha * (D[t] / prev_c) + (1 - alpha) * (prev_S + prev_G)
        G[t] <- beta * (S[t] - prev_S) + (1 - beta) * prev_G
        c_up[t] <- gamma * (D[t] / S[t]) + (1 - gamma) * prev_c
      }

      err <- D - F_vec
      mad_val <- mean(abs(err))
      mape_val <- mean(abs(err / D)) * 100

      # Future forecasts Ft,t+tau = (St + tau * Gt) * c_(t + tau - N)
      tau_max <- max(1, input$win_tau)
      latest_c_by_pos <- numeric(N)
      for (t in seq_len(T_len)) {
        pos <- ((t - 1) %% N) + 1
        latest_c_by_pos[pos] <- c_up[t]
      }

      future_tau <- seq_len(tau_max)
      future_t <- T_len + future_tau
      future_pos <- ((future_t - 1) %% N) + 1
      future_c <- latest_c_by_pos[future_pos]
      future_F <- (S[T_len] + future_tau * G[T_len]) * future_c

      list(
        D = D, N = N, T_len = T_len,
        V1 = V1, V2 = V2, G0 = G0, S0_end2 = S0_end2, S0_start = S0_start,
        c1_j = c1_j, c2_j = c2_j, raw_c = raw_c, init_c = init_c,
        S = S, G = G, c_up = c_up, F_vec = F_vec,
        err = err, MAD = mad_val, MAPE = mape_val,
        future_tau = future_tau, future_t = future_t,
        future_pos = future_pos, future_c = future_c, future_F = future_F
      )
    })

    output$win_summary <- renderPrint({
      w <- win_calc()
      cat("=== STEP 1: WINTERS'S INITIALIZATION (First 2 Seasons, Slide 21) ===\n")
      cat(sprintf("Season 1 Mean (V1) = %.3f   |   Season 2 Mean (V2) = %.3f\n", w$V1, w$V2))
      cat(sprintf("Initial Slope (G0 = (V2 - V1)/N)              : %.4f\n", w$G0))
      cat(sprintf("Initial Level at End of Season 2 (S0, Slide 21): %.4f\n", w$S0_end2))
      cat(sprintf("Initial Seasonal Factors (c1..c%d)            : %s (Sum = %.2f)\n\n",
                  w$N, paste(round(w$init_c, 4), collapse = ", "), sum(w$init_c)))

      cat("=== STEP 2: SMOOTHING & FORECAST SUMMARY (Slide 20) ===\n")
      cat(sprintf("Final Level (S_%d) = %.2f   |   Final Trend (G_%d) = %.3f\n",
                  w$T_len, w$S[w$T_len], w$T_len, w$G[w$T_len]))
      cat(sprintf("In-Sample Errors -> MAD = %.2f   |   MAPE = %.2f%%\n", w$MAD, w$MAPE))
      cat(sprintf("Next Period Forecast (F_%d,%d) = (%.2f + 1*%.3f) * %.4f = %.2f\n",
                  w$T_len, w$T_len + 1, w$S[w$T_len], w$G[w$T_len], w$future_c[1], w$future_F[1]))
    })

    output$win_init_table <- renderTable({
      w <- win_calc()
      df <- data.frame(
        Position = paste0("j = ", seq_len(w$N)),
        c1j = w$c1_j,
        c2j = w$c2_j,
        RawAvg = w$raw_c,
        NormC = w$init_c
      )
      colnames(df) <- c("Season Pos (j)", "Season 1 (c1j)", "Season 2 (c2j)", "Average", "Normalized (cj)")
      df
    }, digits = 4)

    output$win_future_table <- renderTable({
      w <- win_calc()
      df <- data.frame(
        Tau = w$future_tau,
        Period = w$future_t,
        Season = paste0("S", w$future_pos),
        ProjLevel = w$S[w$T_len] + w$future_tau * w$G[w$T_len],
        Factor = w$future_c,
        Forecast = w$future_F
      )
      colnames(df) <- c("Steps Ahead (tau)", "Period (t+tau)", "Season", "St + tau*Gt", "Factor (c)", "Forecast (Ft,t+tau)")
      df
    }, digits = 2)

    output$win_plot <- renderPlot({
      w <- win_calc()
      t_all <- c(seq_len(w$T_len), w$future_t)
      f_all <- c(w$F_vec, w$future_F)
      y_rng <- range(c(w$D, f_all, w$S), na.rm = TRUE)

      plot(
        seq_len(w$T_len), w$D,
        type = "b", pch = 19, col = "#1f77b4", lwd = 2,
        xlim = range(t_all),
        ylim = y_rng + c(-5, 5),
        xlab = "Period (t)",
        ylab = "Demand & Forecast",
        main = "Winters's Method: Actual Demand, Smoothed Level & Forecasts"
      )
      grid()
      lines(seq_len(w$T_len), w$S, col = "gray40", lwd = 2, lty = 3)
      lines(t_all, f_all, type = "b", pch = 17, col = "#d62728", lwd = 2, lty = 2)
      points(w$future_t, w$future_F, pch = 15, col = "#2ca02c", cex = 1.5)
      legend(
        "topleft",
        legend = c("Actual Demand (Dt)", "Smoothed Level (St)", "Winters Forecast (Ft)", "Future Forecast (Ft,t+tau)"),
        col = c("#1f77b4", "gray40", "#d62728", "#2ca02c"),
        pch = c(19, NA, 17, 15),
        lty = c(1, 3, 2, NA),
        lwd = 2,
        bty = "n"
      )
    })

    output$win_table <- renderTable({
      w <- win_calc()
      pos <- ((seq_len(w$T_len) - 1) %% w$N) + 1
      df <- data.frame(
        Period = seq_len(w$T_len),
        Season = paste0("S", pos),
        Demand = w$D,
        Level = w$S,
        Trend = w$G,
        SeasFactor = w$c_up,
        Forecast = w$F_vec,
        Error = w$err
      )
      colnames(df) <- c(
        "Period (t)", "Season", "Demand (Dt)", "Level (St)",
        "Trend (Gt)", "Updated Factor (ct)", "Forecast (Ft)", "Error (et)"
      )
      df
    }, digits = 3)
  })
}
