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
            helpText("b = Sxy / Sxx = sum((X - X_bar)(Y - Y_bar)) / sum((X - X_bar)^2)"),
            helpText("a = Y_bar - b * X_bar"),
            helpText("Y_hat = a + b * X")
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
            helpText("MAPE = (1/m * sum(|et / Dt|)) * 100")
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
      cat(sprintf("Mean X (X_bar) = %.2f        |   Mean Y (Y_bar) = %.2f\n", d$x_bar, d$y_bar))
      cat(sprintf("Sxy = %.2f                 |   Sxx = %.2f\n", d$s_xy, d$s_xx))
      cat(sprintf("Slope (b) = Sxy / Sxx = %.4f\n", d$b))
      cat(sprintf("Intercept (a) = Y_bar - b * X_bar = %.4f\n", d$a))
      cat("------------------------------------------------------------\n")
      cat(sprintf("Fitted Forecasting Equation : Y_hat = %.2f %s %.2f * X\n", d$a, sign_str, abs(d$b)))
      cat(sprintf("Forecast for X = %.2f       : %.2f (approx. %.0f units)\n",
                  input$target_x, d$y_pred, round(d$y_pred)))
      cat(sprintf("SSE (Sum of Squared Errors) : %.2f   |   MAPE : %.2f%%\n", d$sse, d$mape))
    })

    output$reg_table <- renderTable({
      d <- reg_data()
      df <- data.frame(
        Observation = as.character(seq_along(d$x)),
        X = d$x,
        Y = d$y,
        dx = d$dx,
        dy = d$dy,
        dx_dy = d$dx_dy,
        dx2 = d$dx2,
        y_hat = d$y_hat,
        e = d$e,
        stringsAsFactors = FALSE
