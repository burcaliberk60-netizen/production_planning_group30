forecasting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      # 1. SEKME: NEDENSEL MODEL & EN KÜÇÜK KARELER (LEAST SQUARES)
      tabPanel(
        "1. Doğrusal Regresyon (En Küçük Kareler)",
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Veri Girişi (Causal / Trend Model)"),
            helpText("Değerleri virgülle ayırarak girin. Varsayılan veriler slayttaki Mortgage Oranı (X) ve Konut Satışı (Y) örneğidir."),
            textInput(
              ns("reg_x"),
              "Bağımsız Değişken X (veya Zaman t):",
              value = "6, 7, 8, 9, 10"
            ),
            textInput(
              ns("reg_y"),
              "Bağımlı Değişken Y (Talep / Satış):",
              value = "250, 235, 215, 200, 180"
            ),
            numericInput(
              ns("target_x"),
              "Tahmin Edilecek X Değeri (Örn: %11 faiz):",
              value = 11,
              step = 0.5
            ),
            hr(),
            h5("Formüller (Least Squares):"),
            helpText("b = Sxy / Sxx = Σ(X - X̄)(Y - Ȳ) / Σ(X - X̄)²"),
            helpText("a = Ȳ - b·X̄"),
            helpText("Ŷ = a + b·X")
          ),
          mainPanel(
            h4("Model Sonuçları ve Tahmin"),
            verbatimTextOutput(ns("reg_summary")),
            fluidRow(
              column(6, plotOutput(ns("reg_plot"), height = "320px")),
              column(6, plotOutput(ns("res_plot"), height = "320px"))
            ),
            h4("En Küçük Kareler Sapma (Deviation) Tablosu"),
            tableOutput(ns("reg_table"))
          )
        )
      ),

      # 2. SEKME: ZAMAN SERİSİ - HAREKETLİ ORTALAMA & OPTİMUM n (MAPE)
      tabPanel(
        "2. Hareketli Ortalama (MA) & MAPE Optimizasyonu",
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Zaman Serisi Talep Verisi (Dt)"),
            helpText("Geçmiş dönem talep (Dt) değerlerini virgülle ayırarak girin:"),
            textAreaInput(
              ns("ma_d"),
              "Talep Serisi (D1, D2, ..., Dt):",
              value = "250, 235, 240, 220, 215, 230, 205, 200, 195, 180, 190, 185",
              rows = 3
            ),
            checkboxInput(
              ns("auto_n"),
              "MAPE'yi en küçük yapan optimum n'i otomatik seç",
              value = TRUE
            ),
            sliderInput(
              ns("ma_n"),
              "Manuel Pencere Genişliği (n):",
              min = 1,
              max = 6,
              value = 3,
              step = 1
            ),
            hr(),
            h5("Notasyon ve Kriterler:"),
            helpText("Ft = (Dt-1 + Dt-2 + ... + Dt-n) / n"),
            helpText("et = Dt - Ft"),
            helpText("MAPE = (1/m · Σ |et / Dt|) × 100")
          ),
          mainPanel(
            h4("Optimum n Simülasyonu (MAPE Karşılaştırması)"),
            verbatimTextOutput(ns("ma_summary")),
            tableOutput(ns("mape_comp_table")),
            plotOutput(ns("ma_plot"), height = "320px"),
            h4("Dönemsel Tahmin ve Hata Tablosu"),
            tableOutput(ns("ma_detail_table"))
          )
        )
      )
    )
  )
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- 1. BÖLÜM: DOĞRUSAL REGRESYON HESAPLAMALARI ---
    reg_data <- reactive({
      x <- as.numeric(unlist(strsplit(input$reg_x, "[,;\\s]+")))
      y <- as.numeric(unlist(strsplit(input$reg_y, "[,;\\s]+")))
      x <- x[!is.na(x)]
      y <- y[!is.na(y)]

      validate(
        need(length(x) >= 2 && length(y) >= 2, "Lütfen en az 2 veri noktası girin."),
        need(length(x) == length(y), "X ve Y uzunlukları birbirine eşit olmalıdır.")
      )

      x_bar <- mean(x)
      y_bar <- mean(y)
      dx <- x - x_bar
      dy <- y - y_bar
      dx_dy <- dx * dy
      dx2 <- dx^2

      s_xy <- sum(dx_dy)
      s_xx <- sum(dx2)
      validate(need(s_xx > 0, "X değerlerinin tamamı aynı olamaz."))

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
      cat(sprintf("Ortalama X (X̄) = %.2f   |   Ortalama Y (Ȳ) = %.2f\n", d$x_bar, d$y_bar))
      cat(sprintf("Sxy = %.2f              |   Sxx = %.2f\n", d$s_xy, d$s_xx))
      cat(sprintf("Eğim (b) = Sxy / Sxx = %.4f\n", d$b))
      cat(sprintf("Kesim Noktası (a) = Ȳ - b·X̄ = %.4f\n", d$a))
      cat("------------------------------------------------------------\n")
      cat(sprintf("Tahmin Denklemi : Ŷ = %.2f %s %.2f * X\n", d$a, sign_str, abs(d$b)))
      cat(sprintf("X = %.2f için Tahmin (Ŷ) : %.2f (≈ %.0f birim)\n",
                  input$target_x, d$y_pred, round(d$y_pred)))
      cat(sprintf("SSE (Hata Kareler Toplamı) : %.2f   |   MAPE : %%%.2f\n", d$sse, d$mape))
    })

    output$reg_table <- renderTable({
      d <- reg_data()
      df <- data.frame(
        Gozlem = seq_along(d$x),
        X = d$x,
        Y = d$y,
        `X - X̄` = d$dx,
        `Y - Ȳ` = d$dy,
        `(X - X̄)(Y - Ȳ)` = d$dx_dy,
        `(X - X̄)²` = d$dx2,
        `Tahmin (Ŷ)` = d$y_hat,
        `Hata (e = Y - Ŷ)` = d$e,
        check.names = FALSE
      )
      toplam <- data.frame(
        Gozlem = "Toplam",
        X = sum(d$x),
        Y = sum(d$y),
        `X - X̄` = sum(d$dx),
        `Y - Ȳ` = sum(d$dy),
        `(X - X̄)(Y - Ȳ)` = d$s_xy,
        `(X - X̄)²` = d$s_xx,
        `Tahmin (Ŷ)` = sum(d$y_hat),
        `Hata (e = Y - Ŷ)` = sum(d$e),
        check.names = FALSE
      )
      rbind(df, toplam)
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
        xlab = "X (Açıklayıcı Değişken)",
        ylab = "Y (Talep / Satış)",
        main = "En Küçük Kareler Regresyon Doğrusu"
      )
      grid()
      abline(a = d$a, b = d$b, col = "#d62728", lwd = 2)
      points(input$target_x, d$y_pred, pch = 17, col = "#2ca02c", cex = 1.6)
      segments(d$x, d$y, d$x, d$y_hat, col = "gray50", lty = 2)
      legend(
        "topright",
        legend = c("Gözlenen (Y)", "Regresyon Doğrusu (Ŷ)", "Yeni Tahmin"),
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
        ylab = "Kalıntı (e = Y - Ŷ)",
        main = "Kalıntı (Residual) Kontrol Grafiği"
      )
      grid()
      abline(h = 0, col = "black", lwd = 2, lty = 2)
    })

    # --- 2. BÖLÜM: HAREKETLİ ORTALAMA (MA) & MAPE OPTİMİZASYONU ---
    ma_sim <- reactive({
      D <- as.numeric(unlist(strsplit(input$ma_d, "[,;\\s]+")))
      D <- D[!is.na(D)]
      N <- length(D)
      validate(need(N >= 4, "Simülasyon için en az 4 dönemlik talep verisi girin."))

      max_n <- max(2, min(floor(N - 1), 10))

      # Her n değeri için MA tahminlerini ve hata metriklerini hesapla
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

      cat(sprintf("Toplam Gözlem Sayısı (T) : %d dönem\n", res$N))
      cat(sprintf("MAPE'yi Minimum Yapan Optimum n : n = %d (MAPE = %%%.2f)\n",
                  res$best_n, best_info$MAPE))
      cat("------------------------------------------------------------\n")
      cat(sprintf("Seçili Model : MA(%d)\n", res$chosen_n))
      cat(sprintf("Gelecek Dönem Tahmini (F_%d,%d = F_%d) : %.2f\n",
                  res$N, res$N + 1, res$N + 1, chosen_info$next_F))
      cat(sprintf("Seçili Model Hataları -> MAPE: %%%.2f | MAD: %.2f | MSE: %.2f\n",
                  chosen_info$MAPE, chosen_info$MAD, chosen_info$MSE))
    })

    output$mape_comp_table <- renderTable({
      res <- ma_sim()
      df <- do.call(rbind, lapply(res$comp, function(x) {
        data.frame(
          `Pencere Boyutu (n)` = x$n,
          `MAD` = x$MAD,
          `MSE` = x$MSE,
          `MAPE (%)` = x$MAPE,
          `Gelecek Dönem Tahmini (Ft+1)` = x$next_F,
          `Durum` = ifelse(x$n == res$best_n, "★ En Düşük MAPE (Optimum)", ""),
          check.names = FALSE
        )
      }))
      df
    }, digits = 2)

    output$ma_plot <- renderPlot({
      res <- ma_sim()
      chosen <- res$comp[[res$chosen_n]]
      t_axis <- 1:(res$N + 1)
      D_ext <- c(res$D, NA)
      F_ext <- c(chosen$F_vec, chosen$next_F)

      y_rng <- range(c(res$D, F_ext), na.rm = TRUE)
      plot(
        1:res$N, res$D,
        type = "b", pch = 19, col = "#1f77b4", lwd = 2,
        xlim = c(1, res$N + 1),
        ylim = y_rng + c(-10, 10),
        xlab = "Dönem (t)",
        ylab = "Talep (Dt) ve Tahmin (Ft)",
        main = sprintf("Hareketli Ortalama MA(%d) - Gerçekleşen vs. Tahmin", res$chosen_n)
      )
      grid()
      lines(t_axis, F_ext, type = "b", pch = 17, col = "#d62728", lwd = 2, lty = 2)
      points(res$N + 1, chosen$next_F, pch = 15, col = "#2ca02c", cex = 1.6)
      legend(
        "topright",
        legend = c("Gerçekleşen Talep (Dt)", sprintf("MA(%d) Tahmini (Ft)", res$chosen_n), "Gelecek Dönem (Ft+1)"),
        col = c("#1f77b4", "#d62728", "#2ca02c"),
        pch = c(19, 17, 15),
        lty = c(1, 2, NA),
        lwd = c(2, 2, NA),
        bty = "n"
      )
    })

    output$ma_detail_table <- renderTable({
      res <- ma_sim()
      chosen <- res$comp[[res$chosen_n]]
      ape <- abs(chosen$err / res$D) * 100
      data.frame(
        `Dönem (t)` = 1:res$N,
        `Talep (Dt)` = res$D,
        `Tahmin (Ft)` = chosen$F_vec,
        `Hata (et = Dt - Ft)` = chosen$err,
        `|et| / Dt (%)` = ape,
        check.names = FALSE
      )
    }, digits = 2, na = "-")
  })
}
