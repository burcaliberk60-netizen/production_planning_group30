library(shiny)

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve", learning_ui("learning")),
  tabPanel("Forecasting (Week 2)", forecasting_ui("forecasting")),
  tabPanel("Forecasting II (Week 3)", exponential_seasonal_ui("exp_seasonal"))
)

server <- function(input, output, session) {
  learning_server("learning")
  forecasting_server("forecasting")
  exponential_seasonal_server("exp_seasonal")
}

shinyApp(ui, server)
