library(shiny)

shinyUI(fluidPage(
  titlePanel("Automatic Forecasting"),
  includeMarkdown("introduction.Rmd"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("tag_selector"),
      uiOutput("tag_series_selector"),
      uiOutput("tag_series_id_selector"),
      tableOutput("series_id_information")
    ),
    mainPanel(
      plotOutput("series_plot"),
      h3("Forecast"),
      DT::dataTableOutput("tabular_forecast_data"),
      h3("Actual data and in-sample fitted data"),
      DT::dataTableOutput("tabular_actual_v_fitted_data")
    )
  )
))
