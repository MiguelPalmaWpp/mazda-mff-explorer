app_ui <- shiny::fluidPage(
  title = "Mazda MFF Explorer",
  shiny::tags$head(
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    shiny::tags$script(src = "custom.js")
  ),
  shiny::tags$header(class = "wpp-app-header",
    shiny::div(class = "wpp-header-brand",
      shiny::tags$img(src = "mazda-logo.png", alt = "WPP Media")
    ),
    shiny::div(class = "navbar-center-block",
      shiny::tags$span(class = "app-main-title", "MFF Explorer"),
      shiny::tags$span(class = "app-subtitle", "Marketing Mix Modeling · Advanced Analytics Colombia")
    ),
    shiny::div(class = "wpp-header-right",
      shiny::tags$img(src = "mazda-logo.png", alt = "")
    )
  ),
  shiny::tags$main(class = "eda-shell",
    shiny::div(class = "eda-intro",
      shiny::div(class = "eda-eyebrow", "MAZDA · DATA EXPLORATION"),
      shiny::h1("Understand every signal in your MFF"),
      shiny::p("Upload a Mazda MFF, choose a numeric variable, and inspect its original daily values in an interactive chart."),
      shiny::div(class = "eda-steps",
        shiny::div(class = "eda-step", shiny::span("01"), shiny::div(shiny::strong("Upload"), shiny::tags$small("CSV, Excel or RData"))),
        shiny::div(class = "eda-step", shiny::span("02"), shiny::div(shiny::strong("Choose"), shiny::tags$small("Search any numeric column"))),
        shiny::div(class = "eda-step", shiny::span("03"), shiny::div(shiny::strong("Explore"), shiny::tags$small("Zoom, compare and export")))
      )
    ),
    shiny::tags$section(class = "section-card eda-section",
      shiny::div(class = "eda-section-heading",
        shiny::div(
          shiny::span(class = "eda-step-number", "1"),
          shiny::div(shiny::h2("Upload a Mazda MFF"),
                     shiny::p("Start with a file that contains a Date column and at least one numeric variable."))
        )
      ),
      shiny::div(class = "upload-zone",
        shiny::fileInput(
          "file_upload", "Choose an MFF file",
          accept = c(".csv", ".xlsx", ".xls", ".RData"),
          width = "100%",
          placeholder = "Select a file or drop it here"
        ),
        shiny::p(class = "eda-supporting-text", "Supported: .csv, .xlsx, .xls and .RData · Maximum file size: 300 MB")
      ),
      shiny::div(class = "eda-explainer",
        shiny::icon("calendar-check"),
        shiny::span("Mazda calendar fields can restore daily dates when an export has flattened or incomplete dates. The app reports any recovery or removed exact duplicates after upload.")
      ),
      shiny::uiOutput("upload_message"),
      shiny::uiOutput("bulk_buttons_ui")
    ),
    shiny::uiOutput("analysis_ui"),
    shiny::tags$footer(class = "eda-footer", "Mazda MFF Explorer · Advanced Analytics Colombia")
  )
)

analysis_panel_ui <- function(cols_numeric) {
  shiny::tagList(
    shiny::tags$section(class = "section-card eda-section",
      shiny::div(class = "eda-section-heading",
        shiny::div(
          shiny::span(class = "eda-step-number", "2"),
          shiny::div(shiny::h2("Choose a variable"),
                     shiny::p("Search the columns in your MFF. The chart updates when you select a different variable."))
        )
      ),
      shiny::selectInput("var_select", "Numeric variable", choices = cols_numeric, width = "100%"),
      shiny::p(class = "eda-supporting-text", "Values are shown as supplied in the MFF; they are not summed, smoothed or transformed."),
      shiny::uiOutput("data_overview")
    ),
    shiny::tags$section(class = "section-card eda-section",
      shiny::div(class = "eda-section-heading",
        shiny::div(
          shiny::span(class = "eda-step-number", "3"),
          shiny::div(shiny::h2("Explore the time series"),
                     shiny::p("Hover to read a value, drag to zoom, and double-click the chart to reset the axes."))
        )
      ),
      shiny::div(class = "chart-box",
        shiny::div(class = "header-with-btn",
          shiny::div(
            shiny::div(class = "eda-eyebrow", "SELECTED SERIES"),
            shiny::h3(class = "selected-variable", shiny::textOutput("selected_variable", inline = TRUE))
          ),
          shiny::downloadButton("download_chart_data", "Download data (.csv)", class = "custom-download-btn btn-sm")
        ),
        shiny::uiOutput("variable_overview"),
        shiny::div(class = "chart-controls",
          shiny::radioButtons("chart_type", "Display", choices = c("Line" = "line", "Bars" = "bar"), selected = "line", inline = TRUE),
          shiny::div(class = "chart-range-actions",
            shiny::actionButton("focus_range", "Focus on typical values", class = "range-btn"),
            shiny::actionButton("show_all", "Show all values", class = "range-btn")
          )
        ),
        shiny::uiOutput("focus_message"),
        plotly::plotlyOutput("ts_plot", height = "500px", width = "100%"),
        shiny::p(class = "chart-help", "The full linear range is shown initially. Use the Plotly toolbar to pan, reset axes, or save the chart as a PNG. Focus may hide extreme values from the visible range; it never removes them from the data.")
      )
    ),
    shiny::tags$section(class = "section-card eda-section",
      shiny::div(class = "header-with-btn eda-summary-heading",
        shiny::div(
          shiny::div(class = "eda-eyebrow", "DATA QUALITY"),
          shiny::h2("Summary by year"),
          shiny::p("Review coverage, averages, upper outliers and repeated values for the selected variable.")
        ),
        shiny::downloadButton("download_summary_table", "Download summary (.csv)", class = "custom-download-btn btn-sm")
      ),
      shiny::div(class = "summary-table-container", DT::DTOutput("summary_table")),
      shiny::tags$details(class = "eda-methodology",
        shiny::tags$summary("How are these metrics calculated?"),
        shiny::div(
          shiny::p(shiny::strong("Periods with Data"), " counts nonzero, nonmissing values. Data Completeness is that count divided by all dates in the year."),
          shiny::p(shiny::strong("Upper Outliers"), " uses a regression residual limit for trending series and an interquartile range limit for stationary series. Zeros are excluded."),
          shiny::p(shiny::strong("Data Behavior"), " flags five or more consecutive repeated nonzero values.")
        )
      )
    )
  )
}

