app_server <- function(input, output, session) {
  reactive_data <- shiny::reactiveVal(NULL)
  view_mode <- shiny::reactiveVal("full")
  view_revision <- shiny::reactiveVal(0L)
  
  numeric_columns <- shiny::reactive({
    shiny::req(reactive_data())
    names(reactive_data())[vapply(reactive_data(), is.numeric, logical(1))]
  })
  
  plot_data <- shiny::reactive({
    shiny::req(reactive_data(), input$var_select)
    shiny::req(input$var_select %in% numeric_columns())
    prepare_plot_series(reactive_data(), input$var_select)
  })
  
  summary_reactive <- shiny::reactive({
    shiny::req(reactive_data(), input$var_select)
    summary_by_variable(reactive_data(), input$var_select, include_global_details = TRUE)
  })
  
  shiny::observeEvent(input$file_upload, {
    shiny::req(input$file_upload)
    
    tryCatch({
      df_raw <- load_uploaded_data(input$file_upload)
      res <- process_data_and_dates(df_raw)
      reactive_data(res$data)
      view_mode("full")

      source_label <- if(!is.null(res$sheet)) paste0(" Sheet: '", res$sheet, "'.") else ""
      nonfinite_label <- if(res$nonfinite_count > 0) {
        paste0(" ", res$nonfinite_count, " non-finite values were treated as missing.")
      } else {
        ""
      }
      calendar_label <- if(!is.null(res$date_note)) paste0(" ", res$date_note) else ""
      duplicate_label <- if(res$duplicate_row_count > 0) {
        paste0(" ", res$duplicate_row_count, " exact duplicate rows were removed.")
      } else {
        ""
      }
      
      output$upload_message <- shiny::renderUI({
        shiny::div(
          class = "eda-status eda-status--success", role = "status",
          shiny::icon("check-circle"),
          shiny::div(
            shiny::strong("MFF ready to explore"),
            shiny::p(paste0("Date column: ", res$col_detected,
                            " · ", format(nrow(res$data), big.mark = ","), " rows · ",
                            length(res$numeric_columns), " numeric variables.", source_label,
                            nonfinite_label, calendar_label, duplicate_label))
          )
        )
      })
    }, error = function(e) {
      reactive_data(NULL)
      view_mode("full")
      output$upload_message <- shiny::renderUI({
        shiny::div(class = "eda-status eda-status--error", role = "alert",
                   shiny::icon("exclamation-circle"),
                   shiny::div(shiny::strong("The MFF could not be loaded"),
                              shiny::p(e$message)))
      })
    })
  })
  
  output$bulk_buttons_ui <- shiny::renderUI({
    if(is.null(reactive_data())) return(NULL)
    shiny::div(class = "eda-exports",
      shiny::div(
        shiny::strong("Export the full analysis"),
        shiny::p("The Excel workbook includes the dataset, flighting table and summaries. The ZIP contains a chart for every numeric variable.")
      ),
      shiny::div(class = "eda-export-actions",
        shiny::downloadButton("download_master_data", "Master workbook (.xlsx)", class = "custom-download-btn"),
        shiny::downloadButton("download_plots_zip", "All charts (.zip)", class = "custom-download-btn")
      )
    )
  })

  output$data_overview <- shiny::renderUI({
    df <- reactive_data()
    shiny::req(df)
    shiny::div(class = "eda-metric-strip",
      shiny::div(class = "eda-metric", shiny::span("DATE RANGE"),
                 shiny::strong(paste(format(min(df$Date), "%b %Y"), "–", format(max(df$Date), "%b %Y")))),
      shiny::div(class = "eda-metric", shiny::span("DAILY ROWS"),
                 shiny::strong(format(nrow(df), big.mark = ","))),
      shiny::div(class = "eda-metric", shiny::span("NUMERIC VARIABLES"),
                 shiny::strong(length(numeric_columns())))
    )
  })

  output$variable_overview <- shiny::renderUI({
    values <- plot_data()$Value
    finite <- values[is.finite(values)]
    display_number <- function(value) format(signif(value, 5), big.mark = ",", trim = TRUE, scientific = FALSE)
    shiny::div(class = "eda-metric-strip eda-metric-strip--chart",
      shiny::div(class = "eda-metric", shiny::span("AVAILABLE VALUES"),
                 shiny::strong(format(length(finite), big.mark = ","))),
      shiny::div(class = "eda-metric", shiny::span("NONZERO DAYS"),
                 shiny::strong(format(sum(finite != 0), big.mark = ","))),
      shiny::div(class = "eda-metric", shiny::span("MAXIMUM"),
                 shiny::strong(if(length(finite)) display_number(max(finite)) else "—")),
      shiny::div(class = "eda-metric", shiny::span("MISSING VALUES"),
                 shiny::strong(format(sum(!is.finite(values)), big.mark = ",")))
    )
  })
  
  output$download_master_data <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%y%m%d%H%M%S"), " - Master Dataset.xlsx")
    },
    content = function(file) {
      shiny::req(reactive_data())
      shiny::showNotification("Generating Master Excel...", id = "dl_excel")
      on.exit(shiny::removeNotification(id = "dl_excel"))
      
      df <- reactive_data()
      cols_num <- numeric_columns()
      df_flighting <- build_flighting_table(df, cols_num)
      df_summary <- build_summary_table(df, cols_num)
      
      write_master_workbook(df, df_flighting, df_summary, file)
    }
  )
  
  output$download_plots_zip <- shiny::downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%y%m%d%H%M%S"), " - All Plots.zip")
    },
    content = function(file) {
      shiny::req(reactive_data())
      shiny::showNotification("Generating plots...", id = "dl_zip")
      on.exit(shiny::removeNotification(id = "dl_zip"))
      
      df <- reactive_data()
      cols_num <- numeric_columns()
      zip_root <- tempfile(pattern = "plots_to_zip_")
      plots_dir <- file.path(zip_root, "plots")
      
      on.exit(unlink(zip_root, recursive = TRUE), add = TRUE)
      dir.create(plots_dir, recursive = TRUE)
      
      for(var in cols_num) {
        ggplot2::ggsave(
          file.path(plots_dir, safe_plot_filename(var)),
          build_trend_plot(df, var),
          width = 10,
          height = 6
        )
      }
      
      create_plots_zip(file, zip_root)
    }
  )
  
  output$analysis_ui <- shiny::renderUI({
    if(is.null(reactive_data())) return(NULL)
    analysis_panel_ui(numeric_columns())
  })
  
  shiny::observeEvent(input$var_select, {
    view_mode("full")
  })

  shiny::observeEvent(input$focus_range, {
    focus <- focus_y_range(plot_data()$Value)
    if(is.null(focus)) {
      shiny::showNotification("No extreme values need a focused range for this variable.",
                              type = "message")
    } else {
      view_mode("focus")
      view_revision(view_revision() + 1L)
    }
  })

  shiny::observeEvent(input$show_all, {
    view_mode("full")
    view_revision(view_revision() + 1L)
  })

  output$selected_variable <- shiny::renderText({
    shiny::req(reactive_data(), input$var_select)
    input$var_select
  })

  output$focus_message <- shiny::renderUI({
    shiny::req(plot_data())
    if(view_mode() != "focus") return(NULL)
    focus <- focus_y_range(plot_data()$Value)
    if(is.null(focus)) return(NULL)
    shiny::div(
      class = "focus-message",
      paste0("Focused Y range: ", focus$outside,
             " value(s) outside this view. Use 'Show all values' to restore the full range.")
    )
  })

  output$ts_plot <- plotly::renderPlotly({
    view_revision()
    series <- plot_data()
    shiny::validate(shiny::need(any(is.finite(series$Value)),
                                "No numeric values are available for this variable."))
    focus <- if(view_mode() == "focus") focus_y_range(series$Value) else NULL
    build_interactive_plot(
      series, input$var_select,
      chart_type = if(is.null(input$chart_type)) "line" else input$chart_type,
      value_col = "Value",
      y_range = if(is.null(focus)) NULL else focus$range
    )
  })
  
  output$summary_table <- DT::renderDT({
    DT::datatable(
      summary_reactive(),
      class = "summary-table display",
      options = list(dom = "t", paging = FALSE, scrollX = TRUE, autoWidth = FALSE),
      rownames = FALSE,
      selection = "none"
    ) %>%
      DT::formatStyle("Upper Outliers", fontWeight = "bold", color = DT::styleInterval(0, c("black", "#c0392b"))) %>%
      DT::formatStyle("Total Datapoints", fontWeight = "bold", color = DT::styleInterval(99, c("#c0392b", "black"))) %>%
      DT::formatStyle("Data Behavior", backgroundColor = DT::styleEqual("Constant blocks detected", c("#fff3cd")))
  })
  
  output$download_chart_data <- shiny::downloadHandler(
    filename = function() {
      paste0("Data_", tools::file_path_sans_ext(safe_plot_filename(input$var_select)), ".csv")
    },
    content = function(file) {
      utils::write.csv(plot_data(), file, row.names = FALSE)
    }
  )
  
  output$download_summary_table <- shiny::downloadHandler(
    filename = function() {
      paste0("Summary_", input$var_select, ".csv")
    },
    content = function(file) {
      utils::write.csv(summary_reactive(), file, row.names = FALSE)
    }
  )
}
