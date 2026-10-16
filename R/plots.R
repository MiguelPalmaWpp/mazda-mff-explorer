safe_plot_filename <- function(var_name) {
  paste0(gsub("[^[:alnum:]]", "_", var_name), ".png")
}

prepare_plot_series <- function(df, value_col) {
  data.frame(
    Date = as.Date(df$Date),
    Value = as.numeric(df[[value_col]])
  ) |>
    dplyr::arrange(.data$Date)
}

focus_y_range <- function(values) {
  finite <- values[is.finite(values)]
  if(length(finite) < 20 || length(unique(finite)) < 3) return(NULL)

  if(all(finite >= 0)) {
    active <- finite[finite > 0]
    if(length(active) < 20) return(NULL)
    upper <- as.numeric(stats::quantile(active, 0.98, names = FALSE))
    range <- c(0, upper * 1.08)
  } else {
    bounds <- as.numeric(stats::quantile(finite, c(0.02, 0.98), names = FALSE))
    padding <- max(diff(bounds) * 0.08, .Machine$double.eps)
    range <- bounds + c(-padding, padding)
  }

  outside <- sum(finite < range[1] | finite > range[2])
  if(outside == 0 || diff(range) <= 0) return(NULL)
  list(range = range, outside = outside)
}

build_interactive_plot <- function(df, var_name, chart_type = "line",
                                   value_col = var_name, y_range = NULL) {
  series <- prepare_plot_series(df, value_col)
  finite <- series$Value[is.finite(series$Value)]
  max_magnitude <- if(length(finite)) max(abs(finite)) else 0
  digits <- if(max_magnitude < 1) 4 else if(max_magnitude < 100) 2 else 0
  hover <- paste0(
    "<b>%{x|%b %d, %Y}</b><br>Value: %{y:,.",
    digits, "f}<extra></extra>"
  )

  if(chart_type == "bar") {
    plot <- plotly::plot_ly(
      data = series, x = ~Date, y = ~Value,
      type = "bar", marker = list(color = "#5B9BD5"),
      hovertemplate = hover
    )
  } else {
    plot <- plotly::plot_ly(
      data = series, x = ~Date, y = ~Value,
      type = "scatter", mode = "lines",
      line = list(color = "#5B9BD5", width = 2),
      connectgaps = FALSE, hovertemplate = hover
    )
  }

  y_axis <- list(
    title = "Value", tickformat = if(digits > 0) paste0(",.", digits, "f") else ",.0f",
    gridcolor = "#e8eef5", zerolinecolor = "#c8d6e5",
    tickfont = list(size = 11, color = "#334155"),
    titlefont = list(size = 12, color = "#334155")
  )
  if(is.null(y_range)) {
    y_axis$autorange <- TRUE
    if(length(finite) && all(finite >= 0)) y_axis$rangemode <- "tozero"
  } else {
    y_axis$range <- y_range
    y_axis$autorange <- FALSE
  }

  plot |>
    plotly::layout(
      showlegend = FALSE, hovermode = "closest",
      margin = list(l = 85, r = 28, t = 50, b = 62),
      paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
      xaxis = list(
        title = "Date", type = "date",
        gridcolor = "#e8eef5", zerolinecolor = "#e8eef5",
        tickfont = list(size = 11, color = "#334155"),
        titlefont = list(size = 12, color = "#334155"),
        rangeselector = list(buttons = list(
          list(count = 6, label = "6m", step = "month", stepmode = "backward"),
          list(count = 1, label = "1y", step = "year", stepmode = "backward"),
          list(step = "all", label = "All")
        ))
      ),
      yaxis = y_axis
    ) |>
    plotly::config(
      displaylogo = FALSE, responsive = TRUE,
      toImageButtonOptions = list(
        format = "png", filename = tools::file_path_sans_ext(safe_plot_filename(var_name)),
        height = 650, width = 1200, scale = 2
      ),
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )
}

build_trend_plot <- function(df, var_name, value_col = var_name,
                             chart_type = "line") {
  series <- prepare_plot_series(df, value_col)
  plot <- ggplot2::ggplot(series, ggplot2::aes(x = .data$Date, y = .data$Value))
  if(chart_type == "bar") {
    plot <- plot + ggplot2::geom_col(fill = "#5B9BD5", width = 0.8)
  } else {
    plot <- plot + ggplot2::geom_line(color = "#5B9BD5", linewidth = 0.65,
                                     na.rm = TRUE)
  }

  plot +
    ggplot2::labs(
      title = paste(strwrap(gsub("_", " ", var_name), width = 42), collapse = "\n"),
      x = "Date", y = "Value"
    ) +
    ggplot2::scale_x_date(date_labels = "%b %Y") +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 10, color = "#2c3e50"),
      axis.title = ggplot2::element_text(size = 9, color = "#334155"),
      axis.text = ggplot2::element_text(size = 8, color = "#334155"),
      panel.grid.major = ggplot2::element_line(color = "#e8eef5", linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank()
    )
}

create_plots_zip <- function(zip_file, zip_root) {
  files <- list.files(zip_root, recursive = TRUE)
  if(length(files) == 0) {
    stop("No plot files were generated for the ZIP.")
  }
  
  zip_bin <- Sys.which("zip")
  if(nzchar(zip_bin)) {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(zip_root)
    utils::zip(zip_file, files)
    return(invisible(TRUE))
  }
  
  ps_source <- file.path(normalizePath(zip_root, winslash = "\\", mustWork = TRUE), "*")
  ps_dest <- normalizePath(dirname(zip_file), winslash = "\\", mustWork = TRUE)
  ps_zip <- file.path(ps_dest, basename(zip_file))
  escaped_source <- gsub("'", "''", ps_source, fixed = TRUE)
  escaped_zip <- gsub("'", "''", ps_zip, fixed = TRUE)
  
  result <- system2(
    "powershell",
    c(
      "-NoProfile",
      "-Command",
      shQuote(paste0("Compress-Archive -Path '", escaped_source, "' -DestinationPath '", escaped_zip, "' -Force"))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  
  if(!file.exists(zip_file)) {
    stop("Could not create ZIP archive: ", paste(result, collapse = "\n"))
  }
  
  invisible(TRUE)
}
