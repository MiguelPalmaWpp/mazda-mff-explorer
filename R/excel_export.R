place_excel_pictures_in_cells <- function(file, image_map) {
  if(is.null(image_map) || nrow(image_map) == 0) return(FALSE)
  
  ps_script <- tempfile(fileext = ".ps1")
  map_file <- tempfile(fileext = ".csv")
  workbook_path <- normalizePath(file, winslash = "\\", mustWork = TRUE)
  escaped_path <- gsub("'", "''", workbook_path, fixed = TRUE)
  
  utils::write.csv(image_map, map_file, row.names = FALSE)
  escaped_map <- gsub("'", "''", normalizePath(map_file, winslash = "\\", mustWork = TRUE), fixed = TRUE)
  
  writeLines(c(
    "$ErrorActionPreference = 'Stop'",
    "$excel = $null",
    "$workbook = $null",
    "$converted = 0",
    "$targetCount = 0",
    "try {",
    "  $excel = New-Object -ComObject Excel.Application",
    "  $excel.DisplayAlerts = $false",
    "  $excel.Visible = $false",
    paste0("  $workbook = $excel.Workbooks.Open('", escaped_path, "')"),
    paste0("  $items = Import-Csv '", escaped_map, "'"),
    "  $targetCount = @($items).Count",
    "  foreach ($item in $items) {",
    "    try {",
    "      $sheet = $workbook.Worksheets.Item($item.Sheet)",
    "      $sheet.Activate()",
    "      $range = $sheet.Cells.Item([int]$item.Row, [int]$item.Col)",
    "      $range.Select()",
    "      $excel.Selection.InsertPictureInCell($item.Path)",
    "      $converted++",
    "    } catch {",
    "      # Keep the openxlsx fallback shape if native Picture in Cell is unavailable.",
    "    }",
    "  }",
    "  if ($targetCount -gt 0 -and $converted -eq $targetCount) {",
    "    $sheetNames = @($items | Select-Object -ExpandProperty Sheet -Unique)",
    "    foreach ($sheetName in $sheetNames) {",
    "      $sheet = $workbook.Worksheets.Item($sheetName)",
    "      for ($i = $sheet.Shapes.Count; $i -ge 1; $i--) {",
    "        try { $sheet.Shapes.Item($i).Delete() } catch {}",
    "      }",
    "    }",
    "  }",
    "  $workbook.Save()",
    "  Write-Output $converted",
    "} finally {",
    "  if ($workbook -ne $null) { $workbook.Close($true) | Out-Null }",
    "  if ($excel -ne $null) { $excel.Quit() | Out-Null }",
    "  [GC]::Collect()",
    "  [GC]::WaitForPendingFinalizers()",
    "}"
  ), ps_script)
  
  result <- try(
    system2("powershell", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(ps_script)), stdout = TRUE, stderr = TRUE),
    silent = TRUE
  )
  
  if(inherits(result, "try-error")) {
    warning("Could not convert plots to Excel Picture in Cell. Keeping embedded image fallback.", call. = FALSE)
    return(FALSE)
  }
  
  converted <- suppressWarnings(as.integer(tail(result, 1)))
  isTRUE(!is.na(converted) && converted == nrow(image_map))
}

write_master_workbook <- function(df, df_flighting, df_summary, file) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Flighting")
  openxlsx::addWorksheet(wb, "Summary Table")
  
  openxlsx::writeData(wb, "Flighting", df_flighting)
  openxlsx::writeData(wb, "Summary Table", df_summary)
  
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")
  merged_center_style <- openxlsx::createStyle(halign = "center", valign = "center", wrapText = TRUE)
  low_datapoints_style <- openxlsx::createStyle(fontColour = "#C0392B", fgFill = "#FADBD8", textDecoration = "bold")
  spend_number_style <- openxlsx::createStyle(numFmt = "#,##0.00")
  
  openxlsx::addStyle(wb, "Flighting", header_style, rows = 1, cols = seq_along(df_flighting), gridExpand = TRUE)
  openxlsx::addStyle(wb, "Summary Table", header_style, rows = 1, cols = seq_along(df_summary), gridExpand = TRUE)
  
  datapoints_col <- match("Total Datapoints", names(df_summary))
  spend_col <- match("Total Spend", names(df_summary))
  plot_col <- match("Plot", names(df_summary))
  notes_col <- match("Notes", names(df_summary))
  merge_cols <- stats::na.omit(c(datapoints_col, spend_col, plot_col, notes_col))
  variable_blocks <- split(seq_len(nrow(df_summary)), df_summary$VariableName)
  
  if(length(merge_cols) > 0) {
    for(block_rows in variable_blocks) {
      excel_rows <- block_rows + 1
      if(length(excel_rows) > 1) {
        for(col_idx in merge_cols) {
          openxlsx::mergeCells(wb, "Summary Table", cols = col_idx, rows = range(excel_rows))
        }
      }
      openxlsx::addStyle(wb, "Summary Table", merged_center_style, rows = excel_rows, cols = merge_cols, gridExpand = TRUE, stack = TRUE)
    }
  }

  if(!is.na(spend_col)) {
    openxlsx::addStyle(wb, "Summary Table", spend_number_style,
                       rows = seq_len(nrow(df_summary)) + 1, cols = spend_col,
                       gridExpand = TRUE, stack = TRUE)
  }
  
  if(!is.na(datapoints_col)) {
    low_blocks <- variable_blocks[vapply(variable_blocks, function(block_rows) {
      first_value <- df_summary$`Total Datapoints`[block_rows[1]]
      !is.na(first_value) && first_value < 100
    }, logical(1))]
    
    for(block_rows in low_blocks) {
      openxlsx::addStyle(wb, "Summary Table", low_datapoints_style, rows = block_rows + 1, cols = datapoints_col, gridExpand = TRUE, stack = TRUE)
    }
  }
  
  image_map <- NULL
  if(!is.na(plot_col)) {
    plot_dir <- file.path(tempdir(), paste0("embedded_plots_", as.integer(Sys.time())))
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    image_map <- vector("list", length(variable_blocks))
    
    openxlsx::setColWidths(wb, "Summary Table", cols = plot_col, widths = 48)
    
    for(block_idx in seq_along(variable_blocks)) {
      block_rows <- variable_blocks[[block_idx]]
      first_row <- block_rows[1] + 1
      years_in_block <- length(block_rows)
      var_name <- df_summary$VariableName[block_rows[1]]
      plot_file <- file.path(plot_dir, safe_plot_filename(var_name))
      
      ggplot2::ggsave(plot_file, build_trend_plot(df, var_name), width = 4.4, height = 1.35, dpi = 150)
      openxlsx::setRowHeights(wb, "Summary Table", rows = block_rows + 1, heights = max(30, 112 / years_in_block))
      openxlsx::insertImage(
        wb,
        "Summary Table",
        file = plot_file,
        startRow = first_row,
        startCol = plot_col,
        width = 4.4,
        height = 1.35,
        units = "in"
      )
      
      image_map[[block_idx]] <- data.frame(
        Sheet = "Summary Table",
        Row = first_row,
        Col = plot_col,
        Path = normalizePath(plot_file, winslash = "\\", mustWork = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  
  openxlsx::setColWidths(wb, "Flighting", cols = seq_along(df_flighting), widths = "auto")
  summary_cols <- setdiff(seq_along(df_summary), plot_col)
  openxlsx::setColWidths(wb, "Summary Table", cols = summary_cols, widths = "auto")
  if(!is.na(plot_col)) openxlsx::setColWidths(wb, "Summary Table", cols = plot_col, widths = 48)
  openxlsx::freezePane(wb, "Flighting", firstRow = TRUE)
  openxlsx::freezePane(wb, "Summary Table", firstRow = TRUE)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  
  if(!is.null(image_map)) {
    place_excel_pictures_in_cells(file, dplyr::bind_rows(image_map))
  }
}
