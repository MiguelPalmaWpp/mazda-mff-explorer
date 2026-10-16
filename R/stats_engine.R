detect_outliers <- function(valid_values) {
  method_used <- "IQR"
  outliers <- valid_values[0, , drop = FALSE]
  upper_limit <- Inf
  
  if(nrow(valid_values) > 10 && stats::sd(valid_values$Value, na.rm = TRUE) > 0) {
    time_cor <- stats::cor(as.numeric(valid_values$Date), valid_values$Value)
    
    if(!is.na(time_cor) && abs(time_cor) > 0.6) {
      method_used <- "Residuals"
      model <- stats::lm(Value ~ Date, data = valid_values)
      resids <- stats::residuals(model)
      
      q1_r <- stats::quantile(resids, 0.25, na.rm = TRUE)
      q3_r <- stats::quantile(resids, 0.75, na.rm = TRUE)
      iqr_r <- q3_r - q1_r
      outlier_indices <- which(resids > (q3_r + 1.5 * iqr_r))
      outliers <- valid_values[outlier_indices, , drop = FALSE]
    } else {
      q1 <- stats::quantile(valid_values$Value, 0.25, na.rm = TRUE)
      q3 <- stats::quantile(valid_values$Value, 0.75, na.rm = TRUE)
      iqr_val <- q3 - q1
      upper_limit <- q3 + 1.5 * iqr_val
      outliers <- dplyr::filter(valid_values, .data$Value > upper_limit)
    }
  }
  
  list(method = method_used, upper_limit = upper_limit, outliers = outliers)
}

classify_data_behavior <- function(values) {
  runs <- rle(values)
  valid_lengths <- runs$lengths[runs$values != 0 & !is.na(runs$values)]
  max_run <- if(length(valid_lengths) > 0) max(valid_lengths) else 0
  
  if(max_run >= 5) "Constant blocks detected" else "High Variance / No blocks"
}

format_top_outliers <- function(outliers, max_items = 3) {
  if(nrow(outliers) == 0) return("No upper outliers detected")
  
  top_outliers <- outliers %>%
    dplyr::arrange(dplyr::desc(.data$Value)) %>%
    dplyr::slice_head(n = max_items) %>%
    dplyr::mutate(
      label = paste0(
        format(.data$Date, "%Y-%m-%d"),
        " (",
        format(round(.data$Value, 2), big.mark = ",", trim = TRUE),
        ")"
      )
    )
  
  paste0("Top outliers: ", paste(top_outliers$label, collapse = ", "))
}

calculate_advanced_stats <- function(df_var, var_name, include_global_details = FALSE) {
  valid_values <- dplyr::filter(df_var, .data$Value > 0 & !is.na(.data$Value))
  outlier_info <- detect_outliers(valid_values)
  total_datapoints <- nrow(valid_values)
  global_behavior <- classify_data_behavior(df_var$Value)
  global_notes <- paste(format_top_outliers(outlier_info$outliers), "Behavior;", global_behavior, sep = "; ")
  
  stats_summary <- df_var %>%
    dplyr::mutate(Year = lubridate::year(.data$Date)) %>%
    dplyr::group_by(.data$Year) %>%
    dplyr::summarise(
      Total_Rows = dplyr::n(),
      `Periods with Data` = sum(!is.na(.data$Value) & .data$Value != 0),
      `Data Completeness` = paste0(round((`Periods with Data` / .data$Total_Rows) * 100, 1), "%"),
      Average = round(mean(.data$Value, na.rm = TRUE), 2),
      `Outlier Method` = outlier_info$method,
      `Upper Outliers` = if(outlier_info$method == "Residuals") {
        sum(.data$Date %in% outlier_info$outliers$Date)
      } else {
        sum(.data$Value > outlier_info$upper_limit & .data$Value > 0, na.rm = TRUE)
      },
      `Data Behavior` = classify_data_behavior(.data$Value),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$Year) %>%
    dplyr::select(
      "Year",
      "Periods with Data",
      "Data Completeness",
      "Average",
      "Outlier Method",
      "Upper Outliers",
      "Data Behavior"
    )
  
  if(include_global_details) {
    stats_summary <- stats_summary %>%
      dplyr::mutate(
        `Total Datapoints` = dplyr::if_else(dplyr::row_number() == 1, as.integer(total_datapoints), NA_integer_),
        Plot = dplyr::if_else(dplyr::row_number() == 1, "", NA_character_),
        Notes = dplyr::if_else(dplyr::row_number() == 1, global_notes, NA_character_)
      )
  }
  
  stats_summary
}

summarise_variable <- function(df, var_name, include_global_details = TRUE) {
  stats <- calculate_advanced_stats(
    dplyr::select(df, Date, Value = dplyr::all_of(var_name)),
    var_name,
    include_global_details = include_global_details
  )
  stats$VariableName <- var_name
  stats
}

summary_by_variable <- function(df, var_name, include_global_details = TRUE) {
  summarise_variable(df, var_name, include_global_details = include_global_details) %>%
    dplyr::select(-"VariableName")
}

build_flighting_table <- function(df, cols_num) {
  df %>%
    dplyr::select("Date", dplyr::all_of(cols_num)) %>%
    tidyr::pivot_longer(cols = -Date, names_to = "VariableName", values_to = "VariableValue") %>%
    dplyr::rename(Period = Date) %>%
    dplyr::arrange(.data$VariableName, .data$Period)
}

build_summary_table <- function(df, cols_num) {
  cols_num %>%
    purrr::set_names() %>%
    purrr::map_dfr(~ summarise_variable(df, .x, include_global_details = TRUE)) %>%
    dplyr::select("VariableName", dplyr::everything())
}
