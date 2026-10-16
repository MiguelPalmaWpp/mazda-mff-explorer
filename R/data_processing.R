find_date_column <- function(column_names) {
  exact <- which(tolower(trimws(column_names)) %in% c("date", "fecha"))
  if(length(exact) > 0) return(column_names[exact[1]])

  candidates <- grep("date|fecha|period|time", column_names, ignore.case = TRUE)
  if(length(candidates) > 0) column_names[candidates[1]] else NULL
}

load_uploaded_data <- function(file_info) {
  extension <- tolower(tools::file_ext(file_info$name))
  path <- file_info$datapath

  if(extension == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE, guess_max = 10000))
  }

  if(extension %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    for(sheet in sheets) {
      candidate <- readxl::read_excel(path, sheet = sheet, guess_max = 10000)
      date_column <- find_date_column(names(candidate))
      numeric_columns <- setdiff(
        names(candidate)[vapply(candidate, is.numeric, logical(1))],
        date_column
      )
      if(!is.null(date_column) && length(numeric_columns) > 0) {
        attr(candidate, "source_sheet") <- sheet
        return(candidate)
      }
    }
    stop("No Excel sheet contains a Date column and numeric variables.")
  }

  if(extension == "rdata") {
    environment <- new.env()
    load(path, envir = environment)
    objects <- mget(ls(envir = environment), envir = environment)
    tables <- Filter(is.data.frame, objects)
    if(length(tables) == 0) {
      stop("The .RData file does not contain a data frame.")
    }
    dated <- Filter(function(table) !is.null(find_date_column(names(table))), tables)
    if(length(dated) == 0) {
      stop("No data frame in the .RData file contains a Date column.")
    }
    return(dated[[1]])
  }

  stop("Unsupported file type. Upload a .csv, .xlsx, .xls or .RData file.")
}

parse_mff_dates <- function(values) {
  if(inherits(values, "Date")) return(values)
  if(inherits(values, c("POSIXct", "POSIXlt"))) return(as.Date(values))
  if(is.numeric(values)) return(as.Date(values, origin = "1899-12-30"))

  parsed <- lubridate::parse_date_time(
    as.character(values),
    orders = c("ymd", "ymd HMS", "dmy", "dmy HMS", "mdy", "mdy HMS"),
    quiet = TRUE
  )
  as.Date(parsed)
}

repair_mff_dates <- function(df, date_column, parsed_dates) {
  required <- c("trend_time", "day_of_week", "month")
  if(!all(required %in% names(df))) return(NULL)

  time <- suppressWarnings(as.numeric(df$trend_time))
  if(anyNA(time) || any(time != floor(time)) || any(time < 1) ||
     anyNA(parsed_dates[1])) return(NULL)

  # Mazda MFF exports sometimes flatten Date to the first of each month, or
  # write every year as 01. Their daily trend index and calendar labels retain
  # enough information to recover the original dates without changing values.
  raw <- as.character(df[[date_column]])
  first_month <- as.integer(format(parsed_dates[1], "%m"))
  first_day <- as.integer(format(parsed_dates[1], "%d"))
  first_year <- as.integer(format(parsed_dates[1], "%Y"))
  has_full_year <- grepl("(^|[^0-9])[12][0-9]{3}([^0-9]|$)", raw[1])
  years <- if(has_full_year) first_year else seq.int(2000L, as.integer(format(Sys.Date(), "%Y")))
  weekday_labels <- tolower(trimws(as.character(df$day_of_week)))
  month_labels <- tolower(trimws(as.character(df$month)))
  if(anyNA(weekday_labels) || anyNA(month_labels)) return(NULL)
  expected_weekdays <- match(weekday_labels,
                             c("sunday", "monday", "tuesday", "wednesday",
                               "thursday", "friday", "saturday")) - 1L
  expected_months <- match(month_labels, tolower(month.name))
  if(anyNA(expected_weekdays) || anyNA(expected_months)) return(NULL)

  for(year in rev(years)) {
    start <- as.Date(sprintf("%04d-%02d-%02d", year, first_month, first_day))
    if(is.na(start)) next
    dates <- start + (time - time[1])
    calendar <- as.POSIXlt(dates)
    if(any(calendar$wday != expected_weekdays) ||
       any(calendar$mon + 1L != expected_months)) next

    # A day-level export must also agree with its stated day of month.
    raw_days <- suppressWarnings(as.integer(sub("^([0-9]{1,2})[-/].*$", "\\1", raw)))
    if(!has_full_year && all(!is.na(raw_days)) &&
       any(raw_days != as.integer(format(dates, "%d")))) next
    return(dates)
  }
  NULL
}

process_data_and_dates <- function(df) {
  if(!is.data.frame(df) || nrow(df) == 0) {
    stop("The uploaded file must contain a non-empty table.")
  }

  date_column <- find_date_column(names(df))
  if(is.null(date_column)) {
    stop("No Date column found. Expected Date, Fecha, Period or Time.")
  }

  dates <- parse_mff_dates(df[[date_column]])
  invalid_dates <- sum(is.na(dates))
  if(invalid_dates > 0) {
    stop("The Date column contains ", invalid_dates, " missing or invalid values.")
  }
  date_note <- NULL
  weekday_column <- if("day_of_week" %in% names(df)) tolower(trimws(as.character(df$day_of_week))) else NULL
  parsed_weekdays <- c("sunday", "monday", "tuesday", "wednesday",
                       "thursday", "friday", "saturday")[as.POSIXlt(dates)$wday + 1L]
  weekday_mismatch <- !is.null(weekday_column) &&
    any(parsed_weekdays != weekday_column)
  if(anyDuplicated(dates) || weekday_mismatch) {
    repaired <- repair_mff_dates(df, date_column, dates)
    if(!is.null(repaired)) {
      dates <- repaired
      date_note <- paste0(
        "Daily dates were recovered from trend_time, day_of_week and month (",
        format(min(dates), "%Y-%m-%d"), " to ",
        format(max(dates), "%Y-%m-%d"), ")."
      )
    }
  }

  # An unnamed CSV index is generated by the exporter and is not a metric.
  index_columns <- grep("^\\.\\.\\.[0-9]+$", names(df), value = TRUE)
  for(column in index_columns) {
    if(is.numeric(df[[column]]) && identical(as.numeric(df[[column]]), as.numeric(seq_len(nrow(df))))) {
      df[[column]] <- NULL
    }
  }

  # Preserve every distinct observation; remove only exact repeated records.
  duplicate_rows <- duplicated(df)
  duplicate_row_count <- sum(duplicate_rows)
  if(duplicate_row_count > 0) {
    df <- df[!duplicate_rows, , drop = FALSE]
    dates <- dates[!duplicate_rows]
  }
  duplicate_dates <- unique(dates[duplicated(dates)])
  if(length(duplicate_dates) > 0) {
    stop("The Date column contains conflicting records for ",
         format(duplicate_dates[1], "%Y-%m-%d"), ". Check the source MFF.")
  }
  if(weekday_mismatch && is.null(date_note)) {
    stop("Date does not match day_of_week. Check the source MFF calendar fields.")
  }

  sheet <- attr(df, "source_sheet")
  if(date_column != "Date") df[[date_column]] <- NULL
  df$Date <- dates
  df <- df[, c("Date", setdiff(names(df), "Date")), drop = FALSE]

  numeric_columns <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], "Date")
  if(length(numeric_columns) == 0) {
    stop("The file contains no numeric columns to plot.")
  }

  nonfinite_count <- 0L
  for(column in numeric_columns) {
    bad <- !is.finite(df[[column]]) & !is.na(df[[column]])
    nonfinite_count <- nonfinite_count + sum(bad)
    df[[column]][bad] <- NA_real_
  }

  list(
    data = df,
    col_detected = date_column,
    sheet = sheet,
    numeric_columns = numeric_columns,
    nonfinite_count = nonfinite_count,
    date_note = date_note,
    duplicate_row_count = duplicate_row_count
  )
}
