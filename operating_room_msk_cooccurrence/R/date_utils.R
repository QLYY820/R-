extract_submission_year <- function(x) {
  text <- trimws(as.character(x))
  text[is.na(text) | text == ""] <- NA_character_
  year <- rep(NA_integer_, length(text))

  # Handles ISO, slash-separated, Chinese-labelled, and month-first strings
  # without relying on the locale-sensitive as.POSIXct.character dispatcher.
  year_position <- regexpr("(19|20)[0-9]{2}", text, perl = TRUE)
  matched <- !is.na(text) & year_position > 0L
  year[matched] <- suppressWarnings(as.integer(substr(
    text[matched], year_position[matched], year_position[matched] + 3L
  )))

  # All-text XLSX imports can expose Excel serial dates or Unix timestamps.
  numeric_value <- suppressWarnings(as.numeric(text))
  excel_serial <- is.na(year) & !is.na(numeric_value) & numeric_value >= 20000 & numeric_value <= 80000
  if (any(excel_serial)) {
    excel_date <- as.Date(numeric_value[excel_serial], origin = "1899-12-30")
    year[excel_serial] <- suppressWarnings(as.integer(format(excel_date, "%Y")))
  }
  unix_seconds <- is.na(year) & !is.na(numeric_value) & numeric_value >= 946684800 & numeric_value <= 4102444800
  if (any(unix_seconds)) {
    unix_time <- as.POSIXct(numeric_value[unix_seconds], origin = "1970-01-01", tz = "UTC")
    year[unix_seconds] <- suppressWarnings(as.integer(format(unix_time, "%Y")))
  }
  unix_milliseconds <- is.na(year) & !is.na(numeric_value) & numeric_value >= 946684800000 & numeric_value <= 4102444800000
  if (any(unix_milliseconds)) {
    unix_time <- as.POSIXct(numeric_value[unix_milliseconds] / 1000, origin = "1970-01-01", tz = "UTC")
    year[unix_milliseconds] <- suppressWarnings(as.integer(format(unix_time, "%Y")))
  }
  year
}
