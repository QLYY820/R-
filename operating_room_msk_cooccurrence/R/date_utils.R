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

resolve_submission_year <- function(primary, secondary = list(), allowed_years = integer()) {
  primary_year <- extract_submission_year(primary)
  if (length(allowed_years)) {
    primary_year[!is.na(primary_year) & !primary_year %in% allowed_years] <- NA_integer_
  }

  secondary_years <- lapply(secondary, extract_submission_year)
  if (length(allowed_years) && length(secondary_years)) {
    secondary_years <- lapply(secondary_years, function(x) {
      x[!is.na(x) & !x %in% allowed_years] <- NA_integer_
      x
    })
  }

  resolved <- primary_year
  resolution_source <- rep("primary", length(primary_year))
  resolution_source[is.na(primary_year)] <- "unresolved"

  if (length(secondary_years)) {
    secondary_matrix <- do.call(cbind, secondary_years)
    if (is.null(dim(secondary_matrix))) secondary_matrix <- matrix(secondary_matrix, ncol = 1L)
    for (index in which(is.na(primary_year))) {
      available <- unique(secondary_matrix[index, !is.na(secondary_matrix[index, ]), drop = TRUE])
      if (length(available) == 1L) {
        resolved[[index]] <- as.integer(available[[1L]])
        resolution_source[[index]] <- "secondary_consensus"
      } else if (length(available) > 1L) {
        resolution_source[[index]] <- "secondary_conflict"
      }
    }
  }

  list(year = as.integer(resolved), source = resolution_source)
}
