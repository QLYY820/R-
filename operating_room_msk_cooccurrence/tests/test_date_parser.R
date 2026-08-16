source(file.path("R", "date_utils.R"))

excel_serial <- as.numeric(as.Date("2025-01-01") - as.Date("1899-12-30"))
unix_seconds <- as.numeric(as.POSIXct("2021-08-17 02:00:00", tz = "UTC"))

input <- c(
  "2021/05/07 12:34:56",
  "2022-05-07",
  "05/07/2023",
  "2024 year 08 month",
  as.character(excel_serial),
  as.character(unix_seconds),
  as.character(unix_seconds * 1000),
  "not a date",
  ""
)
expected <- c(2021L, 2022L, 2023L, 2024L, 2025L, 2021L, 2021L, NA_integer_, NA_integer_)
observed <- extract_submission_year(input)

stopifnot(identical(observed, expected))

resolution <- resolve_submission_year(
  primary = c("2021-01-01", NA, NA, NA, "2025-01-01"),
  secondary = list(
    c(NA, "2022-01-01", "2023-01-01", NA, "2024-01-01"),
    c(NA, "2022-06-01", "2024-01-01", NA, "2023-01-01")
  ),
  allowed_years = 2021:2025
)
stopifnot(identical(resolution$year, c(2021L, 2022L, NA_integer_, NA_integer_, 2025L)))
stopifnot(identical(
  resolution$source,
  c("primary", "secondary_consensus", "secondary_conflict", "unresolved", "primary")
))
cat("TEST_DATE_PARSER=PASS\n")
