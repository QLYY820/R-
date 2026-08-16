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
cat("TEST_DATE_PARSER=PASS\n")
