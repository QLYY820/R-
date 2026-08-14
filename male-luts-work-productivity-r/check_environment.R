source("config/config.R", encoding = "UTF-8")
required <- c("data.table", "digest", "openxlsx", "poLCA", "psych",
              "sandwich", "lmtest", "mclust", "ggplot2", "jsonlite",
              "car", "officer", "flextable", "readxl")

status <- data.frame(
  package = required,
  installed = vapply(required, requireNamespace, logical(1), quietly = TRUE),
  version = vapply(required, function(x) {
    if (requireNamespace(x, quietly = TRUE)) as.character(utils::packageVersion(x)) else NA_character_
  }, character(1))
)
print(status, row.names = FALSE)
cat("\nR version:", R.version.string, "\n")
cat("Expected source records:", format(CFG$expected_source_records, big.mark = ","), "\n")
cat("RStudio data object available:", exists(CFG$data_object_name, envir = .GlobalEnv, inherits = FALSE), "\n")
cat("Configured data file exists:", file.exists(CFG$data_file), "\n")
if (any(!status$installed)) stop("Install missing packages with source('install_packages.R').")
