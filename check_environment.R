source("config.R", encoding = "UTF-8")

required <- c(
  "data.table", "psych", "sandwich", "lmtest", "ggplot2",
  "openxlsx", "mice", "readxl", "zip"
)
status <- data.frame(
  package = required,
  installed = vapply(required, requireNamespace, logical(1), quietly = TRUE),
  version = vapply(required, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) as.character(packageVersion(pkg)) else NA_character_
  }, character(1)),
  stringsAsFactors = FALSE
)
print(status, row.names = FALSE)
cat("\nR version:", R.version.string, "\n")
cat("Canonical XLSX:", CONFIG$canonical_xlsx, "exists=", file.exists(CONFIG$canonical_xlsx), "\n")
cat("Cached text RDS:", CONFIG$data_path, "exists=", file.exists(CONFIG$data_path), "\n")
if (!all(status$installed)) stop("Required packages are missing; run install_packages.R.")
if (!file.exists(CONFIG$canonical_xlsx) && !file.exists(CONFIG$data_path)) {
  stop("Neither canonical XLSX nor cached text RDS is available.")
}
message("ENVIRONMENT_CHECK_PASSED")
