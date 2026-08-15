source("config/config.R", encoding = "UTF-8")
check_mode <- tolower(Sys.getenv("LUTS_INSTALL_MODE", unset = "preflight"))
if (!check_mode %in% c("preflight", "production")) {
  stop("LUTS_INSTALL_MODE must be 'preflight' or 'production'.", call. = FALSE)
}

preflight_packages <- c("data.table", "digest", "openxlsx", "psych")
analysis_packages <- c("poLCA", "sandwich", "lmtest", "mclust", "ggplot2", "jsonlite")
document_packages <- c("officer", "flextable")
required <- preflight_packages
if (check_mode == "production") {
  required <- c(required, analysis_packages)
  if (isTRUE(CFG$generate_manuscript)) required <- c(required, document_packages)
}

status <- data.frame(
  package = required,
  installed = vapply(required, requireNamespace, logical(1), quietly = TRUE),
  version = vapply(required, function(x) {
    if (requireNamespace(x, quietly = TRUE)) as.character(utils::packageVersion(x)) else NA_character_
  }, character(1))
)
print(status, row.names = FALSE)
cat("\nDependency mode:", check_mode, "\n")
cat("\nR version:", R.version.string, "\n")
cat("Expected source records:", format(CFG$expected_source_records, big.mark = ","), "\n")
cat("RStudio data object available:", exists(CFG$data_object_name, envir = .GlobalEnv, inherits = FALSE), "\n")
cat("Configured data file exists:", file.exists(CFG$data_file), "\n")
if (getRversion() < "4.2.0") {
  warning("R is older than 4.2. Do not run renv::restore(); use install_packages.R.", call. = FALSE)
}
if (any(!status$installed)) {
  stop(
    "Missing ", check_mode, " packages: ",
    paste(status$package[!status$installed], collapse = ", "),
    ". Run source('install_packages.R').",
    call. = FALSE
  )
}
