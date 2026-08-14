install_mode <- tolower(Sys.getenv("LUTS_INSTALL_MODE", unset = "preflight"))
if (!install_mode %in% c("preflight", "production")) {
  stop("LUTS_INSTALL_MODE must be 'preflight' or 'production'.", call. = FALSE)
}

preflight_packages <- c("data.table", "digest", "openxlsx", "psych")
analysis_packages <- c("poLCA", "sandwich", "lmtest", "mclust", "ggplot2", "jsonlite")
document_packages <- c("officer", "flextable")

generate_manuscript <- TRUE
if (file.exists("config/config.R")) {
  config_env <- new.env(parent = baseenv())
  sys.source("config/config.R", envir = config_env)
  if (exists("CFG", envir = config_env, inherits = FALSE)) {
    generate_manuscript <- isTRUE(config_env$CFG$generate_manuscript)
  }
}

cran_packages <- preflight_packages
if (install_mode == "production") {
  cran_packages <- c(cran_packages, analysis_packages)
  if (generate_manuscript) cran_packages <- c(cran_packages, document_packages)
}
cran_packages <- unique(cran_packages)

options(repos = c(CRAN = "https://cloud.r-project.org"))
missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing ", install_mode, " packages: ", paste(missing, collapse = ", "))
  install.packages(
    missing,
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

still_missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) {
  stop(
    "Package installation is incomplete: ", paste(still_missing, collapse = ", "),
    ". Send the complete installation error to the analysis team.",
    call. = FALSE
  )
}

if (getRversion() < "4.2.0") {
  warning(
    "This server uses ", R.version.string,
    ". Do not run renv::restore() from the R 4.5 lockfile; use this staged installer.",
    call. = FALSE
  )
}
message("All ", install_mode, " packages are available.")
