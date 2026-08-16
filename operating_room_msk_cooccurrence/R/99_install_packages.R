options(repos = c(CRAN = "https://cloud.r-project.org"))
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Chinese"), silent = TRUE))
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript R/99_install_packages.R <analysis_config.R>")
source(args[[1L]])
project_root <- normalizePath(file.path(dirname(args[[1L]]), ".."), winslash = "/", mustWork = TRUE)
config <- build_analysis_config(project_root, mode = "formal")
missing <- config$packages[!vapply(config$packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, dependencies = TRUE)
still_missing <- config$packages[!vapply(config$packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(still_missing)) stop("Packages still missing: ", paste(still_missing, collapse = ", "))
cat("All required packages are available.\n")
