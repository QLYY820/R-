options(repos = c(CRAN = "https://cloud.r-project.org"))
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Chinese"), silent = TRUE))
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript R/99_install_packages.R <analysis_config.R>")
source(args[[1L]])
project_root <- normalizePath(file.path(dirname(args[[1L]]), ".."), winslash = "/", mustWork = TRUE)
config <- build_analysis_config(project_root, mode = "formal")

# R 4.1 resolves an older Hmisc dependency that still imports xfun::attr().
# xfun removed that exported helper in 0.56, so pin the last compatible
# archived release before installing qgraph and its network dependencies.
installed <- utils::installed.packages()
xfun_version <- if ("xfun" %in% rownames(installed)) {
  package_version(installed["xfun", "Version"])
} else {
  package_version("0")
}
if (
  getRversion() < package_version("4.2.0") &&
    (xfun_version == package_version("0") || xfun_version >= package_version("0.56"))
) {
  message("Pinning xfun 0.55 for the R 4.1 network-analysis dependency chain.")
  install.packages(
    "https://cran.r-project.org/src/contrib/Archive/xfun/xfun_0.55.tar.gz",
    repos = NULL,
    type = "source"
  )
  installed <- utils::installed.packages()
  if (!"xfun" %in% rownames(installed) || package_version(installed["xfun", "Version"]) >= package_version("0.56")) {
    stop("Failed to install the R 4.1-compatible xfun 0.55 release.")
  }
}

missing <- config$packages[!vapply(config$packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, dependencies = TRUE)
still_missing <- config$packages[!vapply(config$packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(still_missing)) stop("Packages still missing: ", paste(still_missing, collapse = ", "))
cat("All required packages are available.\n")
