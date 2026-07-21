## Reproducible package bootstrap for the bastion host.
## R 4.1.x cannot resolve the current CRAN dependency chain for several
## network-analysis packages, so it uses a compatible frozen repository.

r_version <- getRversion()
legacy_r <- r_version < "4.2.0"
cran_repo <- if (legacy_r) {
  "https://packagemanager.posit.co/cran/2022-02-01"
} else {
  "https://cloud.r-project.org"
}

options(
  repos = c(CRAN = cran_repo),
  timeout = max(1200L, getOption("timeout", 60L))
)

message("R version: ", r_version)
message("Package repository: ", cran_repo)

## Keep the frozen R 4.1 dependency stack isolated from packages previously
## installed from current CRAN. R_LIBS_USER is inherited by every Rscript and
## PSOCK worker launched by the pipeline.
if (legacy_r) {
  user_library <- path.expand("~/R/icu-network-r4.1-snapshot-2022-02-01")
  dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_LIBS_USER = user_library)
  .libPaths(c(user_library, .Library))
} else {
  user_library <- .libPaths()[1]
}

dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
message("Pipeline package library: ", user_library)
message("Active package libraries: ", paste(.libPaths(), collapse = " | "))
stale_locks <- list.files(user_library, pattern = "^00LOCK", full.names = TRUE)
if (length(stale_locks)) {
  message("Removing stale package-install locks: ", paste(basename(stale_locks), collapse = ", "))
  unlink(stale_locks, recursive = TRUE, force = TRUE)
}

## The order is intentional. It resolves the qgraph/NCT/networktools/mgm/bootnet
## dependency chain before packages that import the complete stack.
required_packages <- c(
  "data.table", "MASS", "Matrix", "psych", "ggplot2", "MatchIt", "cobalt",
  "sandwich", "lmtest", "patchwork", "openssl",
  "qgraph", "IsingFit", "IsingSampler", "NetworkComparisonTest", "smacof",
  "networktools", "mgm", "NetworkToolbox", "bootnet"
)

install_one <- function(package) {
  package_path <- suppressWarnings(find.package(package, lib.loc = .libPaths(), quiet = TRUE))
  if (nzchar(package_path)) {
    version <- utils::packageDescription(package, lib.loc = dirname(package_path))$Version
    message("Available: ", package, " ", version)
    return(invisible(TRUE))
  }

  message("Installing: ", package)
  utils::install.packages(
    package,
    repos = cran_repo,
    dependencies = c("Depends", "Imports", "LinkingTo"),
    Ncpus = max(1L, min(4L, parallel::detectCores() - 1L))
  )

  package_path <- suppressWarnings(find.package(package, lib.loc = .libPaths(), quiet = TRUE))
  if (!nzchar(package_path)) {
    stop(
      "Package installation failed: ", package,
      ". Review the installation messages immediately above this line."
    )
  }
  version <- utils::packageDescription(package, lib.loc = dirname(package_path))$Version
  message("Installed: ", package, " ", version)
  invisible(TRUE)
}

for (package in required_packages) install_one(package)

package_paths <- vapply(
  required_packages,
  function(package) suppressWarnings(find.package(package, lib.loc = .libPaths(), quiet = TRUE)),
  FUN.VALUE = character(1)
)
if (any(!nzchar(package_paths))) {
  stop("Packages still missing from active libraries: ", paste(names(package_paths)[!nzchar(package_paths)], collapse = ", "))
}

installed_versions <- vapply(
  seq_along(required_packages),
  function(i) utils::packageDescription(required_packages[[i]], lib.loc = dirname(package_paths[[i]]))$Version,
  FUN.VALUE = character(1)
)

## Validate namespace loading in a clean child process. This catches mixed
## dependency stacks before the several-hour analysis starts.
validation_script <- tempfile(pattern = "icu_package_validation_", fileext = ".R")
on.exit(unlink(validation_script, force = TRUE), add = TRUE)
writeLines(
  c(
    paste0("packages <- ", paste(deparse(required_packages), collapse = "")),
    "ok <- vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))",
    "if (!all(ok)) stop('Namespaces not loadable: ', paste(names(ok)[!ok], collapse = ', '))",
    "cat('CLEAN_SESSION_PACKAGE_LOAD_OK\\n')"
  ),
  validation_script
)
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
validation_status <- system2(rscript, validation_script)
if (!identical(validation_status, 0L)) {
  stop("Package namespace validation failed in a clean R session.")
}

message("All required R packages are installed and loadable in a clean session.")
print(data.frame(package = required_packages, version = unname(installed_versions)))
