## Reproducible package bootstrap for the ICU network pipeline.

.icu_installer_file <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  valid_source <- is.character(source_file) && length(source_file) == 1L &&
    !is.na(source_file) && nzchar(source_file)
  if (!valid_source) {
    normalizePath("install_packages.R", mustWork = TRUE)
  } else {
    normalizePath(source_file, mustWork = TRUE)
  }
})

r_version <- getRversion()
legacy_r <- r_version < "4.2.0"
worker_mode <- identical(Sys.getenv("ICU_PACKAGE_INSTALL_WORKER"), "1")
cran_repo <- if (legacy_r) {
  "https://packagemanager.posit.co/cran/2022-02-01"
} else {
  "https://cloud.r-project.org"
}

required_packages <- c(
  "data.table", "MASS", "Matrix", "psych", "ggplot2", "MatchIt", "cobalt",
  "sandwich", "lmtest", "patchwork", "openssl",
  "qgraph", "IsingFit", "IsingSampler", "NetworkComparisonTest", "smacof",
  "networktools", "mgm", "NetworkToolbox", "bootnet"
)

locate_package <- function(package) {
  path <- suppressWarnings(find.package(package, lib.loc = .libPaths(), quiet = TRUE))
  if (!length(path)) "" else path[[1L]]
}

install_stack <- function() {
  options(
    repos = c(CRAN = cran_repo),
    timeout = max(1200L, getOption("timeout", 60L))
  )

  user_library <- Sys.getenv("R_LIBS_USER", unset = .libPaths()[1])
  dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_library, .Library))

  message("R version: ", r_version)
  message("Package repository: ", cran_repo)
  message("Pipeline package library: ", user_library)
  message("Active package libraries: ", paste(.libPaths(), collapse = " | "))

  stale_locks <- list.files(user_library, pattern = "^00LOCK", full.names = TRUE)
  if (length(stale_locks)) {
    message("Removing stale package-install locks: ", paste(basename(stale_locks), collapse = ", "))
    unlink(stale_locks, recursive = TRUE, force = TRUE)
  }

  install_one <- function(package) {
    package_path <- locate_package(package)
    if (nzchar(package_path)) {
      version <- utils::packageDescription(package, lib.loc = dirname(package_path))$Version
      message("Available: ", package, " ", version)
      return(invisible(TRUE))
    }

    message("Installing: ", package)
    utils::install.packages(
      package,
      lib = user_library,
      repos = cran_repo,
      dependencies = c("Depends", "Imports", "LinkingTo"),
      Ncpus = max(1L, min(4L, parallel::detectCores() - 1L))
    )

    package_path <- locate_package(package)
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

  load_checks <- vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
  if (!all(load_checks)) {
    stop("Namespaces not loadable: ", paste(names(load_checks)[!load_checks], collapse = ", "))
  }

  versions <- vapply(
    required_packages,
    function(package) as.character(utils::packageVersion(package)),
    FUN.VALUE = character(1)
  )
  message("CLEAN_SESSION_PACKAGE_LOAD_OK")
  print(data.frame(package = required_packages, version = unname(versions)))
  invisible(TRUE)
}

if (legacy_r && !worker_mode) {
  ## The clean child process excludes packages previously installed in the
  ## user's normal library and in the host site-library. The same environment
  ## is inherited by the analysis Rscript and all PSOCK workers.
  private_library <- path.expand("~/R/icu-network-r4.1-snapshot-2022-02-01")
  empty_site_library <- file.path(private_library, "empty-site-library")
  dir.create(private_library, recursive = TRUE, showWarnings = FALSE)
  dir.create(empty_site_library, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_LIBS_USER = private_library, R_LIBS_SITE = empty_site_library)

  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  worker_expression <- paste0("source(", deparse(.icu_installer_file), ")")
  message("Launching isolated R 4.1 package installer...")
  status <- system2(
    rscript,
    args = c("-e", shQuote(worker_expression)),
    env = c(
      "ICU_PACKAGE_INSTALL_WORKER=1",
      paste0("R_LIBS_USER=", private_library),
      paste0("R_LIBS_SITE=", empty_site_library)
    )
  )
  if (!identical(status, 0L)) {
    stop("Isolated package installation returned nonzero status ", status, ".")
  }

  .libPaths(c(private_library, .Library))
  message("Package installation completed in the isolated R 4.1 library.")
} else {
  install_stack()
}
