## Reproducible package bootstrap for the bastion host.
## R 4.1.x cannot resolve the current CRAN dependency chain for several
## network-analysis packages, so it uses a compatible frozen repository.

r_version <- getRversion()
cran_repo <- if (r_version < "4.2.0") {
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

user_library <- .libPaths()[1]
dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
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
  if (requireNamespace(package, quietly = TRUE)) {
    message("Available: ", package, " ", as.character(utils::packageVersion(package)))
    return(invisible(TRUE))
  }

  message("Installing: ", package)
  utils::install.packages(
    package,
    repos = cran_repo,
    dependencies = c("Depends", "Imports", "LinkingTo"),
    Ncpus = max(1L, min(4L, parallel::detectCores() - 1L))
  )

  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package installation failed: ", package,
      ". Review the installation messages immediately above this line."
    )
  }
  message("Installed: ", package, " ", as.character(utils::packageVersion(package)))
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
  stop("Packages still not loadable: ", paste(names(load_checks)[!load_checks], collapse = ", "))
}

installed_versions <- vapply(
  required_packages,
  function(package) as.character(utils::packageVersion(package)),
  FUN.VALUE = character(1)
)
message("All required R packages are installed and loadable.")
print(data.frame(package = required_packages, version = unname(installed_versions)))
