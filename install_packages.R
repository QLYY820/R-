options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2022-02-01"))

required <- c(
  "data.table", "psych", "GPArotation", "sandwich", "lmtest",
  "ggplot2", "openxlsx", "mice", "readxl", "zip"
)

available <- vapply(required, requireNamespace, logical(1), quietly = TRUE)
if (all(available)) {
  message("All required statistical packages are already available.")
} else {
  missing <- required[!available]
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = c("Depends", "Imports", "LinkingTo"))
}

available_after <- vapply(required, requireNamespace, logical(1), quietly = TRUE)
if (!all(available_after)) {
  stop(
    "Package installation is incomplete: ",
    paste(required[!available_after], collapse = ", ")
  )
}

versions <- vapply(required, function(pkg) as.character(packageVersion(pkg)), character(1))
print(data.frame(package = required, version = versions, row.names = NULL))
message("PACKAGE_INSTALLATION_COMPLETE")
