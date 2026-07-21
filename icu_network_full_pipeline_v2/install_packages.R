required_packages <- c(
  "data.table", "MASS", "psych", "qgraph", "networktools", "Matrix",
  "MatchIt", "cobalt", "ggplot2", "sandwich", "lmtest", "bootnet",
  "NetworkComparisonTest", "mgm", "patchwork", "openssl"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org", dependencies = TRUE)
}

still_missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(still_missing)) stop("Packages still missing: ", paste(still_missing, collapse = ", "))
message("All required R packages are available.")
