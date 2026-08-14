# Install the packages required by the three ICN 2027 analyses.
# Run once only if RStudio reports that packages are missing.

required <- c("rms", "sandwich", "lmtest", "car", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
