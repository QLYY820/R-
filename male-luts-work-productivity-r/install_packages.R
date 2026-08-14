cran_packages <- c(
  "data.table", "digest", "openxlsx", "poLCA", "psych", "sandwich",
  "lmtest", "mclust", "ggplot2", "jsonlite", "car", "officer",
  "flextable", "readxl", "renv"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))
missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, dependencies = TRUE)

if (file.exists("renv.lock") && requireNamespace("renv", quietly = TRUE)) {
  message("renv.lock detected. Run renv::restore() if exact package versions are required.")
}
message("Required R packages are available.")
