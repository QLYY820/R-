## One-command, memory-safe resume for the R 4.1 bastion host.

output_dir <- path.expand("~/ICU_network_REAL_RUN")
stable_commit <- "81dacffbb465f51940c02b60d64398d2347943af"
code_root <- path.expand("~/icu_pipeline_resume_code_81dacff")
zip_file <- tempfile(pattern = "icu_pipeline_", fileext = ".zip")
dir.create(code_root, recursive = TRUE, showWarnings = FALSE)
message("Downloading the memory-safe pipeline code...")
utils::download.file(
  paste0("https://codeload.github.com/QLYY820/R-/zip/", stable_commit),
  destfile = zip_file,
  mode = "wb",
  method = "libcurl",
  quiet = TRUE
)
utils::unzip(zip_file, exdir = code_root, overwrite = TRUE)
unlink(zip_file, force = TRUE)

project_dirs <- list.dirs(code_root, recursive = TRUE, full.names = TRUE)
project_dir <- project_dirs[basename(project_dirs) == "icu_network_full_pipeline_v2"][1L]
if (!length(project_dir) || is.na(project_dir) || !dir.exists(project_dir)) {
  stop("Downloaded ICU pipeline project directory was not found.")
}
setwd(project_dir)

source("install_packages.R")
source("run_from_R_object.R")

preparation_marker <- file.path(output_dir, ".pipeline_state", "01_preparation.PASS")
scored_input <- file.path(output_dir, "01_preparation", "scored_analysis_data.csv")
if (!file.exists(preparation_marker) || !file.exists(scored_input)) {
  stop("The saved preparation checkpoint is missing; memory-safe resume is not available.")
}

if (exists("data", envir = .GlobalEnv, inherits = FALSE)) {
  message("Releasing the raw `data` object from memory; the source Excel file is unchanged.")
  rm(list = "data", envir = .GlobalEnv)
  invisible(gc())
}

result_dir <- resume_icu_network_pipeline(
  output_dir = output_dir,
  mode = "real",
  cores = 3L,
  bootstrap_cores = 2L
)
cat("\nFULL PIPELINE COMPLETE\nResult directory:\n", result_dir, "\n")
