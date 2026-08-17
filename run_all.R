args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)

if (length(file_arg)) {
  script_file <- normalizePath(
    sub("^--file=", "", file_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  PIPELINE_ROOT <- dirname(script_file)
} else {
  PIPELINE_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(PIPELINE_ROOT, "config.R"), encoding = "UTF-8")

if (!file.exists(CONFIG$data_path)) {
  message("Cached text RDS does not exist; preparing it from canonical XLSX.")
  source(file.path(PIPELINE_ROOT, "prepare_data.R"), encoding = "UTF-8")
}

DATA_PATH <- normalizePath(
  CONFIG$data_path,
  winslash = "/",
  mustWork = TRUE
)

run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
run_id <- Sys.getenv("MALE_SLEEP_RUN_ID", unset = paste0("run_", run_stamp))
RUN_ROOT <- file.path(PIPELINE_ROOT, CONFIG$output_base, run_id)

output_dirs <- c(
  "00_audit", "01_descriptive", "02_regression", "03_spline",
  "04_sensitivity", "05_tables", "06_figures", "07_report",
  "08_logs", "_work"
)
for (d in output_dirs) {
  dir.create(file.path(RUN_ROOT, d), recursive = TRUE, showWarnings = FALSE)
}

latest_pointer <- file.path(PIPELINE_ROOT, CONFIG$output_base, "LATEST_RUN.txt")
dir.create(dirname(latest_pointer), recursive = TRUE, showWarnings = FALSE)
writeLines(normalizePath(RUN_ROOT, winslash = "/"), latest_pointer, useBytes = TRUE)

source(
  file.path(PIPELINE_ROOT, "R", "analysis.R"),
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path(PIPELINE_ROOT, "package_results.R"),
  encoding = "UTF-8",
  chdir = FALSE
)

message("PIPELINE_SUCCESS")
message("RUN_ROOT=", normalizePath(RUN_ROOT, winslash = "/"))
message("TRANSFER_ZIP=", normalizePath(TRANSFER_ZIP, winslash = "/"))
