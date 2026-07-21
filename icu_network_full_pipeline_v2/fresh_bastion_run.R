## One-command, memory-safe fresh run for the R 4.1 bastion host.

if (!exists("data", envir = .GlobalEnv, inherits = FALSE) ||
    !is.data.frame(get("data", envir = .GlobalEnv))) {
  stop("Load the source Excel file into an R object named `data` before starting.")
}

stable_commit <- "682f8d34978544a8a852855e9f66990996964dac"
code_root <- path.expand("~/icu_pipeline_fresh_code_682f8d3")
zip_file <- tempfile(pattern = "icu_pipeline_", fileext = ".zip")
dir.create(code_root, recursive = TRUE, showWarnings = FALSE)
message("Downloading the memory-safe full pipeline...")
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

result_dir <- run_icu_network_pipeline(
  data = data,
  output_dir = "~/ICU_network_REAL_RUN_SAFE_V2",
  mode = "real",
  cores = 3L,
  bootstrap_cores = 2L,
  release_input_memory = TRUE
)
cat("\nFULL PIPELINE COMPLETE\nResult directory:\n", result_dir, "\n")
