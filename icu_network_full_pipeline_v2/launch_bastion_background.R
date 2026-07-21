## One-command background launcher for a fresh R 4.1 bastion run.

if (.Platform$OS.type != "unix") stop("This background launcher requires the Linux bastion host.")
if (!exists("data", envir = .GlobalEnv, inherits = FALSE) ||
    !is.data.frame(get("data", envir = .GlobalEnv))) {
  stop("Load the source Excel file into an R object named `data` before starting.")
}

stable_commit <- "5b99fdc9fe9650049f8f8a7c014c0e7ed38dfb89"
code_root <- path.expand("~/icu_pipeline_background_code_5b99fdc")
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

if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table is unavailable.")
dictionary <- file.path(project_dir, "config", "source_dictionary.csv")
original_column_count <- ncol(get("data", envir = .GlobalEnv))
input_data <- .icu_select_analysis_input(get("data", envir = .GlobalEnv), dictionary)
selected_source_columns <- names(input_data)
if (!"analysis_id" %in% names(input_data)) {
  input_data[, analysis_id := sprintf("AUTO-%08d", seq_len(.N))]
}
if (!"participant_hash" %in% names(input_data)) {
  input_data[, participant_hash := paste0("ROW-", analysis_id)]
}
if (anyDuplicated(input_data$analysis_id) || anyDuplicated(input_data$participant_hash)) {
  stop("Generated or supplied technical identifiers are not unique.")
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
runtime_dir <- path.expand(file.path("~/icu_network_runtime", timestamp))
output_dir <- path.expand("~/ICU_network_REAL_RUN_BACKGROUND_V2")
if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
  stop("Background output directory already exists and is not empty: ", output_dir)
}
dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
input_file <- file.path(runtime_dir, "raw_input.csv")
selection_file <- file.path(runtime_dir, "INPUT_COLUMN_SELECTION.csv")
data.table::fwrite(
  data.table::data.table(
    source_column_count = original_column_count,
    selected_source_column_count = length(selected_source_columns),
    selected_column = selected_source_columns
  ),
  selection_file
)
message("Writing the durable background input file. This can take several minutes...")
data.table::fwrite(input_data, input_file)
rm(input_data)
rm(list = "data", envir = .GlobalEnv)
invisible(gc())

rscript <- file.path(R.home("bin"), "Rscript")
pipeline <- file.path(project_dir, "R", "final_analysis_pipeline.R")
config <- file.path(project_dir, "config", "analysis_config.yml")
runner_file <- file.path(runtime_dir, "run_pipeline.sh")
pid_file <- file.path(runtime_dir, "pipeline.pid")
status_file <- file.path(runtime_dir, "exit_status.txt")
console_log <- file.path(runtime_dir, "background_console.log")

pipeline_command <- paste(
  shQuote(rscript), shQuote(pipeline),
  "--input", shQuote(input_file),
  "--dictionary", shQuote(dictionary),
  "--config", shQuote(config),
  "--output", shQuote(output_dir),
  "--mode real"
)
shell_lines <- c(
  "#!/bin/sh",
  paste("echo $$ >", shQuote(pid_file)),
  paste("export R_LIBS_USER=", shQuote(Sys.getenv("R_LIBS_USER")), sep = ""),
  paste("export R_LIBS_SITE=", shQuote(Sys.getenv("R_LIBS_SITE")), sep = ""),
  "export ICU_PIPELINE_CORES=3",
  "export ICU_BOOTSTRAP_CORES=2",
  "export ICU_PIPELINE_MAX_CORES=4",
  "export OMP_NUM_THREADS=1",
  "export OPENBLAS_NUM_THREADS=1",
  "export MKL_NUM_THREADS=1",
  "export VECLIB_MAXIMUM_THREADS=1",
  "export NUMEXPR_NUM_THREADS=1",
  pipeline_command,
  "status=$?",
  paste("echo $status >", shQuote(status_file)),
  paste("if [ -d", shQuote(output_dir), "]; then cp", shQuote(selection_file), shQuote(file.path(output_dir, "INPUT_COLUMN_SELECTION.csv")), "; fi"),
  paste("rm -f", shQuote(input_file)),
  "exit $status"
)
writeLines(shell_lines, runner_file)
Sys.chmod(runner_file, mode = "0700")

launch_status <- system2(
  "nohup",
  args = shQuote(runner_file),
  stdout = console_log,
  stderr = console_log,
  wait = FALSE
)
if (!identical(launch_status, 0L)) stop("Failed to launch the background pipeline.")
Sys.sleep(2)

background_job <- list(
  output_dir = output_dir,
  runtime_dir = runtime_dir,
  console_log = console_log,
  pid_file = pid_file,
  status_file = status_file
)
cat(
  "\nBACKGROUND PIPELINE STARTED\n",
  "You may close or log out of RStudio after this message.\n",
  "Output: ", output_dir, "\n",
  "Log: ", console_log, "\n",
  sep = ""
)
