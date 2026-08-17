# Resume an interrupted run after data preparation, LCA, and Ising analysis.
# Creates a new output root and copies completed intermediate artifacts; source
# outputs are never overwritten.

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))
}

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_argument)) stop("resume_from_models.R must be executed with Rscript")
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
config_path <- file.path(project_root, "config", "analysis_config.R")
source(config_path)

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(2L, 3L)) {
  stop("Usage: Rscript resume_from_models.R <source_run_root> <new_output_root> [formal|test]")
}
source_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
target_root <- normalizePath(args[[2L]], winslash = "/", mustWork = FALSE)
mode <- if (length(args) == 3L) args[[3L]] else "formal"
if (!mode %in% c("formal", "test")) stop("Resume mode must be formal or test")
if (file.exists(target_root)) stop("Resume target already exists; refusing to overwrite: ", target_root)

required_source <- c(
  "data/operating_room_analysis_data.rds",
  "data/operating_room_survey_year.csv",
  "data/sample_audit.csv",
  "models/lca_class_assignments.csv",
  "models/lca_fit_statistics.csv",
  "models/lca_class_quality.csv",
  "models/ising_adjacency_matrix.csv",
  "models/ising_centrality.csv",
  "models/ising_stability.csv",
  "runtime/lca_class_labels_runtime.csv"
)
missing_source <- required_source[!file.exists(file.path(source_root, required_source))]
if (length(missing_source)) {
  stop("Source run lacks completed intermediate artifacts: ", paste(missing_source, collapse = ", "))
}

dir.create(target_root, recursive = TRUE, showWarnings = FALSE)
for (directory in c("data", "models", "runtime", "tables", "figures", "logs")) {
  dir.create(file.path(target_root, directory), recursive = TRUE, showWarnings = FALSE)
}

copy_tree <- function(name) {
  source_dir <- file.path(source_root, name)
  files <- list.files(source_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  if (!length(files)) stop("Source intermediate directory is empty: ", source_dir)
  relative <- substring(files, nchar(source_dir) + 2L)
  targets <- file.path(target_root, name, relative)
  invisible(lapply(unique(dirname(targets)), dir.create, recursive = TRUE, showWarnings = FALSE))
  copied <- file.copy(files, targets, overwrite = FALSE)
  if (!all(copied)) stop("Failed to copy one or more ", name, " artifacts")
}
for (name in c("data", "models", "runtime")) copy_tree(name)

config <- build_analysis_config(project_root, mode = mode, output_root = target_root)
set.seed(config$seed)
Sys.setenv(
  OR_MSK_PROJECT_ROOT = project_root,
  OR_MSK_CONFIG = config_path,
  OR_MSK_MODE = mode,
  OR_MSK_SEED = as.character(config$seed),
  OR_MSK_OUTPUT_ROOT = target_root
)

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) stop("Rscript was not found")
run_step <- function(step_name, script_relative, arguments, completion_marker) {
  log_path <- file.path(target_root, "logs", paste0(step_name, ".log"))
  status <- system2(
    rscript,
    args = c(shQuote(file.path(project_root, script_relative)), vapply(arguments, shQuote, character(1))),
    stdout = log_path,
    stderr = log_path
  )
  if (!identical(as.integer(status), 0L)) {
    log_tail <- utils::tail(readLines(log_path, warn = FALSE, encoding = "UTF-8"), 30L)
    stop(step_name, " failed with exit code ", status, "\n", paste(log_tail, collapse = "\n"))
  }
  log_text <- readLines(log_path, warn = FALSE, encoding = "UTF-8")
  if (!any(grepl(completion_marker, log_text, fixed = TRUE))) {
    stop(step_name, " exited 0 but completion marker is missing: ", completion_marker)
  }
  cat("STEP_OK=", step_name, "; LOG=", log_path, "\n", sep = "")
}

run_step(
  "04_multinomial_regression_resume",
  "R/04_multinomial_regression.R",
  c(
    file.path(target_root, "data", "operating_room_analysis_data.rds"),
    file.path(target_root, "models", "lca_class_assignments.csv"),
    file.path(target_root, "runtime", "lca_class_labels_runtime.csv"),
    file.path(target_root, "tables"),
    file.path(target_root, "figures")
  ),
  "STEP_COMPLETE=multinomial_regression"
)

run_step(
  "90_validate_outputs_resume",
  "R/90_validate_outputs.R",
  c(target_root),
  "STEP_COMPLETE=validate_outputs"
)

writeLines(capture.output(sessionInfo()), file.path(target_root, "sessionInfo.txt"), useBytes = TRUE)
writeLines(
  c(
    paste("Source interrupted run:", source_root),
    paste("Resume mode:", mode),
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"))
  ),
  file.path(target_root, "runtime", "resume_provenance.txt"),
  useBytes = TRUE
)

output_files <- list.files(target_root, recursive = TRUE, all.files = FALSE)
output_files <- output_files[!grepl("^logs/", output_files)]
manifest <- c(
  "# Analysis Outputs",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste("Study type:", config$project$design),
  paste("Run mode:", mode),
  paste("Random seed:", config$seed),
  paste("Resumed from:", source_root),
  "",
  "## Files",
  paste0("- `", output_files, "`")
)
writeLines(manifest, file.path(target_root, "_analysis_outputs.md"), useBytes = TRUE)

if (!file.exists(file.path(target_root, "RUN_COMPLETE.ok"))) stop("Final completion marker is missing")
cat("PIPELINE_RESUME_COMPLETE=TRUE\n")
cat("OUTPUT_ROOT=", target_root, "\n", sep = "")
