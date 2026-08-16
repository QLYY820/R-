# Analysis: End-to-end operating-room multisite musculoskeletal symptom pipeline.
# Date: 2026-08-17
# Random seed: centralized in config/analysis_config.R.
# R: tested with R 4.5.0; package versions are captured in sessionInfo.txt.

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))
}

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_argument)) stop("run_all.R must be executed with Rscript")
project_root_hint <- Sys.getenv("OR_MSK_PROJECT_ROOT")
project_root <- if (nzchar(project_root_hint)) {
  project_root_hint
} else {
  script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]), winslash = "/", mustWork = TRUE)
  normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
}
config_path <- file.path(project_root, "config", "analysis_config.R")
source(config_path)

parse_options <- function(arguments) {
  values <- list(
    mode = "formal", output_root = NULL, data_text_rds = NULL,
    raw_xlsx = NULL, analysis_rds = NULL
  )
  for (argument in arguments) {
    if (grepl("^--mode=", argument)) values$mode <- sub("^--mode=", "", argument)
    else if (grepl("^--output-root=", argument)) values$output_root <- sub("^--output-root=", "", argument)
    else if (grepl("^--data-text-rds=", argument)) values$data_text_rds <- sub("^--data-text-rds=", "", argument)
    else if (grepl("^--raw-xlsx=", argument)) values$raw_xlsx <- sub("^--raw-xlsx=", "", argument)
    else if (grepl("^--analysis-rds=", argument)) values$analysis_rds <- sub("^--analysis-rds=", "", argument)
    else stop("Unknown argument: ", argument)
  }
  if (!values$mode %in% c("formal", "test")) stop("--mode must be formal or test")
  values
}

options_cli <- parse_options(commandArgs(trailingOnly = TRUE))
if (!is.null(options_cli$data_text_rds)) Sys.setenv(OR_MSK_DATA_TEXT_RDS = options_cli$data_text_rds)
if (!is.null(options_cli$raw_xlsx)) Sys.setenv(OR_MSK_RAW_XLSX = options_cli$raw_xlsx)
if (!is.null(options_cli$analysis_rds) && options_cli$mode != "test") {
  stop("--analysis-rds is permitted only in test mode")
}
config <- build_analysis_config(project_root, mode = options_cli$mode, output_root = options_cli$output_root)
set.seed(config$seed)

run_root <- config$paths$output_root
if (file.exists(run_root)) stop("Output target already exists; refusing to overwrite: ", run_root)
dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
for (directory in c("data", "models", "tables", "figures", "logs", "runtime")) {
  dir.create(file.path(run_root, directory), recursive = TRUE, showWarnings = FALSE)
}

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) stop("Rscript was not found")
Sys.setenv(
  OR_MSK_PROJECT_ROOT = project_root,
  OR_MSK_CONFIG = config_path,
  OR_MSK_MODE = config$mode,
  OR_MSK_SEED = as.character(config$seed),
  OR_MSK_OUTPUT_ROOT = run_root
)

run_step <- function(step_name, script_relative, arguments, completion_marker) {
  script <- file.path(project_root, script_relative)
  if (!file.exists(script)) stop("Missing step script: ", script)
  log_path <- file.path(run_root, "logs", paste0(step_name, ".log"))
  status <- system2(
    rscript,
    args = c(shQuote(script), vapply(arguments, shQuote, character(1))),
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
  invisible(log_path)
}

data_dir <- file.path(run_root, "data")
models_dir <- file.path(run_root, "models")
tables_dir <- file.path(run_root, "tables")
figures_dir <- file.path(run_root, "figures")

if (!is.null(options_cli$analysis_rds)) {
  run_step(
    "01_stage_prepared_test_data",
    "R/01_stage_prepared_test_data.R",
    c(
      options_cli$analysis_rds,
      file.path(dirname(options_cli$analysis_rds), "operating_room_survey_year.csv"),
      data_dir
    ),
    "STEP_COMPLETE=stage_prepared_test_data"
  )
} else {
  if (!file.exists(config$paths$data_text_rds) || file.info(config$paths$data_text_rds)$size <= 0) {
    run_step(
      "00_import_data_text",
      "R/00_import_data_text.R",
      c(config$paths$raw_xlsx, config$paths$data_text_rds),
      "STEP_COMPLETE=import_data_text_created"
    )
  } else {
    raw_text <- readRDS(config$paths$data_text_rds)
    if (!is.data.frame(raw_text) || nrow(raw_text) == 0L || !all(vapply(raw_text, is.character, logical(1)))) {
      stop("Existing data_text.rds is not a nonempty all-text data frame")
    }
    cat("STEP_OK=00_import_data_text_reused; ROWS=", nrow(raw_text), "; COLS=", ncol(raw_text), "\n", sep = "")
    rm(raw_text)
    invisible(gc())
  }
  run_step(
    "01_prepare_analysis_data",
    "R/01_prepare_analysis_data.R",
    c(config$paths$data_text_rds, data_dir),
    "STEP_COMPLETE=prepare_analysis_data"
  )
}
analysis_path <- file.path(data_dir, "operating_room_analysis_data.rds")

formal_labels <- if (config$mode == "formal") config$paths$class_labels else character(0)
lca_arguments <- c(analysis_path, models_dir, formal_labels)
run_step(
  "02_latent_class_analysis",
  "R/02_latent_class_analysis.R",
  lca_arguments,
  "STEP_COMPLETE=latent_class_analysis"
)

runtime_labels <- file.path(run_root, "runtime", "lca_class_labels_runtime.csv")
quality <- data.table::fread(file.path(models_dir, "lca_class_quality.csv"), data.table = FALSE)
if (config$mode == "formal") {
  labels <- data.table::fread(config$paths$class_labels, data.table = FALSE)
  if (!setequal(labels$latent_class, quality$class)) {
    stop("Formal LCA class identities do not match the prespecified label configuration")
  }
} else {
  ordered_classes <- quality$class[order(quality$mean_site_probability)]
  labels <- data.frame(
    latent_class = ordered_classes,
    class_label_en = c("Low burden", paste("Test symptom profile", seq_len(length(ordered_classes) - 1L) + 1L)),
    class_label_cn = c("低负担型", paste("测试症状类别", seq_len(length(ordered_classes) - 1L) + 1L)),
    presentation_order = seq_along(ordered_classes),
    stringsAsFactors = FALSE
  )
}
data.table::fwrite(labels, runtime_labels, bom = TRUE)

run_step(
  "03_ising_network_analysis",
  "R/03_ising_network_analysis.R",
  c(analysis_path, models_dir),
  "STEP_COMPLETE=ising_network_analysis"
)

run_step(
  "04_multinomial_regression",
  "R/04_multinomial_regression.R",
  c(
    analysis_path,
    file.path(models_dir, "lca_class_assignments.csv"),
    runtime_labels,
    tables_dir,
    figures_dir
  ),
  "STEP_COMPLETE=multinomial_regression"
)

run_step(
  "90_validate_outputs",
  "R/90_validate_outputs.R",
  c(run_root),
  "STEP_COMPLETE=validate_outputs"
)

session_path <- file.path(run_root, "sessionInfo.txt")
writeLines(capture.output(sessionInfo()), session_path, useBytes = TRUE)

output_files <- list.files(run_root, recursive = TRUE, all.files = FALSE)
output_files <- output_files[!grepl("^logs/", output_files)]
manifest <- c(
  "# Analysis Outputs",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste("Study type:", config$project$design),
  paste("Run mode:", config$mode),
  paste("Random seed:", config$seed),
  "",
  "## Files",
  paste0("- `", output_files, "`")
)
writeLines(manifest, file.path(run_root, "_analysis_outputs.md"), useBytes = TRUE)

if (!file.exists(file.path(run_root, "RUN_COMPLETE.ok"))) stop("Final completion marker is missing")
cat("PIPELINE_COMPLETE=TRUE\n")
cat("OUTPUT_ROOT=", run_root, "\n", sep = "")
