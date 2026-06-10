options(stringsAsFactors = FALSE, scipen = 999)

# Final head-nurse analysis runner for the bastion RStudio environment.
# Expected use after the final questionnaire data have been loaded as object `data`:
#   source("scripts/run_head_nurse_final_bastion_analysis.R")

parse_runner_args <- function(args) {
  out <- list()
  i <- 1
  while (i <= length(args)) {
    item <- args[[i]]
    if (grepl("^--[^=]+=", item)) {
      key <- sub("^--([^=]+)=.*$", "\\1", item)
      value <- sub("^--[^=]+=", "", item)
      out[[key]] <- value
      i <- i + 1
    } else if (startsWith(item, "--") && i < length(args)) {
      out[[substring(item, 3)]] <- args[[i + 1]]
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  out
}

runner_args <- parse_runner_args(commandArgs(trailingOnly = TRUE))

default_out_dir <- file.path("analysis_outputs", "head_nurse_occupational_health_final")
out_dir <- runner_args[["out-dir"]]
if (is.null(out_dir) || !nzchar(out_dir)) {
  out_dir <- Sys.getenv("HEAD_NURSE_BASE_DIR", unset = default_out_dir)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!nzchar(Sys.getenv("HEAD_NURSE_MAIN_DOCX_NAME", unset = ""))) {
  Sys.setenv(
    HEAD_NURSE_MAIN_DOCX_NAME =
      "head_nurse_occupational_health_baseline_cross_sectional_manuscript.docx"
  )
}
Sys.setenv(
  NURSE_OUTPUT_DIR = out_dir,
  HEAD_NURSE_BASE_DIR = out_dir
)

github_raw_base <- Sys.getenv(
  "HEAD_NURSE_GITHUB_RAW_BASE",
  unset = "https://raw.githubusercontent.com/QLYY820/R-/codex/msk-lca-rstudio-bastion-20260602/scripts"
)

this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) NA_character_
)
local_scripts_dir <- if (!is.na(this_file) && !grepl("^https?://", this_file)) {
  dirname(this_file)
} else {
  file.path(getwd(), "scripts")
}

source_script <- function(file_name) {
  local_candidates <- unique(c(
    file.path(local_scripts_dir, file_name),
    file.path(getwd(), "scripts", file_name),
    file_name
  ))

  for (path in local_candidates) {
    if (file.exists(path)) {
      message("Sourcing local script: ", normalizePath(path, winslash = "/", mustWork = FALSE))
      source(path, local = FALSE, encoding = "UTF-8")
      return(invisible(path))
    }
  }

  remote_url <- paste0(github_raw_base, "/", file_name)
  message("Sourcing GitHub script: ", remote_url)
  source(remote_url, local = FALSE, encoding = "UTF-8")
  invisible(remote_url)
}

if (exists("data", envir = .GlobalEnv)) {
  final_data <- get("data", envir = .GlobalEnv)
  if (is.data.frame(final_data)) {
    message("Detected final data object `data`: ", nrow(final_data), " rows x ", ncol(final_data), " columns.")
  } else {
    message("Object `data` exists but is not a data frame; the core script will try other configured sources.")
  }
} else {
  message("No object named `data` detected. The core script will try other configured sources if available.")
}

message("Output directory: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))

source_script("run_head_nurse_propensity_health_analysis_20260606_ijns.R")

required_core_outputs <- c(
  "00_cleaning_rules.md",
  "03c_baseline_table1_journal.csv",
  "04_propensity_summary.csv",
  "07_outcome_models_att.csv",
  "08_risk_transition_summary.csv"
)
missing_core_outputs <- required_core_outputs[
  !file.exists(file.path(out_dir, required_core_outputs))
]
if (length(missing_core_outputs) > 0) {
  stop(
    "Core analysis did not create expected output file(s): ",
    paste(missing_core_outputs, collapse = ", "),
    call. = FALSE
  )
}

source_script("build_head_nurse_baseline_cross_sectional_manuscript_formatted.R")
source_script("build_head_nurse_supplementary_material_final.R")

final_files <- c(
  file.path(out_dir, Sys.getenv("HEAD_NURSE_MAIN_DOCX_NAME")),
  file.path(out_dir, "head_nurse_occupational_health_supplementary_material.docx"),
  file.path(out_dir, "00_cleaning_rules.md"),
  file.path(out_dir, "analysis_run_log.txt")
)

message("")
message("Final head-nurse analysis completed.")
message("Key files:")
for (path in final_files) {
  message(" - ", normalizePath(path, winslash = "/", mustWork = FALSE))
}
