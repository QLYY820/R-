# Run all three ICN 2027 abstract analyses in a clean RStudio project session.
# Random seed: 42.

if (!file.exists(file.path("R", "common.R"))) {
  stop("Open ICN2027_Three_Abstracts_R.Rproj before running this file.", call. = FALSE)
}

set.seed(42)
analysis_scripts <- c(
  "01_negative_acts_exhaustion.R",
  "02_menopause_productivity_corrected.R",
  "03_work_family_turnover.R"
)

run_started <- Sys.time()
for (analysis_script in analysis_scripts) {
  message("Running ", analysis_script, " ...")
  analysis_environment <- new.env(parent = globalenv())
  sys.source(analysis_script, envir = analysis_environment)
  rm(analysis_environment)
  invisible(gc())
}

source(file.path("R", "common.R"), encoding = "UTF-8")
config <- load_project_config()
result_files <- if (dir.exists(config$output_dir)) {
  sort(list.files(config$output_dir, recursive = TRUE, full.names = FALSE))
} else {
  character()
}
run_lines <- c(
  paste0("Started: ", format(run_started, "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Finished: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "Status: all three analyses completed successfully",
  paste0("Scripts: ", paste(analysis_scripts, collapse = "; ")),
  paste0("Result files: ", length(result_files))
)
write_text_utf8(run_lines, file.path(config$output_dir, "run_log.txt"))
message("All three analyses completed. Results: ", config$output_dir)
