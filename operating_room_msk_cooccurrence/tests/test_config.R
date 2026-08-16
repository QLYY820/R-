if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))
}
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
project_root_hint <- Sys.getenv("OR_MSK_PROJECT_ROOT")
project_root <- if (nzchar(project_root_hint)) {
  project_root_hint
} else {
  test_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE)
  normalizePath(file.path(dirname(test_path), ".."), winslash = "/", mustWork = TRUE)
}
source(file.path(project_root, "config", "analysis_config.R"))
config <- build_analysis_config(project_root, mode = "test", output_root = file.path(tempdir(), "or_msk_test_output"))
stopifnot(config$seed == 42L)
stopifnot(config$mode == "test")
stopifnot(config$runtime$network_boots < 1000L)
stopifnot(config$runtime$mice_m < 20L)
stopifnot(basename(config$paths$raw_xlsx) == "data.xlsx")
stopifnot(basename(config$paths$data_text_rds) == "data_text.rds")
stopifnot(length(config$variables$sites) == 9L)
stopifnot(identical(config$variables$survey_date, "submittime"))
stopifnot(length(config$variables$response_time_variables) == 4L)
stopifnot(config$cohort_cleaning$minimum_work_years == 1)
stopifnot(config$cohort_cleaning$minimum_nursing_entry_age == 16)
stopifnot(config$cohort_cleaning$minimum_response_time_seconds == 600)
cat("TEST_CONFIG=PASS\n")
