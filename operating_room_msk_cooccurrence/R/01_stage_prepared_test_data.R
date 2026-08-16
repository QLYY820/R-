# Analysis: Stage a deidentified prepared analysis RDS for test-mode execution.
# Date: 2026-08-17
# Random seed: supplied by OR_MSK_SEED (default 42; no stochastic operations).
# Key package: data.table.

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Chinese"), silent = TRUE))
}
seed <- as.integer(Sys.getenv("OR_MSK_SEED", "42"))
set.seed(seed)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: Rscript R/01_stage_prepared_test_data.R <prepared_analysis.rds> <survey_year.csv> <output_data_dir>")
}
input_path <- args[[1L]]
survey_year_source <- args[[2L]]
output_dir <- args[[3L]]
if (!file.exists(input_path)) stop("Prepared test RDS does not exist: ", input_path)
if (!file.exists(survey_year_source)) stop("Survey-year companion does not exist: ", survey_year_source)
if (!dir.exists(output_dir)) stop("Output data directory does not exist: ", output_dir)
if (!requireNamespace("data.table", quietly = TRUE)) stop("Missing package: data.table")

data <- readRDS(input_path)
sites <- c("neck", "shoulder", "upper_back", "elbow", "wrist_hand", "lower_back", "hip_thigh", "knee", "ankle_foot")
symptom_vars <- paste0(sites, "_symptom_12m")
required <- c("row_id", symptom_vars)
missing <- setdiff(required, names(data))
if (length(missing)) stop("Prepared test RDS lacks required columns: ", paste(missing, collapse = ", "))
if (anyDuplicated(data$row_id)) stop("Prepared test RDS contains duplicate row_id values")

survey_year <- data.table::fread(survey_year_source, data.table = FALSE)
if (!all(c("row_id", "survey_year") %in% names(survey_year))) stop("Survey-year companion has invalid columns")
if (!setequal(data$row_id, survey_year$row_id)) stop("Survey-year companion does not reconcile to prepared test RDS")

saveRDS(data, file.path(output_dir, "operating_room_analysis_data.rds"))
data.table::fwrite(survey_year, file.path(output_dir, "operating_room_survey_year.csv"), bom = TRUE)

complete_n <- sum(stats::complete.cases(data[symptom_vars]))
sample_audit <- data.frame(
  metric = c(
    "source_clean_rows", "operating_room_rows", "operating_room_percent",
    "duplicate_coded_id_rows", "complete_primary_symptom_rows", "survey_year_missing_rows"
  ),
  value = c(nrow(data), nrow(data), 1, 0, complete_n, sum(is.na(survey_year$survey_year)))
)
missingness <- data.frame(
  variable = names(data),
  missing_n = vapply(data, function(x) sum(is.na(x)), numeric(1)),
  stringsAsFactors = FALSE
)
missingness$missing_percent <- missingness$missing_n / nrow(data)

wilson_ci <- function(events, total) {
  interval <- suppressWarnings(stats::prop.test(events, total, correct = FALSE)$conf.int)
  c(max(0, interval[[1L]]), min(1, interval[[2L]]))
}
site_prevalence <- do.call(rbind, lapply(seq_along(symptom_vars), function(index) {
  values <- data[[symptom_vars[[index]]]]
  total <- sum(!is.na(values))
  events <- sum(values == 1, na.rm = TRUE)
  interval <- wilson_ci(events, total)
  data.frame(
    site = sites[[index]], events = events, total = total, prevalence = events / total,
    ci_lower = interval[[1L]], ci_upper = interval[[2L]], missing = sum(is.na(values))
  )
}))

data.table::fwrite(sample_audit, file.path(output_dir, "sample_audit.csv"), bom = TRUE)
data.table::fwrite(missingness, file.path(output_dir, "variable_missingness.csv"), bom = TRUE)
data.table::fwrite(site_prevalence, file.path(output_dir, "site_prevalence_12m.csv"), bom = TRUE)
cat("Prepared test rows:", nrow(data), "; complete symptom rows:", complete_n, "\n")
cat("STEP_COMPLETE=stage_prepared_test_data\n")
