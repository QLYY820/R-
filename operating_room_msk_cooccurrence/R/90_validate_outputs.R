# Analysis: Validate all aggregate statistical outputs for the operating-room project.
# Date: 2026-08-17
# Random seed: supplied by OR_MSK_SEED (default 42; no stochastic operations).
# Key package: data.table.

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))
}
seed <- as.integer(Sys.getenv("OR_MSK_SEED", "42"))
set.seed(seed)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript R/90_validate_outputs.R <run_output_root>")
run_root <- args[[1L]]
if (!dir.exists(run_root)) stop("Run output root does not exist: ", run_root)
if (!requireNamespace("data.table", quietly = TRUE)) stop("Missing package: data.table")

required <- c(
  "data/sample_audit.csv",
  "data/variable_missingness.csv",
  "data/site_prevalence_12m.csv",
  "data/operating_room_analysis_data.rds",
  "models/lca_fit_statistics.csv",
  "models/lca_class_quality.csv",
  "models/lca_class_assignments.csv",
  "models/ising_adjacency_matrix.csv",
  "models/ising_centrality.csv",
  "models/ising_stability.csv",
  "tables/multinomial_primary_complete_case.csv",
  "tables/regression_complete_case_audit.csv",
  "tables/multinomial_posterior_draw_sensitivity.csv",
  "tables/multinomial_vif_primary.csv",
  "tables/multinomial_linearity_test.csv",
  "tables/multinomial_model_fit_summary.csv",
  "tables/multinomial_epv.csv",
  "tables/multinomial_marginal_effects.csv",
  "figures/multinomial_work_factor_forest.pdf",
  "figures/multinomial_work_factor_forest.png"
)
paths <- file.path(run_root, required)
missing <- required[!file.exists(paths) | file.info(paths)$size <= 0]
if (length(missing)) stop("Missing or empty required outputs: ", paste(missing, collapse = ", "))

audit <- data.table::fread(file.path(run_root, "data", "sample_audit.csv"), data.table = FALSE)
value_of <- function(metric) as.numeric(audit$value[match(metric, audit$metric)])
source_n <- value_of("source_clean_rows")
operating_n <- value_of("operating_room_rows")
complete_n <- value_of("complete_primary_symptom_rows")
if (anyNA(c(source_n, operating_n, complete_n))) stop("Sample audit lacks required metrics")
if (operating_n <= 0 || operating_n > source_n || complete_n > operating_n) stop("Sample-flow reconciliation failed")
if (identical(Sys.getenv("OR_MSK_MODE", "formal"), "formal")) {
  flow_metrics <- c(
    "source_raw_rows", "duplicate_id_rows_removed", "core_missing_excluded_rows",
    "work_years_lt_1_excluded_rows", "nursing_entry_age_lt_16_excluded_rows",
    "response_time_lt_600_excluded_rows", "source_clean_rows"
  )
  flow <- vapply(flow_metrics, value_of, numeric(1))
  if (anyNA(flow) || flow[[1L]] - sum(flow[2:6]) != flow[[7L]]) {
    stop("Formal parent-cohort exclusion counts do not reconcile")
  }
  operating_before_year <- value_of("operating_room_rows_before_survey_year_exclusion")
  unresolved_year <- value_of("survey_year_unresolved_excluded_rows")
  if (
    anyNA(c(operating_before_year, unresolved_year)) ||
      operating_before_year - unresolved_year != operating_n
  ) {
    stop("Formal operating-room survey-year exclusion counts do not reconcile")
  }
}

assignments <- data.table::fread(file.path(run_root, "models", "lca_class_assignments.csv"), data.table = FALSE)
if (nrow(assignments) != complete_n) stop("LCA assignment count does not match complete symptom records")
if (anyDuplicated(assignments$row_id)) stop("LCA assignments contain duplicate row_id values")

adjacency <- data.table::fread(file.path(run_root, "models", "ising_adjacency_matrix.csv"), data.table = FALSE)
matrix_values <- as.matrix(adjacency[, -1L, drop = FALSE])
storage.mode(matrix_values) <- "double"
if (nrow(matrix_values) != ncol(matrix_values)) stop("Ising adjacency matrix is not square")
if (max(abs(matrix_values - t(matrix_values)), na.rm = TRUE) > 1e-10) stop("Ising adjacency matrix is not symmetric")
if (max(abs(diag(matrix_values)), na.rm = TRUE) > 1e-10) stop("Ising adjacency diagonal is not zero")

regression_audit <- data.table::fread(file.path(run_root, "tables", "regression_complete_case_audit.csv"), data.table = FALSE)
regression_value <- function(metric) as.numeric(regression_audit$value[match(metric, regression_audit$metric)])
regression_input_n <- regression_value("regression_input_rows")
bmi_excluded_n <- regression_value("bmi_missing_or_nonfinite_excluded_rows")
regression_n <- regression_value("regression_complete_case_rows")
primary_model_n <- regression_value("primary_model_complete_rows")
if (anyNA(c(regression_input_n, bmi_excluded_n, regression_n, primary_model_n))) {
  stop("Regression sample audit lacks required metrics")
}
if (
  regression_input_n != nrow(assignments) ||
    regression_input_n - bmi_excluded_n != regression_n ||
    regression_n != primary_model_n
) {
  stop("BMI complete-case regression sample arithmetic failed")
}
model_fit <- data.table::fread(file.path(run_root, "tables", "multinomial_model_fit_summary.csv"), data.table = FALSE)
fitted_n <- as.numeric(model_fit$estimate[match("n", model_fit$metric)])
if (is.na(fitted_n) || fitted_n != regression_n) stop("Primary fitted sample does not match regression audit")

primary <- data.table::fread(file.path(run_root, "tables", "multinomial_primary_complete_case.csv"), data.table = FALSE)
needed_primary <- c("y.level", "term", "OR", "ci_lower", "ci_upper", "p_value")
if (!all(needed_primary %in% names(primary))) stop("Primary regression output lacks required columns")
if (any(!is.finite(primary$OR)) || any(primary$ci_lower > primary$OR) || any(primary$ci_upper < primary$OR)) {
  stop("Primary OR/CI validation failed")
}

summary <- data.frame(
  check = c("required_outputs", "sample_flow", "lca_assignments", "ising_matrix", "regression_sample", "primary_or_ci"),
  status = "PASS",
  detail = c(
    paste(length(required), "required files exist and are nonempty"),
    paste0("source=", source_n, "; operating_room=", operating_n, "; complete_symptoms=", complete_n),
    paste(nrow(assignments), "unique assignments"),
    paste(nrow(matrix_values), "symmetric nodes; zero diagonal"),
    paste0("input=", regression_input_n, "; BMI excluded=", bmi_excluded_n, "; fitted=", regression_n),
    paste(nrow(primary), "primary coefficient rows with coherent 95% CIs")
  ),
  stringsAsFactors = FALSE
)
data.table::fwrite(summary, file.path(run_root, "validation_summary.csv"), bom = TRUE)
writeLines(c("RUN_COMPLETE", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")), file.path(run_root, "RUN_COMPLETE.ok"))
print(summary)
cat("STEP_COMPLETE=validate_outputs\n")
