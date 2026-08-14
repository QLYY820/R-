# Abstract 3: Work-family balance and turnover intention.
# Cross-sectional baseline analysis; restricted cubic spline with HC3 covariance.
# Random seed: 42.

if (!file.exists(file.path("R", "common.R"))) {
  stop("Open ICN2027_Three_Abstracts_R.Rproj before running this file.", call. = FALSE)
}
source(file.path("R", "common.R"), encoding = "UTF-8")
check_packages()
set.seed(42)

config <- load_project_config()
output_dir <- prepare_output_dir(config, "03_work_family_turnover")
source_data <- read_analysis_data(config$data_file)

work_family_items <- paste0("C_q50_", seq_len(14L))
required_variables <- c(
  "C_gzjt_all", "C_lizhiyiyuan_all", work_family_items,
  "age", "A_q2", "A_q6", "A_q9", "A_q10"
)
assert_columns(source_data, required_variables)

work_family_item_matrix <- as.data.frame(lapply(source_data[work_family_items], as_numeric_safe))
conflict_item_indices <- c(1:4, 8:11)
harmonized_item_matrix <- work_family_item_matrix
harmonized_item_matrix[conflict_item_indices] <- lapply(
  harmonized_item_matrix[conflict_item_indices], function(item) 6 - item
)
work_family_recalculated <- rowSums(harmonized_item_matrix, na.rm = FALSE)
work_family_stored <- as_numeric_safe(source_data$C_gzjt_all)
score_audit <- score_recalculation_audit(
  work_family_stored, work_family_recalculated,
  "Direction-harmonised 14-item work-family balance score"
)
write_csv_utf8(score_audit, file.path(output_dir, "score_recalculation_audit.csv"))
if (score_audit$mismatch_n > 0L) {
  stop("Stored work-family balance score does not match the item-level recalculation.", call. = FALSE)
}

analysis_data <- data.frame(
  work_family_balance = work_family_stored,
  turnover_intention = as_numeric_safe(source_data$C_lizhiyiyuan_all),
  age = as_numeric_safe(source_data$age),
  sex = factor(source_data$A_q2),
  marital_status = factor(source_data$A_q6),
  department = factor(source_data$A_q9),
  employment_type = factor(source_data$A_q10)
)
analysis_data$age_per_10_years <-
  (analysis_data$age - mean(analysis_data$age, na.rm = TRUE)) / 10
model_variables <- c(
  "turnover_intention", "work_family_balance", "age_per_10_years", "sex",
  "marital_status", "department", "employment_type"
)
complete_data <- analysis_data[stats::complete.cases(analysis_data[model_variables]), , drop = FALSE]

if (any(complete_data$work_family_balance < 14 | complete_data$work_family_balance > 70)) {
  stop("Work-family balance score is outside 14-70.", call. = FALSE)
}
if (any(complete_data$turnover_intention < 6 | complete_data$turnover_intention > 24)) {
  stop("Turnover-intention score is outside 6-24.", call. = FALSE)
}
assert_expected_n(
  nrow(complete_data), config$expected_n_3, "Abstract 3 complete-case N",
  config$strict_reproduction
)

sample_flow <- data.frame(
  stage = c("Input scored data", "Complete cases for adjusted spline model"),
  n = c(nrow(analysis_data), nrow(complete_data)),
  excluded_from_previous_stage = c(NA_integer_, nrow(analysis_data) - nrow(complete_data))
)
write_csv_utf8(sample_flow, file.path(output_dir, "sample_flow.csv"))

score_quantiles <- data.frame(
  percentile = c(5, 10, 25, 35, 50, 65, 75, 90, 95),
  score = as.numeric(stats::quantile(
    complete_data$work_family_balance,
    c(0.05, 0.10, 0.25, 0.35, 0.50, 0.65, 0.75, 0.90, 0.95), names = FALSE
  ))
)
write_csv_utf8(score_quantiles, file.path(output_dir, "work_family_score_quantiles.csv"))

spline_knots <- as.numeric(stats::quantile(
  complete_data$work_family_balance, c(0.05, 0.35, 0.65, 0.95), names = FALSE
))
if (length(unique(spline_knots)) != 4L) stop("Spline knots are not distinct.", call. = FALSE)
write_csv_utf8(data.frame(
  percentile = c(5, 35, 65, 95), knot = spline_knots
), file.path(output_dir, "spline_knots.csv"))

spline_model <- stats::lm(
  turnover_intention ~ rms::rcs(work_family_balance, spline_knots) +
    age_per_10_years + sex + marital_status + department + employment_type,
  data = complete_data
)
spline_vcov <- sandwich::vcovHC(spline_model, type = "HC3")
coefficient_table <- robust_coefficient_table(spline_model, spline_vcov)
write_csv_utf8(coefficient_table, file.path(output_dir, "robust_regression_coefficients.csv"))

all_spline_terms <- grep("rms::rcs", names(stats::coef(spline_model)), fixed = TRUE, value = TRUE)
nonlinear_terms <- grep("'", all_spline_terms, fixed = TRUE, value = TRUE)
association_tests <- rbind(
  robust_linear_hypothesis(
    spline_model, all_spline_terms, spline_vcov,
    "Overall association of work-family balance"
  ),
  robust_linear_hypothesis(
    spline_model, nonlinear_terms, spline_vcov,
    "Nonlinear component of work-family balance"
  )
)
write_csv_utf8(association_tests, file.path(output_dir, "spline_association_tests.csv"))
nonlinearity_test <- association_tests[2L, , drop = FALSE]

make_newdata <- function(balance_values) {
  data.frame(
    work_family_balance = balance_values,
    age_per_10_years = 0,
    sex = factor(rep(most_common_level(complete_data$sex), length(balance_values)),
                 levels = levels(complete_data$sex)),
    marital_status = factor(
      rep(most_common_level(complete_data$marital_status), length(balance_values)),
      levels = levels(complete_data$marital_status)
    ),
    department = factor(rep(most_common_level(complete_data$department), length(balance_values)),
                        levels = levels(complete_data$department)),
    employment_type = factor(
      rep(most_common_level(complete_data$employment_type), length(balance_values)),
      levels = levels(complete_data$employment_type)
    )
  )
}

reference_balance <- 40
abstract_balance_values <- c(40, 44, 50, 57)
abstract_contrasts <- contrast_differences(
  spline_model, spline_vcov, make_newdata(abstract_balance_values),
  exposure = "work_family_balance", reference_value = reference_balance
)
abstract_contrasts$comparison <- paste0(
  "Work-family balance ", abstract_contrasts$exposure_value, " versus ", reference_balance
)
abstract_contrasts$lower_turnover_magnitude <- -abstract_contrasts$estimate
abstract_contrasts$lower_turnover_ci_lower <- -abstract_contrasts$ci_upper
abstract_contrasts$lower_turnover_ci_upper <- -abstract_contrasts$ci_lower
abstract_contrasts <- abstract_contrasts[, c(
  "comparison", "exposure_value", "reference_value", "estimate", "robust_se",
  "ci_lower", "ci_upper", "lower_turnover_magnitude",
  "lower_turnover_ci_lower", "lower_turnover_ci_upper"
)]
write_csv_utf8(abstract_contrasts, file.path(output_dir, "abstract_adjusted_contrasts.csv"))

headline_rows <- abstract_contrasts$exposure_value %in% c(44, 50, 57)
assert_near(
  abstract_contrasts$lower_turnover_magnitude[headline_rows], c(1.59, 3.72, 4.73), 0.02,
  "Abstract 3 headline contrasts", config$strict_reproduction
)

curve_balance_values <- seq(
  min(complete_data$work_family_balance), max(complete_data$work_family_balance), length.out = 250L
)
curve_data <- contrast_differences(
  spline_model, spline_vcov, make_newdata(curve_balance_values),
  exposure = "work_family_balance", reference_value = reference_balance
)
write_csv_utf8(curve_data, file.path(output_dir, "adjusted_spline_curve.csv"))

spline_plot <- ggplot2::ggplot(curve_data, ggplot2::aes(x = exposure_value, y = estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                       fill = "#009E73", alpha = 0.24) +
  ggplot2::geom_line(color = "#007A5E", linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey40") +
  ggplot2::geom_vline(xintercept = reference_balance, linetype = 3, color = "grey55") +
  ggplot2::labs(
    x = "Direction-harmonised work-family balance score (14-70; higher = better)",
    y = "Adjusted difference in turnover-intention score (reference: 40)",
    title = "Work-family balance and turnover intention"
  ) +
  ggplot2::theme_classic(base_size = 10)
save_ggplot_pair(spline_plot, output_dir, "adjusted_spline_curve")

diagnostic_statistics <- model_diagnostic_statistics(spline_model)
write_csv_utf8(diagnostic_statistics, file.path(output_dir, "model_diagnostic_statistics.csv"))
write_csv_utf8(vif_table(spline_model), file.path(output_dir, "vif.csv"))
save_lm_diagnostics(spline_model, output_dir, "diagnostics_spline_model")
saveRDS(spline_model, file.path(output_dir, "spline_model.rds"))

contrast_at <- function(value) {
  abstract_contrasts[abstract_contrasts$exposure_value == value, , drop = FALSE]
}
contrast_44 <- contrast_at(44)
contrast_50 <- contrast_at(50)
contrast_57 <- contrast_at(57)
result_lines <- c(
  "Abstract 3: Work-family balance and turnover intention",
  sprintf("Complete-case N: %s", format(stats::nobs(spline_model), big.mark = ",")),
  sprintf("Spline nonlinearity: F = %.2f, p %s.",
          nonlinearity_test$statistic, format_p_value(nonlinearity_test$p_value)),
  sprintf("Balance 44 versus 40: turnover intention %.2f points lower (95%% CI %.2f to %.2f).",
          contrast_44$lower_turnover_magnitude, contrast_44$lower_turnover_ci_lower,
          contrast_44$lower_turnover_ci_upper),
  sprintf("Balance 50 versus 40: turnover intention %.2f points lower (95%% CI %.2f to %.2f).",
          contrast_50$lower_turnover_magnitude, contrast_50$lower_turnover_ci_lower,
          contrast_50$lower_turnover_ci_upper),
  sprintf("Balance 57 versus 40: turnover intention %.2f points lower (95%% CI %.2f to %.2f).",
          contrast_57$lower_turnover_magnitude, contrast_57$lower_turnover_ci_lower,
          contrast_57$lower_turnover_ci_upper)
)
write_text_utf8(result_lines, file.path(output_dir, "results_for_abstract.txt"))
write_analysis_manifest(config, output_dir, "03_work_family_turnover.R", stats::nobs(spline_model))
message(paste(result_lines, collapse = "\n"))
