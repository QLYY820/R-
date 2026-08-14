# Abstract 1: Workplace negative acts and emotional exhaustion.
# Cross-sectional baseline analysis; HC3 robust linear regression.
# Random seed: 42.

if (!file.exists(file.path("R", "common.R"))) {
  stop("Open ICN2027_Three_Abstracts_R.Rproj before running this file.", call. = FALSE)
}
source(file.path("R", "common.R"), encoding = "UTF-8")
check_packages()
set.seed(42)

config <- load_project_config()
output_dir <- prepare_output_dir(config, "01_negative_acts_exhaustion")
source_data <- read_analysis_data(config$data_file)

negative_act_items <- paste0("F_q1_", seq_len(22L))
emotional_exhaustion_items <- paste0("F_q3_", c(1, 2, 3, 6, 8, 13, 14, 16, 20))
required_variables <- c(
  "F_fuxingxingwei_all", "F_qingganshuaijie", negative_act_items,
  emotional_exhaustion_items, "age", "A_q2", "A_q5", "A_q9", "A_q10"
)
assert_columns(source_data, required_variables)

negative_item_matrix <- as.data.frame(lapply(source_data[negative_act_items], as_numeric_safe))
exhaustion_item_matrix <- as.data.frame(lapply(source_data[emotional_exhaustion_items], as_numeric_safe))
negative_score_stored <- as_numeric_safe(source_data$F_fuxingxingwei_all)
exhaustion_score_stored <- as_numeric_safe(source_data$F_qingganshuaijie)
negative_score_recalculated <- rowSums(negative_item_matrix, na.rm = FALSE)
exhaustion_score_recalculated <- rowSums(exhaustion_item_matrix, na.rm = FALSE)

score_audit <- rbind(
  score_recalculation_audit(
    negative_score_stored, negative_score_recalculated,
    "Negative Acts Questionnaire, 22 items"
  ),
  score_recalculation_audit(
    exhaustion_score_stored, exhaustion_score_recalculated,
    "Emotional exhaustion, 9 items"
  )
)
write_csv_utf8(score_audit, file.path(output_dir, "score_recalculation_audit.csv"))
if (any(score_audit$mismatch_n > 0L)) {
  stop("Stored scale totals do not match item-level direct sums.", call. = FALSE)
}

analysis_data <- data.frame(
  emotional_exhaustion = exhaustion_score_stored,
  negative_acts = negative_score_stored,
  age = as_numeric_safe(source_data$age),
  sex = factor(source_data$A_q2),
  education = factor(source_data$A_q5),
  department = factor(source_data$A_q9),
  employment_type = factor(source_data$A_q10)
)
analysis_data$age_per_10_years <-
  (analysis_data$age - mean(analysis_data$age, na.rm = TRUE)) / 10
analysis_data$any_negative_act <- as.numeric(analysis_data$negative_acts > 22)

model_variables <- c(
  "emotional_exhaustion", "negative_acts", "age_per_10_years", "sex",
  "education", "department", "employment_type", "any_negative_act"
)
complete_data <- analysis_data[stats::complete.cases(analysis_data[model_variables]), , drop = FALSE]

if (any(complete_data$negative_acts < 22 | complete_data$negative_acts > 110)) {
  stop("Negative-acts score is outside 22-110.", call. = FALSE)
}
if (any(complete_data$emotional_exhaustion < 0 | complete_data$emotional_exhaustion > 54)) {
  stop("Emotional-exhaustion score is outside 0-54.", call. = FALSE)
}
assert_expected_n(
  nrow(complete_data), config$expected_n_1, "Abstract 1 complete-case N",
  config$strict_reproduction
)

sample_flow <- data.frame(
  stage = c("Input scored data", "Complete cases for adjusted model", "Exposed nurses in dose model"),
  n = c(nrow(analysis_data), nrow(complete_data), sum(complete_data$any_negative_act == 1)),
  excluded_from_previous_stage = c(
    NA_integer_, nrow(analysis_data) - nrow(complete_data), sum(complete_data$any_negative_act == 0)
  )
)
write_csv_utf8(sample_flow, file.path(output_dir, "sample_flow.csv"))

exposure_distribution <- data.frame(
  exposure = c("No negative acts", "At least one negative act"),
  n = c(sum(complete_data$any_negative_act == 0), sum(complete_data$any_negative_act == 1))
)
exposure_distribution$percent <- 100 * exposure_distribution$n / sum(exposure_distribution$n)
write_csv_utf8(exposure_distribution, file.path(output_dir, "exposure_distribution.csv"))

binary_model <- stats::lm(
  emotional_exhaustion ~ any_negative_act + age_per_10_years + sex + education +
    department + employment_type,
  data = complete_data
)
binary_vcov <- sandwich::vcovHC(binary_model, type = "HC3")

exposed_data <- complete_data[complete_data$any_negative_act == 1, , drop = FALSE]
exposed_data$negative_acts_per_10 <- (exposed_data$negative_acts - 23) / 10
dose_model <- stats::lm(
  emotional_exhaustion ~ negative_acts_per_10 + age_per_10_years + sex + education +
    department + employment_type,
  data = exposed_data
)
dose_vcov <- sandwich::vcovHC(dose_model, type = "HC3")

binary_coefficients <- robust_coefficient_table(binary_model, binary_vcov)
binary_coefficients$model <- "Any versus no negative acts"
dose_coefficients <- robust_coefficient_table(dose_model, dose_vcov)
dose_coefficients$model <- "Dose among exposed nurses"
coefficient_table <- rbind(binary_coefficients, dose_coefficients)
coefficient_table <- coefficient_table[, c("model", setdiff(names(coefficient_table), "model"))]
write_csv_utf8(coefficient_table, file.path(output_dir, "robust_regression_coefficients.csv"))

binary_primary <- binary_coefficients[binary_coefficients$term == "any_negative_act", , drop = FALSE]
dose_primary <- dose_coefficients[dose_coefficients$term == "negative_acts_per_10", , drop = FALSE]
binary_primary$standardized_effect <-
  binary_primary$estimate * stats::sd(complete_data$any_negative_act) /
  stats::sd(complete_data$emotional_exhaustion)
dose_primary$standardized_effect <-
  dose_primary$estimate * stats::sd(exposed_data$negative_acts_per_10) /
  stats::sd(exposed_data$emotional_exhaustion)

primary_effects <- data.frame(
  estimand = c(
    "Adjusted mean difference: any versus no negative acts",
    "Adjusted mean difference per 10-point higher score among exposed nurses"
  ),
  analysis_n = c(stats::nobs(binary_model), stats::nobs(dose_model)),
  estimate = c(binary_primary$estimate, dose_primary$estimate),
  robust_se = c(binary_primary$robust_se, dose_primary$robust_se),
  ci_lower = c(binary_primary$ci_lower, dose_primary$ci_lower),
  ci_upper = c(binary_primary$ci_upper, dose_primary$ci_upper),
  p_value = c(binary_primary$p_value, dose_primary$p_value),
  standardized_effect = c(binary_primary$standardized_effect, dose_primary$standardized_effect)
)
write_csv_utf8(primary_effects, file.path(output_dir, "abstract_primary_effects.csv"))

assert_near(
  primary_effects$estimate, c(13.04, 3.76), 0.02,
  "Abstract 1 headline estimates", config$strict_reproduction
)

exposed_quantiles <- as.numeric(stats::quantile(exposed_data$negative_acts, c(0.25, 0.75), names = FALSE))
iqr_multiplier <- diff(exposed_quantiles) / 10
write_csv_utf8(data.frame(
  contrast = "75th versus 25th percentile among exposed nurses",
  exposure_p25 = exposed_quantiles[[1L]], exposure_p75 = exposed_quantiles[[2L]],
  estimate = dose_primary$estimate * iqr_multiplier,
  ci_lower = dose_primary$ci_lower * iqr_multiplier,
  ci_upper = dose_primary$ci_upper * iqr_multiplier
), file.path(output_dir, "real_world_translation.csv"))

diagnostic_statistics <- rbind(
  cbind(model = "Any versus no negative acts", model_diagnostic_statistics(binary_model)),
  cbind(model = "Dose among exposed nurses", model_diagnostic_statistics(dose_model))
)
write_csv_utf8(diagnostic_statistics, file.path(output_dir, "model_diagnostic_statistics.csv"))
write_csv_utf8(vif_table(binary_model), file.path(output_dir, "vif_binary_model.csv"))
write_csv_utf8(vif_table(dose_model), file.path(output_dir, "vif_dose_model.csv"))
save_lm_diagnostics(binary_model, output_dir, "diagnostics_binary_model")
save_lm_diagnostics(dose_model, output_dir, "diagnostics_dose_model")

result_lines <- c(
  "Abstract 1: Workplace negative acts and emotional exhaustion",
  sprintf("Complete-case N: %s", format(stats::nobs(binary_model), big.mark = ",")),
  sprintf("Any exposure: %s/%s (%.1f%%)",
          format(sum(complete_data$any_negative_act == 1), big.mark = ","),
          format(nrow(complete_data), big.mark = ","),
          100 * mean(complete_data$any_negative_act == 1)),
  sprintf("Any versus no exposure: adjusted difference %.2f (95%% CI %.2f to %.2f), p %s.",
          binary_primary$estimate, binary_primary$ci_lower, binary_primary$ci_upper,
          format_p_value(binary_primary$p_value)),
  sprintf("Among %s exposed nurses, each 10-point higher score: adjusted difference %.2f (95%% CI %.2f to %.2f), p %s.",
          format(stats::nobs(dose_model), big.mark = ","), dose_primary$estimate,
          dose_primary$ci_lower, dose_primary$ci_upper, format_p_value(dose_primary$p_value)),
  "All confidence intervals and tests use HC3 robust covariance."
)
write_text_utf8(result_lines, file.path(output_dir, "results_for_abstract.txt"))
write_analysis_manifest(config, output_dir, "01_negative_acts_exhaustion.R", stats::nobs(binary_model))
message(paste(result_lines, collapse = "\n"))
