# Abstract 2: Modified Kupperman symptoms and health-related productivity loss.
# Corrected SPS-6 scoring; restricted cubic spline with HC3 robust covariance.
# Random seed: 42.

if (!file.exists(file.path("R", "common.R"))) {
  stop("Open ICN2027_Three_Abstracts_R.Rproj before running this file.", call. = FALSE)
}
source(file.path("R", "common.R"), encoding = "UTF-8")
check_packages()
set.seed(42)

config <- load_project_config()
output_dir <- prepare_output_dir(config, "02_menopause_productivity_corrected")
source_data <- read_analysis_data(config$data_file)

sps_items <- paste0("D_q21_", seq_len(6L))
kupperman_items <- paste0("B_q", 83:95)
routing_variables <- c("B_q75", paste0("B_q", 76:81), kupperman_items)
required_variables <- c(
  "B_weijuejingqizonghezheng_all", "B_wyjqzhz_fenji",
  "D_shengchanlishousun_all", sps_items, "age", "A_q2", "A_q6",
  "A_q9", "A_q10", "C_q8", routing_variables
)
assert_columns(source_data, required_variables)

# The six stored item variables already contain the dictionary-assigned scores.
# In particular, D_q21_5 and D_q21_6 must not be transformed again.
sps_item_matrix <- as.data.frame(lapply(source_data[sps_items], as_numeric_safe))
names(sps_item_matrix) <- sps_items
item_validation <- data.frame(
  item = sps_items,
  valid_n = vapply(sps_item_matrix, function(x) sum(is.finite(x)), integer(1L)),
  missing_n = vapply(sps_item_matrix, function(x) sum(!is.finite(x)), integer(1L)),
  min = vapply(sps_item_matrix, function(x) min(x, na.rm = TRUE), numeric(1L)),
  max = vapply(sps_item_matrix, function(x) max(x, na.rm = TRUE), numeric(1L)),
  outside_1_to_5 = vapply(
    sps_item_matrix, function(x) sum(is.finite(x) & (x < 1 | x > 5)), integer(1L)
  )
)
write_csv_utf8(item_validation, file.path(output_dir, "corrected_item_validation.csv"))
if (any(item_validation$outside_1_to_5 > 0L)) {
  stop("At least one SPS-6 item is outside 1-5.", call. = FALSE)
}

sps6_productivity_loss_corrected <- rowSums(
  sps_item_matrix[, c(
    "D_q21_1", "D_q21_2", "D_q21_3",
    "D_q21_4", "D_q21_5", "D_q21_6"
  ), drop = FALSE],
  na.rm = FALSE
)
sps6_productivity_loss_corrected <- as_numeric_safe(sps6_productivity_loss_corrected)
old_historical_score <- as_numeric_safe(source_data$D_shengchanlishousun_all)

if (any(is.finite(sps6_productivity_loss_corrected) &
        (sps6_productivity_loss_corrected < 6 | sps6_productivity_loss_corrected > 30))) {
  stop("Corrected SPS-6 total is outside 6-30.", call. = FALSE)
}
assert_expected_n(
  length(sps6_productivity_loss_corrected), config$expected_full_n,
  "Abstract 2 full-data N", config$strict_reproduction
)

item_frequency_rows <- function(item_data) {
  do.call(rbind, lapply(names(item_data), function(variable) {
    x <- item_data[[variable]]
    frequency <- table(factor(x, levels = 1:5), useNA = "no")
    valid_n <- sum(is.finite(x))
    data.frame(
      item = variable, value = 1:5, n = as.integer(frequency),
      percent_of_nonmissing = if (valid_n) 100 * as.integer(frequency) / valid_n else NA_real_,
      item_valid_n = valid_n, item_missing_n = sum(!is.finite(x)),
      item_min = min(x, na.rm = TRUE), item_max = max(x, na.rm = TRUE)
    )
  }))
}
write_csv_utf8(item_frequency_rows(sps_item_matrix),
               file.path(output_dir, "corrected_item_frequencies.csv"))

full_reliability <- reliability_rows(sps_item_matrix, "Full scored dataset")
full_alpha <- unique(full_reliability$cronbach_alpha)
assert_near(full_alpha, 0.91532817641736, 0.002,
            "Full-data SPS-6 Cronbach alpha", config$strict_reproduction)
if (any(full_reliability$corrected_item_total_correlation <= 0)) {
  stop("A full-data SPS-6 corrected item-total correlation is non-positive.", call. = FALSE)
}

full_descriptive <- score_descriptive_row(
  sps6_productivity_loss_corrected, "Full scored dataset"
)
full_descriptive$n_score_6 <- sum(sps6_productivity_loss_corrected == 6, na.rm = TRUE)
full_descriptive$n_score_30 <- sum(sps6_productivity_loss_corrected == 30, na.rm = TRUE)

paired <- is.finite(old_historical_score) & is.finite(sps6_productivity_loss_corrected)
score_comparison <- data.frame(
  paired_n = sum(paired),
  old_missing_n = sum(!is.finite(old_historical_score)),
  corrected_missing_n = sum(!is.finite(sps6_productivity_loss_corrected)),
  old_mean = mean(old_historical_score, na.rm = TRUE),
  corrected_mean = mean(sps6_productivity_loss_corrected, na.rm = TRUE),
  mean_difference_corrected_minus_old = mean(
    sps6_productivity_loss_corrected[paired] - old_historical_score[paired]
  ),
  old_sd = stats::sd(old_historical_score, na.rm = TRUE),
  corrected_sd = stats::sd(sps6_productivity_loss_corrected, na.rm = TRUE),
  pearson_correlation = stats::cor(old_historical_score[paired],
                                   sps6_productivity_loss_corrected[paired]),
  spearman_correlation = stats::cor(old_historical_score[paired],
                                    sps6_productivity_loss_corrected[paired], method = "spearman"),
  exact_match_n = sum(old_historical_score[paired] == sps6_productivity_loss_corrected[paired]),
  different_n = sum(old_historical_score[paired] != sps6_productivity_loss_corrected[paired])
)
write_csv_utf8(score_comparison, file.path(output_dir, "old_vs_corrected_score_comparison.csv"))

analysis_data <- data.frame(
  source_row = seq_len(nrow(source_data)),
  modified_kupperman_index = as_numeric_safe(source_data$B_weijuejingqizonghezheng_all),
  symptom_grade_stored = as_numeric_safe(source_data$B_wyjqzhz_fenji),
  sps6_productivity_loss_corrected = sps6_productivity_loss_corrected,
  age = as_numeric_safe(source_data$age),
  sex = factor(source_data$A_q2),
  marital_status = factor(source_data$A_q6),
  department = factor(source_data$A_q9),
  employment_type = factor(source_data$A_q10),
  night_shift_frequency = as_numeric_safe(source_data$C_q8),
  age_screen_40_to_55 = as_numeric_safe(source_data$B_q75),
  recent_hormone_use = as_numeric_safe(source_data$B_q76),
  bilateral_oophorectomy = as_numeric_safe(source_data$B_q77),
  hysterectomy = as_numeric_safe(source_data$B_q78),
  malignancy_history = as_numeric_safe(source_data$B_q79),
  chemotherapy_or_radiotherapy = as_numeric_safe(source_data$B_q80),
  reproductive_stage = as_numeric_safe(source_data$B_q81)
)

kupperman_item_matrix <- as.data.frame(lapply(source_data[kupperman_items], as_numeric_safe))
names(kupperman_item_matrix) <- kupperman_items
kupperman_weights <- setNames(rep(1, length(kupperman_items)), kupperman_items)
kupperman_weights["B_q83"] <- 4
kupperman_weights[c("B_q84", "B_q85", "B_q86", "B_q94", "B_q95")] <- 2
kupperman_recalculated <- as.vector(as.matrix(kupperman_item_matrix) %*% kupperman_weights)
kupperman_items_valid <- apply(
  kupperman_item_matrix, 1L, function(row) all(is.finite(row) & row %in% 0:3)
)

female_rows <- !is.na(analysis_data$sex) & analysis_data$sex == "2"
age_screen_yes <- is.finite(analysis_data$age_screen_40_to_55) &
  analysis_data$age_screen_40_to_55 == 1
exclusions_clear <- Reduce(`&`, lapply(
  analysis_data[c(
    "recent_hormone_use", "bilateral_oophorectomy", "hysterectomy",
    "malignancy_history", "chemotherapy_or_radiotherapy"
  )],
  function(x) is.finite(x) & x == 2
))
stage_valid <- is.finite(analysis_data$reproductive_stage) &
  analysis_data$reproductive_stage %in% 1:3
total_valid <- is.finite(analysis_data$modified_kupperman_index) &
  analysis_data$modified_kupperman_index >= 0 & analysis_data$modified_kupperman_index <= 63
grade_valid <- is.finite(analysis_data$symptom_grade_stored) &
  analysis_data$symptom_grade_stored %in% 1:4

stored_total_eligible <- female_rows & total_valid & grade_valid
routing_eligible <- female_rows & age_screen_yes & exclusions_clear &
  stage_valid & kupperman_items_valid
if (!identical(stored_total_eligible, routing_eligible)) {
  stop("Questionnaire routing does not select the same rows as valid Kupperman total/grade.",
       call. = FALSE)
}
eligible_data <- analysis_data[stored_total_eligible, , drop = FALSE]

kupperman_difference <- abs(
  kupperman_recalculated[stored_total_eligible] - eligible_data$modified_kupperman_index
)
kupperman_audit <- data.frame(
  eligible_n = nrow(eligible_data),
  comparable_n = sum(is.finite(kupperman_difference)),
  mismatch_n = sum(kupperman_difference > 1e-12, na.rm = TRUE),
  maximum_absolute_difference = max(kupperman_difference, na.rm = TRUE)
)
write_csv_utf8(kupperman_audit, file.path(output_dir, "corrected_kupperman_score_audit.csv"))
if (kupperman_audit$mismatch_n > 0L) {
  stop("Stored Kupperman total does not match the 13 weighted items.", call. = FALSE)
}

eligible_data$age_per_10_years <-
  (eligible_data$age - mean(eligible_data$age, na.rm = TRUE)) / 10
eligible_data$night_shift_group <- factor(
  eligible_data$night_shift_frequency,
  levels = c(-3, 1, 2, 3),
  labels = c("Day work/no current night shifts", "<=4/month", "5-9/month", ">=10/month")
)
eligible_data$symptom_grade_calculated <- cut(
  eligible_data$modified_kupperman_index,
  breaks = c(-Inf, 5, 15, 30, Inf),
  labels = c("Normal range", "Mild", "Moderate", "Severe"), right = TRUE
)
grade_audit <- data.frame(
  eligible_n = nrow(eligible_data),
  mismatch_n = sum(
    as.numeric(eligible_data$symptom_grade_calculated) != eligible_data$symptom_grade_stored,
    na.rm = TRUE
  )
)
write_csv_utf8(grade_audit, file.path(output_dir, "corrected_symptom_grade_audit.csv"))
if (grade_audit$mismatch_n > 0L) stop("Stored Kupperman grade is inconsistent.", call. = FALSE)

model_variables <- c(
  "sps6_productivity_loss_corrected", "modified_kupperman_index", "age_per_10_years",
  "marital_status", "department", "employment_type", "night_shift_group"
)
complete_data <- eligible_data[stats::complete.cases(eligible_data[model_variables]), , drop = FALSE]
assert_expected_n(
  nrow(complete_data), config$expected_n_2, "Abstract 2 complete-case N",
  config$strict_reproduction
)
if (config$strict_reproduction && !identical(complete_data$source_row, eligible_data$source_row)) {
  stop("Model covariates removed rows from the intended 8,540-person sample.", call. = FALSE)
}

analysis_reliability <- reliability_rows(
  sps_item_matrix[complete_data$source_row, , drop = FALSE], "Abstract 2 analysis sample"
)
analysis_alpha <- unique(analysis_reliability$cronbach_alpha)
assert_near(analysis_alpha, 0.880018176182215, 0.002,
            "Analysis-sample SPS-6 Cronbach alpha", config$strict_reproduction)
if (any(analysis_reliability$corrected_item_total_correlation <= 0)) {
  stop("An analysis-sample SPS-6 corrected item-total correlation is non-positive.", call. = FALSE)
}
write_csv_utf8(rbind(full_reliability, analysis_reliability),
               file.path(output_dir, "corrected_reliability.csv"))

analysis_descriptive <- score_descriptive_row(
  complete_data$sps6_productivity_loss_corrected, "Abstract 2 analysis sample"
)
analysis_descriptive$n_score_6 <- sum(complete_data$sps6_productivity_loss_corrected == 6)
analysis_descriptive$n_score_30 <- sum(complete_data$sps6_productivity_loss_corrected == 30)
write_csv_utf8(rbind(full_descriptive, analysis_descriptive),
               file.path(output_dir, "corrected_score_descriptive.csv"))

set.seed(42)
sampled_source_rows <- sample(complete_data$source_row, size = 20L, replace = FALSE)
manual_recalculation <- do.call(rbind, lapply(seq_along(sampled_source_rows), function(index) {
  source_row <- sampled_source_rows[[index]]
  values <- unlist(sps_item_matrix[source_row, ], use.names = FALSE)
  data.frame(
    audit_case = sprintf("case_%02d", index), source_row = source_row,
    D_q21_1 = values[[1L]], D_q21_2 = values[[2L]], D_q21_3 = values[[3L]],
    D_q21_4 = values[[4L]], D_q21_5 = values[[5L]], D_q21_6 = values[[6L]],
    manual_direct_sum = sum(values),
    corrected_score = sps6_productivity_loss_corrected[[source_row]],
    exact_match = identical(as.numeric(sum(values)),
                            as.numeric(sps6_productivity_loss_corrected[[source_row]]))
  )
}))
write_csv_utf8(manual_recalculation, file.path(output_dir, "corrected_manual_recalculation.csv"))
if (!all(manual_recalculation$exact_match)) stop("A manual SPS-6 recalculation failed.", call. = FALSE)

sample_flow <- data.frame(
  stage = c(
    "Input scored data", "Female nurses", "Yes to age 40-55 screening item",
    "Passed hormone/surgery/malignancy/treatment exclusions",
    "Valid reproductive stage", "All 13 Kupperman items valid",
    "Valid Kupperman total and stored grade", "Complete adjusted-model sample"
  ),
  n = c(
    nrow(analysis_data), sum(female_rows), sum(female_rows & age_screen_yes),
    sum(female_rows & age_screen_yes & exclusions_clear),
    sum(female_rows & age_screen_yes & exclusions_clear & stage_valid),
    sum(routing_eligible), sum(stored_total_eligible), nrow(complete_data)
  )
)
sample_flow$excluded_from_previous_stage <- c(NA_integer_, -diff(sample_flow$n))
write_csv_utf8(sample_flow, file.path(output_dir, "corrected_sample_flow.csv"))

stage_labels <- c(`1` = "Premenopausal", `2` = "Perimenopausal", `3` = "Postmenopausal")
stage_distribution <- as.data.frame(table(complete_data$reproductive_stage), stringsAsFactors = FALSE)
names(stage_distribution) <- c("stage_code", "n")
stage_distribution$stage <- unname(stage_labels[as.character(stage_distribution$stage_code)])
stage_distribution$percent <- 100 * stage_distribution$n / sum(stage_distribution$n)
stage_distribution <- stage_distribution[, c("stage_code", "stage", "n", "percent")]
write_csv_utf8(stage_distribution, file.path(output_dir, "reproductive_stage_distribution.csv"))

age_valid <- complete_data$age[is.finite(complete_data$age)]
age_quartiles <- stats::quantile(age_valid, c(0.25, 0.75), names = FALSE)
age_audit <- data.frame(
  n = length(age_valid), missing_n = sum(!is.finite(complete_data$age)),
  mean = mean(age_valid), sd = stats::sd(age_valid), median = stats::median(age_valid),
  q1 = age_quartiles[[1L]], q3 = age_quartiles[[2L]], min = min(age_valid), max = max(age_valid),
  outside_derived_age_40_to_55_n = sum(age_valid < 40 | age_valid > 55)
)
write_csv_utf8(age_audit, file.path(output_dir, "age_screen_consistency.csv"))

spline_knots <- as.numeric(stats::quantile(
  complete_data$modified_kupperman_index, c(0.05, 0.35, 0.65, 0.95), names = FALSE
))
if (length(unique(spline_knots)) != 4L) stop("Spline knots are not distinct.", call. = FALSE)
assert_near(spline_knots, c(0, 4, 11, 24), 1e-12,
            "Abstract 2 spline knots", config$strict_reproduction)
write_csv_utf8(data.frame(
  percentile = c(5, 35, 65, 95), knot = spline_knots
), file.path(output_dir, "corrected_spline_knots.csv"))

spline_model <- stats::lm(
  sps6_productivity_loss_corrected ~ rms::rcs(modified_kupperman_index, spline_knots) +
    age_per_10_years + marital_status + department + employment_type + night_shift_group,
  data = complete_data
)
spline_vcov <- sandwich::vcovHC(spline_model, type = "HC3")
coefficient_table <- robust_coefficient_table(spline_model, spline_vcov)
write_csv_utf8(coefficient_table, file.path(output_dir, "corrected_regression_coefficients.csv"))

all_spline_terms <- grep("rms::rcs", names(stats::coef(spline_model)), fixed = TRUE, value = TRUE)
nonlinear_terms <- grep("'", all_spline_terms, fixed = TRUE, value = TRUE)
association_tests <- rbind(
  robust_linear_hypothesis(
    spline_model, all_spline_terms, spline_vcov,
    "Overall association of modified Kupperman Index"
  ),
  robust_linear_hypothesis(
    spline_model, nonlinear_terms, spline_vcov,
    "Nonlinear component of modified Kupperman Index"
  )
)
write_csv_utf8(association_tests, file.path(output_dir, "corrected_spline_association_tests.csv"))

make_newdata <- function(index_values) {
  data.frame(
    modified_kupperman_index = index_values,
    age_per_10_years = 0,
    marital_status = factor(
      rep(most_common_level(complete_data$marital_status), length(index_values)),
      levels = levels(complete_data$marital_status)
    ),
    department = factor(rep(most_common_level(complete_data$department), length(index_values)),
                        levels = levels(complete_data$department)),
    employment_type = factor(
      rep(most_common_level(complete_data$employment_type), length(index_values)),
      levels = levels(complete_data$employment_type)
    ),
    night_shift_group = factor(
      rep(most_common_level(complete_data$night_shift_group), length(index_values)),
      levels = levels(complete_data$night_shift_group)
    )
  )
}

abstract_index_values <- c(0, 6, 16, 31)
abstract_newdata <- make_newdata(abstract_index_values)
adjusted_predictions <- robust_predictions(
  spline_model, spline_vcov, abstract_newdata, "modified_kupperman_index"
)
write_csv_utf8(adjusted_predictions, file.path(output_dir, "corrected_adjusted_predictions.csv"))

abstract_contrasts <- contrast_differences(
  spline_model, spline_vcov, abstract_newdata,
  exposure = "modified_kupperman_index", reference_value = 0
)
abstract_contrasts$comparison <- paste0(
  "Kupperman index ", abstract_contrasts$exposure_value, " versus 0"
)
abstract_contrasts <- abstract_contrasts[, c(
  "comparison", "exposure_value", "reference_value", "estimate", "robust_se",
  "ci_lower", "ci_upper"
)]
write_csv_utf8(abstract_contrasts, file.path(output_dir, "corrected_abstract_adjusted_contrasts.csv"))

headline_rows <- abstract_contrasts$exposure_value %in% c(6, 16, 31)
assert_near(
  abstract_contrasts$estimate[headline_rows], c(2.48875855, 5.70271815, 8.45810360), 0.02,
  "Abstract 2 corrected headline contrasts", config$strict_reproduction
)

curve_index_values <- seq(
  min(complete_data$modified_kupperman_index),
  max(complete_data$modified_kupperman_index), length.out = 250L
)
curve_data <- contrast_differences(
  spline_model, spline_vcov, make_newdata(curve_index_values),
  exposure = "modified_kupperman_index", reference_value = 0
)
write_csv_utf8(curve_data, file.path(output_dir, "corrected_adjusted_spline_curve.csv"))

spline_plot <- ggplot2::ggplot(curve_data, ggplot2::aes(x = exposure_value, y = estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                       fill = "#56B4E9", alpha = 0.25) +
  ggplot2::geom_line(color = "#0072B2", linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey40") +
  ggplot2::geom_vline(xintercept = c(6, 16, 31), linetype = 3, color = "grey55") +
  ggplot2::labs(
    x = "Modified Kupperman Index (0-63)",
    y = "Adjusted difference in corrected SPS-6 productivity-loss score (reference: 0)",
    title = "Kupperman symptoms and health-related productivity loss"
  ) +
  ggplot2::theme_classic(base_size = 10)
save_ggplot_pair(spline_plot, output_dir, "corrected_adjusted_spline_curve")

diagnostic_statistics <- model_diagnostic_statistics(spline_model)
write_csv_utf8(diagnostic_statistics,
               file.path(output_dir, "corrected_model_diagnostic_statistics.csv"))
write_csv_utf8(vif_table(spline_model), file.path(output_dir, "corrected_vif.csv"))
save_lm_diagnostics(spline_model, output_dir, "corrected_diagnostics_spline_model")
saveRDS(spline_model, file.path(output_dir, "corrected_spline_model.rds"))

contrast_at <- function(value) {
  abstract_contrasts[abstract_contrasts$exposure_value == value, , drop = FALSE]
}
c6 <- contrast_at(6)
c16 <- contrast_at(16)
c31 <- contrast_at(31)
nonlinearity_test <- association_tests[2L, , drop = FALSE]

result_lines <- c(
  "Abstract 2: Modified Kupperman symptoms and corrected SPS-6 productivity loss",
  sprintf("Complete-case N: %s", format(stats::nobs(spline_model), big.mark = ",")),
  sprintf("Corrected SPS-6: mean %.2f, SD %.2f, alpha %.3f.",
          analysis_descriptive$mean, analysis_descriptive$sd, analysis_alpha),
  sprintf("Spline nonlinearity: F = %.2f, p %s.",
          nonlinearity_test$statistic, format_p_value(nonlinearity_test$p_value)),
  sprintf("Kupperman 6 versus 0: adjusted difference %.2f (95%% CI %.2f to %.2f).",
          c6$estimate, c6$ci_lower, c6$ci_upper),
  sprintf("Kupperman 16 versus 0: adjusted difference %.2f (95%% CI %.2f to %.2f).",
          c16$estimate, c16$ci_lower, c16$ci_upper),
  sprintf("Kupperman 31 versus 0: adjusted difference %.2f (95%% CI %.2f to %.2f).",
          c31$estimate, c31$ci_lower, c31$ci_upper),
  "Higher corrected SPS-6 scores indicate greater health-related productivity loss."
)
write_text_utf8(result_lines, file.path(output_dir, "results_for_abstract.txt"))

eligibility_lines <- c(
  "# Abstract 2 eligibility audit",
  "",
  paste0("Input records: ", nrow(analysis_data)),
  paste0("Female nurses: ", sum(female_rows)),
  paste0("Final analysis sample: ", nrow(complete_data)),
  "",
  "The final sample passed the reproductive-health module routing/exclusions and had valid reproductive-stage and modified Kupperman data.",
  "It should not be described as entirely experiencing the menopausal transition, because the sample includes premenopausal, perimenopausal, and postmenopausal respondents.",
  paste0("Derived ages outside 40-55 despite a positive screening response: ",
         age_audit$outside_derived_age_40_to_55_n),
  "",
  "Reproductive-stage distribution:",
  paste0("- ", stage_distribution$stage, ": ", stage_distribution$n,
         " (", sprintf("%.1f", stage_distribution$percent), "%)")
)
write_text_utf8(eligibility_lines, file.path(output_dir, "menopause_eligibility_audit.md"))
write_analysis_manifest(
  config, output_dir, "02_menopause_productivity_corrected.R", stats::nobs(spline_model)
)
message(paste(result_lines, collapse = "\n"))
