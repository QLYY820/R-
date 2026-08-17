# Analysis: Multinomial regression for latent musculoskeletal symptom classes.
# Date: 2026-08-17
# Random seed: centralized in config/analysis_config.R
# R: 4.5.0
# Key packages: mice, nnet, broom, data.table, openxlsx, ggplot2

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))
}
project_root <- normalizePath(Sys.getenv("OR_MSK_PROJECT_ROOT"), winslash = "/", mustWork = TRUE)
source(Sys.getenv("OR_MSK_CONFIG"))
config <- build_analysis_config(
  project_root,
  mode = Sys.getenv("OR_MSK_MODE", "formal"),
  output_root = Sys.getenv("OR_MSK_OUTPUT_ROOT")
)
set.seed(config$seed)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5) {
  stop("Usage: Rscript scripts/05_multinomial_regression.R <analysis_data.rds> <assignments.csv> <class_labels.csv> <output_dir> <figures_dir>")
}

input_path <- args[1]
assignment_path <- args[2]
label_path <- args[3]
output_dir <- args[4]
figures_dir <- args[5]
for (path in c(input_path, assignment_path, label_path)) {
  if (!file.exists(path)) stop("Required regression input does not exist: ", path)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

for (pkg in c("mice", "nnet", "broom", "data.table", "openxlsx", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing package: ", pkg)
}

cat("R:", R.version.string, "\n")
for (pkg in c("mice", "nnet", "broom", "data.table", "openxlsx", "ggplot2")) {
  cat(pkg, ": ", as.character(utils::packageVersion(pkg)), "\n", sep = "")
}

data <- readRDS(input_path)
assignments <- data.table::fread(assignment_path, data.table = FALSE)
class_labels <- data.table::fread(label_path, data.table = FALSE)
class_labels <- class_labels[order(class_labels$presentation_order), ]

assignment_columns <- c(
  "row_id", "latent_class", "posterior_max",
  grep("^posterior_class_", names(assignments), value = TRUE)
)
analysis <- merge(data, assignments[assignment_columns], by = "row_id", all.x = TRUE, sort = FALSE)
analysis <- merge(analysis, class_labels, by = "latent_class", all.x = TRUE, sort = FALSE)
survey_year_path <- file.path(dirname(input_path), "operating_room_survey_year.csv")
if (!file.exists(survey_year_path)) stop("Missing survey-year reconciliation file: ", survey_year_path)
survey_year_data <- data.table::fread(survey_year_path, data.table = FALSE)
if (!all(c("row_id", "survey_year") %in% names(survey_year_data))) stop("Survey-year file must contain row_id and survey_year")
survey_year_matched <- survey_year_data$survey_year[match(analysis$row_id, survey_year_data$row_id)]
if ("survey_year" %in% names(analysis)) {
  if (
    anyNA(survey_year_matched) ||
      !identical(as.integer(analysis$survey_year), as.integer(survey_year_matched))
  ) {
    stop("Survey year in the analysis RDS does not reconcile with its companion file")
  }
} else {
  analysis$survey_year <- survey_year_matched
}
analysis <- analysis[order(analysis$row_id), ]
if (anyNA(analysis$survey_year)) stop("Survey year was not reconciled for every analysis row")
outcome_levels <- class_labels$class_label_en
analysis$class_outcome <- factor(analysis$class_label_en, levels = outcome_levels)
analysis$survey_year <- factor(analysis$survey_year, levels = sort(unique(analysis$survey_year)))
analysis$age10 <- analysis$age / 10
analysis$BMI5 <- analysis$BMI / 5
analysis$work_years5 <- analysis$work_years / 5
analysis$income_numeric <- as.numeric(analysis$income_level)
analysis$night_busy_score <- ifelse(is.na(analysis$night_busy), NA_real_, 6 - as.numeric(analysis$night_busy))

primary_predictors <- c(
  "any_night_shift", "overtime_weeks_last_month", "age10", "sex", "BMI5",
  "bachelor_or_above", "married", "permanent_employment",
  "supervisor_title_or_above", "administrative_role", "income_numeric", "survey_year"
)
auxiliary_predictors <- c("work_years5", "multisite_burden_12m", "night_shifts_per_month", "work_sleep_overlap", "night_busy_score")
mi_variables <- unique(c("class_outcome", primary_predictors, auxiliary_predictors))
mi_data <- droplevels(analysis[mi_variables])
numeric_variables <- names(mi_data)[vapply(mi_data, is.numeric, logical(1))]
for (variable in numeric_variables) {
  nonfinite <- !is.na(mi_data[[variable]]) & !is.finite(mi_data[[variable]])
  if (any(nonfinite)) mi_data[[variable]][nonfinite] <- NA_real_
}

bmi_predictor_candidates <- c(
  "class_outcome", "age10", "sex", "work_years5", "multisite_burden_12m",
  "bachelor_or_above", "married", "income_numeric", "survey_year"
)
is_usable_imputation_predictor <- function(x) {
  observed <- x[!is.na(x)]
  length(observed) == length(x) && length(unique(observed)) > 1L
}
bmi_predictor_used <- vapply(
  mi_data[bmi_predictor_candidates],
  is_usable_imputation_predictor,
  logical(1)
)
if (!any(bmi_predictor_used)) stop("No complete, nonconstant predictors are available for BMI imputation")

# R 4.1 can fail inside MICE when factor contrasts are generated implicitly.
# Build a finite, full-rank numeric design explicitly, while retaining the
# original factor variables for every regression model after imputation.
bmi_predictor_frame <- mi_data[names(bmi_predictor_used)[bmi_predictor_used]]
bmi_predictor_frame[] <- lapply(bmi_predictor_frame, function(x) {
  if (is.factor(x) || is.ordered(x)) factor(as.character(x)) else x
})
bmi_design_all <- stats::model.matrix(
  ~ .,
  data = bmi_predictor_frame,
  na.action = stats::na.pass
)
bmi_design_all <- bmi_design_all[, colnames(bmi_design_all) != "(Intercept)", drop = FALSE]
storage.mode(bmi_design_all) <- "double"
finite_nonconstant <- vapply(seq_len(ncol(bmi_design_all)), function(index) {
  values <- bmi_design_all[, index]
  all(is.finite(values)) && length(unique(values)) > 1L
}, logical(1))
bmi_design <- bmi_design_all[, finite_nonconstant, drop = FALSE]
if (!ncol(bmi_design)) stop("BMI imputation dummy design has no finite, nonconstant columns")
design_with_intercept <- cbind(.intercept = 1, bmi_design)
design_qr <- qr(design_with_intercept, tol = 1e-07, LAPACK = FALSE)
full_rank_with_intercept <- design_qr$pivot[seq_len(design_qr$rank)]
full_rank_columns <- sort(full_rank_with_intercept[full_rank_with_intercept > 1L] - 1L)
bmi_design <- bmi_design[, full_rank_columns, drop = FALSE]
colnames(bmi_design) <- make.names(colnames(bmi_design), unique = TRUE)

imputation_data <- data.frame(BMI5 = mi_data$BMI5, bmi_design, check.names = FALSE)
method <- mice::make.method(imputation_data)
method[] <- ""
method["BMI5"] <- "pmm"
predictor_matrix <- mice::make.predictorMatrix(imputation_data)
predictor_matrix[,] <- 0
predictor_matrix["BMI5", setdiff(names(imputation_data), "BMI5")] <- 1

mi_audit <- data.frame(
  variable = names(mi_data),
  storage_class = vapply(mi_data, function(x) paste(class(x), collapse = "/"), character(1)),
  missing_n = vapply(mi_data, function(x) sum(is.na(x)), numeric(1)),
  nonfinite_n = vapply(mi_data, function(x) {
    if (is.numeric(x)) sum(!is.na(x) & !is.finite(x)) else 0
  }, numeric(1)),
  unique_nonmissing = vapply(mi_data, function(x) length(unique(x[!is.na(x)])), numeric(1)),
  bmi_imputation_predictor = names(mi_data) %in% names(bmi_predictor_used)[bmi_predictor_used],
  stringsAsFactors = FALSE
)
data.table::fwrite(mi_audit, file.path(output_dir, "multinomial_mi_variable_audit.csv"), bom = TRUE)
data.table::fwrite(
  data.frame(
    dummy_column = colnames(bmi_design),
    finite = vapply(seq_len(ncol(bmi_design)), function(index) all(is.finite(bmi_design[, index])), logical(1)),
    unique_values = vapply(seq_len(ncol(bmi_design)), function(index) length(unique(bmi_design[, index])), numeric(1)),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "multinomial_mi_numeric_design_audit.csv"),
  bom = TRUE
)
cat(
  "BMI imputation predictors:",
  paste(names(bmi_predictor_used)[bmi_predictor_used], collapse = ", "),
  "\n"
)
cat("BMI numeric dummy predictors retained:", ncol(bmi_design), "\n")
excluded_bmi_predictors <- names(bmi_predictor_used)[!bmi_predictor_used]
if (length(excluded_bmi_predictors)) {
  cat("BMI predictor candidates excluded for missingness/zero variance:", paste(excluded_bmi_predictors, collapse = ", "), "\n")
}

cat(
  "Running MICE with", config$runtime$mice_m, "imputed datasets and",
  config$runtime$mice_maxit, "iterations; BMI missing n =", sum(is.na(mi_data$BMI5)), "\n"
)
imputation <- mice::mice(
  imputation_data,
  m = config$runtime$mice_m,
  maxit = config$runtime$mice_maxit,
  method = method,
  predictorMatrix = predictor_matrix,
  seed = config$seed,
  printFlag = FALSE
)
saveRDS(imputation, file.path(output_dir, "multinomial_mice_imputation.rds"))
imputed_sets <- lapply(seq_len(imputation$m), function(index) {
  completed_numeric <- mice::complete(imputation, index)
  completed_analysis <- mi_data
  completed_analysis$BMI5 <- completed_numeric$BMI5
  completed_analysis
})

linear_primary_formula <- stats::as.formula(paste("class_outcome ~", paste(primary_predictors, collapse = " + ")))
primary_formula <- class_outcome ~ any_night_shift + overtime_weeks_last_month +
  splines::ns(age10, df = 3) + sex + BMI5 + bachelor_or_above + married +
  permanent_employment + supervisor_title_or_above + administrative_role + factor(income_numeric) + survey_year
work_year_formula <- class_outcome ~ any_night_shift + overtime_weeks_last_month +
  work_years5 + sex + BMI5 + bachelor_or_above + married + permanent_employment +
  supervisor_title_or_above + administrative_role + factor(income_numeric) + survey_year
night_formula <- class_outcome ~ night_shifts_per_month + work_sleep_overlap + night_busy_score +
  overtime_weeks_last_month + splines::ns(age10, df = 3) + sex + BMI5 + bachelor_or_above + married +
  permanent_employment + supervisor_title_or_above + administrative_role + factor(income_numeric) + survey_year
linear_night_formula <- class_outcome ~ night_shifts_per_month + work_sleep_overlap + night_busy_score +
  overtime_weeks_last_month + age10 + sex + BMI5 + bachelor_or_above + married +
  permanent_employment + supervisor_title_or_above + administrative_role + income_numeric + survey_year

fit_model_list <- function(datasets, formula, subset_function = NULL, outcome_override = NULL) {
  fits <- vector("list", length(datasets))
  tidy_rows <- vector("list", length(datasets))
  for (index in seq_along(datasets)) {
    dataset <- datasets[[index]]
    if (!is.null(outcome_override)) dataset$class_outcome <- outcome_override(index, dataset)
    if (!is.null(subset_function)) dataset <- subset_function(dataset)
    dataset$class_outcome <- factor(dataset$class_outcome, levels = outcome_levels)
    fit <- nnet::multinom(formula, data = dataset, Hess = TRUE, trace = FALSE, maxit = 1000)
    fits[[index]] <- fit
    tidy <- broom::tidy(fit)
    tidy$imputation <- index
    tidy_rows[[index]] <- tidy
  }
  list(fits = fits, tidy = do.call(rbind, tidy_rows))
}

pool_multinom <- function(tidy_data) {
  data_table <- data.table::as.data.table(tidy_data)
  pooled <- data_table[, {
    m <- .N
    estimate_pooled <- mean(estimate)
    within_variance <- mean(std.error^2)
    between_variance <- if (m > 1) stats::var(estimate) else 0
    total_variance <- within_variance + (1 + 1 / m) * between_variance
    relative_increase <- if (within_variance > 0) ((1 + 1 / m) * between_variance) / within_variance else 0
    degrees_freedom <- if (between_variance <= .Machine$double.eps) Inf else (m - 1) * (1 + 1 / relative_increase)^2
    standard_error <- sqrt(total_variance)
    critical <- if (is.finite(degrees_freedom)) stats::qt(0.975, degrees_freedom) else stats::qnorm(0.975)
    statistic <- estimate_pooled / standard_error
    p_value <- if (is.finite(degrees_freedom)) 2 * stats::pt(abs(statistic), degrees_freedom, lower.tail = FALSE) else 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    list(
      estimate = estimate_pooled,
      std_error = standard_error,
      degrees_freedom = degrees_freedom,
      p_value = p_value,
      OR = exp(estimate_pooled),
      ci_lower = exp(estimate_pooled - critical * standard_error),
      ci_upper = exp(estimate_pooled + critical * standard_error),
      within_variance = within_variance,
      between_variance = between_variance
    )
  }, by = .(y.level, term)]
  as.data.frame(pooled)
}

primary_result <- fit_model_list(imputed_sets, primary_formula)
primary_pooled <- pool_multinom(primary_result$tidy)
primary_pooled$model <- "Primary_MICE_age_adjusted"
primary_exposure_terms <- c("any_night_shiftYes", "overtime_weeks_last_month")
exposure_rows <- primary_pooled$term %in% primary_exposure_terms
primary_pooled$p_adjusted_BH_primary_exposures <- NA_real_
primary_pooled$p_adjusted_BH_primary_exposures[exposure_rows] <- stats::p.adjust(primary_pooled$p_value[exposure_rows], method = "BH")

univariable_any_night <- pool_multinom(fit_model_list(imputed_sets, class_outcome ~ any_night_shift)$tidy)
univariable_any_night$model <- "Univariable_any_night_shift"
univariable_overtime <- pool_multinom(fit_model_list(imputed_sets, class_outcome ~ overtime_weeks_last_month)$tidy)
univariable_overtime$model <- "Univariable_overtime"
univariable_pooled <- rbind(univariable_any_night, univariable_overtime)

work_year_result <- fit_model_list(imputed_sets, work_year_formula)
work_year_pooled <- pool_multinom(work_year_result$tidy)
work_year_pooled$model <- "Sensitivity_MICE_work_years_adjusted"

night_subset <- function(dataset) dataset[dataset$any_night_shift == "Yes", , drop = FALSE]
night_result <- fit_model_list(imputed_sets, night_formula, subset_function = night_subset)
night_pooled <- pool_multinom(night_result$tidy)
night_pooled$model <- "Night_worker_secondary"
night_terms <- c("night_shifts_per_month5_to_9", "night_shifts_per_month10_or_more", "work_sleep_overlapYes", "night_busy_score", "overtime_weeks_last_month")
night_pooled$p_adjusted_BH_night_work_factors <- NA_real_
night_exposure_rows <- night_pooled$term %in% night_terms
night_pooled$p_adjusted_BH_night_work_factors[night_exposure_rows] <- stats::p.adjust(night_pooled$p_value[night_exposure_rows], method = "BH")

complete_case <- analysis[stats::complete.cases(analysis[c("class_outcome", primary_predictors)]), ]
complete_case_fit <- nnet::multinom(primary_formula, data = complete_case, Hess = TRUE, trace = FALSE, maxit = 1000)
complete_case_tidy <- broom::tidy(complete_case_fit)
complete_case_tidy$OR <- exp(complete_case_tidy$estimate)
complete_case_tidy$ci_lower <- exp(complete_case_tidy$estimate - stats::qnorm(0.975) * complete_case_tidy$std.error)
complete_case_tidy$ci_upper <- exp(complete_case_tidy$estimate + stats::qnorm(0.975) * complete_case_tidy$std.error)
complete_case_tidy$model <- "Sensitivity_complete_case"

posterior_columns <- paste0("posterior_class_", seq_len(nrow(class_labels)))
posterior_matrix <- as.matrix(analysis[posterior_columns])
if (length(imputed_sets) != config$runtime$posterior_draws) {
  stop("The configured posterior draw count must equal the number of imputed datasets")
}
draw_outcome <- function(index, dataset) {
  set.seed(config$seed + 1000L + index)
  drawn_class <- apply(posterior_matrix, 1, function(probability) sample.int(nrow(class_labels), size = 1, prob = probability))
  drawn_label <- class_labels$class_label_en[match(drawn_class, class_labels$latent_class)]
  factor(drawn_label, levels = outcome_levels)
}
pseudo_result <- fit_model_list(imputed_sets, primary_formula, outcome_override = draw_outcome)
pseudo_pooled <- pool_multinom(pseudo_result$tidy)
pseudo_pooled$model <- "Sensitivity_posterior_class_draws"

calculate_vif <- function(dataset, formula) {
  complete <- dataset[stats::complete.cases(dataset[all.vars(formula)]), ]
  design <- stats::model.matrix(formula, data = complete)[, -1, drop = FALSE]
  values <- vapply(seq_len(ncol(design)), function(index) {
    if (ncol(design) == 1) return(1)
    model <- stats::lm(design[, index] ~ design[, -index, drop = FALSE])
    1 / (1 - summary(model)$r.squared)
  }, numeric(1))
  data.frame(term = colnames(design), VIF = values, stringsAsFactors = FALSE)
}
vif_primary <- calculate_vif(imputed_sets[[1]], linear_primary_formula)
vif_night <- calculate_vif(night_subset(imputed_sets[[1]]), linear_night_formula)

linearity_complete <- analysis[stats::complete.cases(analysis[c("class_outcome", primary_predictors)]), ]
linear_fit <- nnet::multinom(linear_primary_formula, data = linearity_complete, trace = FALSE, maxit = 1000)
linearity_test <- do.call(rbind, lapply(c("age10", "BMI5", "overtime_weeks_last_month", "income_numeric"), function(variable) {
  alternative_formula <- stats::update(
    linear_primary_formula,
    stats::as.formula(paste(". ~ . -", variable, "+ splines::ns(", variable, ", df = 3)"))
  )
  alternative_fit <- nnet::multinom(alternative_formula, data = linearity_complete, trace = FALSE, maxit = 1000)
  comparison <- stats::anova(linear_fit, alternative_fit, test = "Chisq")
  data.frame(
    variable = variable,
    degrees_freedom = comparison[2, 5],
    likelihood_ratio = comparison[2, 6],
    p_value = comparison[2, 7],
    final_handling = ifelse(variable %in% c("age10", "income_numeric"), ifelse(variable == "age10", "Natural spline, 3 df", "Categorical factor"), "Linear"),
    stringsAsFactors = FALSE
  )
}))

model_fit_rows <- do.call(rbind, lapply(seq_along(primary_result$fits), function(index) {
  fit <- primary_result$fits[[index]]
  null_fit <- nnet::multinom(class_outcome ~ 1, data = imputed_sets[[index]], trace = FALSE)
  predicted <- predict(fit, type = "class")
  data.frame(
    imputation = index,
    n = nrow(imputed_sets[[index]]),
    log_likelihood = as.numeric(stats::logLik(fit)),
    AIC = stats::AIC(fit),
    McFadden_R2 = 1 - as.numeric(stats::logLik(fit)) / as.numeric(stats::logLik(null_fit)),
    modal_accuracy = mean(predicted == imputed_sets[[index]]$class_outcome)
  )
}))
model_fit_summary <- data.frame(
  metric = c("n", "AIC", "McFadden_R2", "modal_accuracy"),
  estimate = c(model_fit_rows$n[1], mean(model_fit_rows$AIC), mean(model_fit_rows$McFadden_R2), mean(model_fit_rows$modal_accuracy))
)
model_matrix_columns <- ncol(stats::model.matrix(primary_formula, data = imputed_sets[[1]])) - 1
class_counts <- table(analysis$class_outcome)
epv_table <- data.frame(
  class = names(class_counts),
  n = as.integer(class_counts),
  predictors_per_logit = model_matrix_columns,
  observations_per_parameter = as.integer(class_counts) / model_matrix_columns
)

marginal_contrast <- function(fits, datasets, variable, low_value, high_value, label) {
  rows <- vector("list", length(fits))
  for (index in seq_along(fits)) {
    low_data <- datasets[[index]]
    high_data <- datasets[[index]]
    if (is.factor(low_data[[variable]])) {
      low_data[[variable]] <- factor(low_value, levels = levels(low_data[[variable]]))
      high_data[[variable]] <- factor(high_value, levels = levels(high_data[[variable]]))
    } else {
      low_data[[variable]] <- low_value
      high_data[[variable]] <- high_value
    }
    low_probability <- colMeans(predict(fits[[index]], newdata = low_data, type = "probs"))
    high_probability <- colMeans(predict(fits[[index]], newdata = high_data, type = "probs"))
    rows[[index]] <- data.frame(imputation = index, class = names(low_probability), risk_low = low_probability, risk_high = high_probability, risk_difference = high_probability - low_probability)
  }
  all_rows <- do.call(rbind, rows)
  summary <- aggregate(cbind(risk_low, risk_high, risk_difference) ~ class, all_rows, mean)
  summary$contrast <- label
  summary$number_needed_to_harm_or_benefit <- ifelse(abs(summary$risk_difference) > 0, 1 / abs(summary$risk_difference), NA_real_)
  summary$direction <- ifelse(summary$risk_difference > 0, "NNH", "NNT-like benefit")
  summary
}
marginal_any_night <- marginal_contrast(primary_result$fits, imputed_sets, "any_night_shift", "No", "Yes", "Any night shift: Yes vs No")
marginal_overtime <- marginal_contrast(primary_result$fits, imputed_sets, "overtime_weeks_last_month", 0, 4, "Weeks >40 h: 4 vs 0")
marginal_effects <- rbind(marginal_any_night, marginal_overtime)

data.table::fwrite(primary_pooled, file.path(output_dir, "multinomial_primary_mice.csv"), bom = TRUE)
data.table::fwrite(univariable_pooled, file.path(output_dir, "multinomial_univariable.csv"), bom = TRUE)
data.table::fwrite(work_year_pooled, file.path(output_dir, "multinomial_work_year_sensitivity.csv"), bom = TRUE)
data.table::fwrite(night_pooled, file.path(output_dir, "multinomial_night_worker_secondary.csv"), bom = TRUE)
data.table::fwrite(complete_case_tidy, file.path(output_dir, "multinomial_complete_case_sensitivity.csv"), bom = TRUE)
data.table::fwrite(pseudo_pooled, file.path(output_dir, "multinomial_posterior_draw_sensitivity.csv"), bom = TRUE)
data.table::fwrite(vif_primary, file.path(output_dir, "multinomial_vif_primary.csv"), bom = TRUE)
data.table::fwrite(vif_night, file.path(output_dir, "multinomial_vif_night_worker.csv"), bom = TRUE)
data.table::fwrite(linearity_test, file.path(output_dir, "multinomial_linearity_test.csv"), bom = TRUE)
data.table::fwrite(model_fit_rows, file.path(output_dir, "multinomial_model_fit_by_imputation.csv"), bom = TRUE)
data.table::fwrite(model_fit_summary, file.path(output_dir, "multinomial_model_fit_summary.csv"), bom = TRUE)
data.table::fwrite(epv_table, file.path(output_dir, "multinomial_epv.csv"), bom = TRUE)
data.table::fwrite(marginal_effects, file.path(output_dir, "multinomial_marginal_effects.csv"), bom = TRUE)

workbook <- openxlsx::createWorkbook()
sheet_data <- list(
  primary_MICE = primary_pooled,
  univariable = univariable_pooled,
  work_year_sensitivity = work_year_pooled,
  night_worker_secondary = night_pooled,
  complete_case = complete_case_tidy,
  posterior_draws = pseudo_pooled,
  VIF_primary = vif_primary,
  VIF_night = vif_night,
  linearity_test = linearity_test,
  model_fit = model_fit_summary,
  EPV = epv_table,
  marginal_effects = marginal_effects
)
for (sheet in names(sheet_data)) {
  openxlsx::addWorksheet(workbook, sheet)
  openxlsx::writeData(workbook, sheet, sheet_data[[sheet]])
}
openxlsx::saveWorkbook(workbook, file.path(output_dir, "multinomial_regression_results.xlsx"), overwrite = TRUE)

forest_data <- primary_pooled[primary_pooled$term %in% primary_exposure_terms, ]
forest_data$y.level <- factor(forest_data$y.level, levels = rev(outcome_levels[-1]))
forest_data$term_label <- factor(forest_data$term, levels = primary_exposure_terms, labels = c("Any night shift (yes vs no)", "Each additional week >40 h"))
forest_plot <- ggplot2::ggplot(forest_data, ggplot2::aes(x = OR, y = y.level, color = term_label)) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
    width = 0.18,
    orientation = "y",
    position = ggplot2::position_dodge(width = 0.5)
  ) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5), size = 2) +
  ggplot2::scale_x_log10() +
  ggplot2::labs(x = "Adjusted odds ratio (95% CI), reference: Low burden", y = NULL, color = NULL, title = "Work-time factors associated with latent symptom classes") +
  ggplot2::theme_minimal(base_family = "Arial", base_size = 9) + ggplot2::theme(legend.position = "bottom")
png_path <- file.path(figures_dir, "multinomial_work_factor_forest.png")
png_type <- if (capabilities("cairo")) "cairo" else if (.Platform$OS.type == "windows") "windows" else "Xlib"
grDevices::png(png_path, width = 2100, height = 1440, res = 300, type = png_type)
print(forest_plot)
grDevices::dev.off()
if (!file.exists(png_path) || file.info(png_path)$size <= 0L) {
  stop("Failed to create a non-empty multinomial forest-plot PNG.")
}
ggplot2::ggsave(file.path(figures_dir, "multinomial_work_factor_forest.pdf"), forest_plot, width = 7, height = 4.8, device = grDevices::cairo_pdf)

saveRDS(list(primary = primary_result$fits, work_year = work_year_result$fits, night_worker = night_result$fits, pseudo_draw = pseudo_result$fits), file.path(output_dir, "multinomial_models.rds"))

cat("\nPrimary exposure results (MICE):\n")
print(primary_pooled[primary_pooled$term %in% primary_exposure_terms, ])
cat("\nNight-worker secondary work-factor results:\n")
print(night_pooled[night_pooled$term %in% night_terms, ])
cat("\nPrimary-model VIF:\n")
print(vif_primary)
cat("\nLinearity test:\n")
print(linearity_test)
cat("\nModel fit summary:\n")
print(model_fit_summary)
cat("\nMarginal effects:\n")
print(marginal_effects)
cat("STEP_COMPLETE=multinomial_regression\n")
