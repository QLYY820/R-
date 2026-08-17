suppressPackageStartupMessages({
  library(data.table)
  library(psych)
  library(sandwich)
  library(lmtest)
  library(ggplot2)
  library(openxlsx)
  library(mice)
})

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(CONFIG$seed)

out_path <- function(...) file.path(RUN_ROOT, ...)
write_csv <- function(x, ...) {
  data.table::fwrite(as.data.table(x), out_path(...), bom = TRUE, na = "")
}

log_file <- out_path("08_logs", "analysis_run_log.txt")
if (file.exists(log_file)) file.remove(log_file)
log_msg <- function(level, ...) {
  z <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " [", level, "] ", paste0(..., collapse = "")
  )
  message(z)
  cat(z, "\n", file = log_file, append = TRUE)
}

numeric_clean <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))

fmt_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

collapse_rare <- function(x, minimum_n = 30L) {
  y <- trimws(as.character(x))
  y[y == "" | is.na(y)] <- NA_character_
  tab <- table(y, useNA = "no")
  rare <- names(tab)[tab < minimum_n]
  y[y %in% rare] <- "Other"
  factor(y)
}

safe_alpha <- function(x) {
  x <- as.data.frame(x)
  if (ncol(x) < 2L || sum(complete.cases(x)) < 20L) return(NA_real_)
  suppressWarnings(
    psych::alpha(x, check.keys = FALSE, warnings = FALSE)$total$raw_alpha
  )
}

safe_omega <- function(x) {
  x <- as.data.frame(x)
  x <- x[complete.cases(x), , drop = FALSE]
  if (ncol(x) < 3L || nrow(x) < 100L) return(NA_real_)
  suppressWarnings(tryCatch(
    psych::omega(x, nfactors = 1L, plot = FALSE)$omega.tot,
    error = function(e) NA_real_
  ))
}

agreement_row <- function(source, recalculated, scale, formula_text) {
  ok <- is.finite(source) & is.finite(recalculated)
  if (!any(ok)) {
    return(data.table(
      scale = scale,
      calculation = formula_text,
      compared_n = 0L,
      exact_agreement_percent = NA_real_,
      maximum_absolute_difference = NA_real_,
      correlation = NA_real_
    ))
  }
  data.table(
    scale = scale,
    calculation = formula_text,
    compared_n = sum(ok),
    exact_agreement_percent = 100 * mean(source[ok] == recalculated[ok]),
    maximum_absolute_difference = max(abs(source[ok] - recalculated[ok])),
    correlation = cor(source[ok], recalculated[ok])
  )
}

robust_coef_table <- function(fit) {
  vc <- sandwich::vcovHC(fit, type = "HC3")
  b <- coef(fit)
  terms_kept <- intersect(names(b), rownames(vc))
  b <- b[terms_kept]
  vc <- vc[terms_kept, terms_kept, drop = FALSE]
  se <- sqrt(diag(vc))
  z <- b / se
  data.table(
    term = terms_kept,
    estimate = as.numeric(b),
    robust_se = as.numeric(se),
    ci_lower = as.numeric(b - qnorm(0.975) * se),
    ci_upper = as.numeric(b + qnorm(0.975) * se),
    p_value = as.numeric(2 * pnorm(abs(z), lower.tail = FALSE))
  )
}

vif_columns <- function(fit) {
  mm <- model.matrix(fit)
  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  if (!ncol(mm)) return(data.table(term = character(), VIF = numeric()))
  varying <- apply(mm, 2L, function(z) length(unique(z)) > 1L)
  mm <- mm[, varying, drop = FALSE]
  if (ncol(mm) == 1L) return(data.table(term = colnames(mm), VIF = 1))
  vals <- vapply(seq_len(ncol(mm)), function(j) {
    y <- mm[, j]
    x <- mm[, -j, drop = FALSE]
    r2 <- tryCatch(summary(lm(y ~ x))$r.squared, error = function(e) NA_real_)
    if (!is.finite(r2) || r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))
  data.table(term = colnames(mm), VIF = vals)
}

wilson_interval <- function(events, total, conf = 0.95) {
  if (!is.finite(total) || total <= 0L) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- events / total
  den <- 1 + z^2 / total
  center <- (p + z^2 / (2 * total)) / den
  half <- z * sqrt(p * (1 - p) / total + z^2 / (4 * total^2)) / den
  c(center - half, center + half)
}

standardized_effect <- function(estimate, exposure, outcome) {
  estimate * sd(exposure, na.rm = TRUE) / sd(outcome, na.rm = TRUE)
}

save_diagnostic_plot <- function(fit, stem) {
  png(
    out_path("06_figures", paste0(stem, ".png")),
    width = 2400, height = 1800, res = 300
  )
  par(mfrow = c(2, 2))
  plot(fit)
  dev.off()
  pdf(out_path("06_figures", paste0(stem, ".pdf")), width = 8, height = 6)
  par(mfrow = c(2, 2))
  plot(fit)
  dev.off()
}

log_msg("INFO", "Pipeline started; version=", CONFIG$pipeline_version,
        "; seed=", CONFIG$seed)

vars <- CONFIG$variables
requested <- unique(c(
  vars$id, vars$sex, unname(vars$psqi_components), vars$psqi_total,
  vars$psqi_poor, vars$burnout_items, vars$burnout_total,
  vars$turnover_items, vars$turnover_total, unname(vars$covariates)
))

if (grepl("\\.rds$", DATA_PATH, ignore.case = TRUE)) {
  input_all <- readRDS(DATA_PATH)
  if (!is.data.frame(input_all)) {
    stop("The configured RDS does not contain a data.frame-like object.")
  }
  header <- names(input_all)
} else {
  input_all <- NULL
  header <- names(fread(DATA_PATH, nrows = 0L, encoding = "UTF-8"))
}
missing_vars <- setdiff(requested, header)
if (length(missing_vars)) {
  stop("Required variables missing: ", paste(missing_vars, collapse = ", "))
}

if (is.null(input_all)) {
  dt <- fread(
    DATA_PATH,
    select = requested,
    na.strings = c("", "NA"),
    encoding = "UTF-8",
    showProgress = FALSE
  )
} else {
  dt <- as.data.table(input_all[, requested, drop = FALSE])
  rm(input_all)
  invisible(gc())
}

if (nrow(dt) != CONFIG$expected_source_n) {
  stop(
    "Source record count mismatch: observed ", nrow(dt),
    ", expected ", CONFIG$expected_source_n
  )
}

numeric_fields <- setdiff(
  requested,
  c(vars$id, vars$covariates[["chronic"]])
)
for (v in numeric_fields) set(dt, j = v, value = numeric_clean(dt[[v]]))

id_value <- trimws(as.character(dt[[vars$id]]))
id_audit <- data.table(
  metric = c(
    "source_records", "unique_nonmissing_ids", "duplicated_id_records",
    "missing_ids"
  ),
  value = c(
    nrow(dt), uniqueN(id_value[id_value != "" & !is.na(id_value)]),
    sum(duplicated(id_value) & id_value != "" & !is.na(id_value)),
    sum(is.na(id_value) | id_value == "")
  )
)
write_csv(id_audit, "00_audit", "id_audit.csv")

sex_distribution <- dt[, .N, by = .(A_q2 = get(vars$sex))][order(A_q2)]
sex_distribution[, percent := 100 * N / sum(N)]
write_csv(sex_distribution, "00_audit", "sex_distribution.csv")

male <- copy(dt[get(vars$sex) == CONFIG$male_code])
if (!nrow(male)) stop("No male records found using configured sex code.")
if (uniqueN(male[[vars$id]]) != nrow(male)) {
  stop("Male records are not unique by ID; analysis stopped.")
}

psqi_source <- unname(vars$psqi_components)
psqi_short <- names(vars$psqi_components)
setnames(male, psqi_source, psqi_short)
for (v in psqi_short) male[!get(v) %in% 0:3, (v) := NA_real_]
male[, psqi_complete := complete.cases(.SD), .SDcols = psqi_short]
male[, psqi_recalc := fifelse(psqi_complete, rowSums(.SD), NA_real_),
     .SDcols = psqi_short]

burn <- as.data.table(male[, vars$burnout_items, with = FALSE])
for (v in names(burn)) burn[!get(v) %in% 0:6, (v) := NA_real_]
ee_items <- paste0("F_q3_", c(1, 2, 3, 6, 8, 13, 14, 16, 20))
dp_items <- paste0("F_q3_", c(5, 10, 11, 15, 22))
pa_items <- paste0("F_q3_", c(4, 7, 9, 12, 17, 18, 19, 21))
burn_complete <- complete.cases(burn)
burn_matrix <- as.matrix(burn)
mbi_all_same <- apply(burn_matrix, 1L, function(x) {
  all(is.finite(x)) && length(unique(x)) == 1L
})
mbi_within_person_sd <- apply(burn_matrix, 1L, sd, na.rm = TRUE)
ee_recalc <- dp_recalc <- pa_recalc <- rep(NA_real_, nrow(male))
ee_recalc[burn_complete] <- rowSums(
  burn[burn_complete, ee_items, with = FALSE]
)
dp_recalc[burn_complete] <- rowSums(
  burn[burn_complete, dp_items, with = FALSE]
)
pa_recalc[burn_complete] <- rowSums(
  burn[burn_complete, pa_items, with = FALSE]
)
male[, `:=`(
  ee_recalc = ee_recalc,
  dp_recalc = dp_recalc,
  pa_recalc = pa_recalc,
  mbi_all_same = mbi_all_same,
  mbi_within_person_sd = mbi_within_person_sd
)]

mbi_response_quality <- data.table(
  metric = c(
    "MBI_complete_records", "All_22_items_identical",
    "All_22_items_identical_percent", "Within_person_SD_zero"
  ),
  value = c(
    sum(burn_complete), sum(mbi_all_same),
    100 * mean(mbi_all_same), sum(mbi_within_person_sd == 0, na.rm = TRUE)
  )
)
write_csv(mbi_response_quality, "00_audit", "MBI_response_quality_audit.csv")

mbi_straightline_distribution <- data.table(
  response_value = 0:6,
  n = vapply(0:6, function(value) {
    sum(mbi_all_same & burn_matrix[, 1L] == value, na.rm = TRUE)
  }, integer(1L))
)
mbi_straightline_distribution[, percent_of_male := 100 * n / nrow(male)]
write_csv(
  mbi_straightline_distribution,
  "00_audit", "MBI_straightline_distribution.csv"
)

turn <- as.data.table(male[, vars$turnover_items, with = FALSE])
for (v in names(turn)) turn[!get(v) %in% 1:4, (v) := NA_real_]
turn_complete <- complete.cases(turn)
turnover_recalc <- rep(NA_real_, nrow(male))
turnover_recalc[turn_complete] <- rowSums(turn[turn_complete])
male[, turnover_recalc := turnover_recalc]

score_audit <- rbindlist(list(
  agreement_row(
    male[[vars$psqi_total]], male$psqi_recalc,
    "PSQI total", "sum of 7 PSQI components"
  ),
  agreement_row(
    male[[vars$turnover_total]], male$turnover_recalc,
    "Turnover intention", "sum of 6 items"
  )
))
write_csv(score_audit, "00_audit", "scale_scoring_agreement.csv")

legacy_burnout <- male$ee_recalc + male$dp_recalc + (48 - male$pa_recalc)
legacy_burnout_audit <- agreement_row(
  male[[vars$burnout_total]], legacy_burnout,
  "Legacy composite excluded from inference",
  "EE + DP + (48 - PA); retained only to document the source field"
)
legacy_burnout_audit[, reason_excluded := paste(
  "The MBI dimensions are analysed separately;",
  "the composite is not an official MBI total score."
)]
write_csv(
  legacy_burnout_audit,
  "00_audit", "legacy_burnout_composite_audit.csv"
)

component_distribution <- rbindlist(lapply(psqi_short, function(v) {
  a <- as.data.table(table(factor(male[[v]], levels = 0:3), useNA = "no"))
  setnames(a, c("score", "n"))
  a[, `:=`(
    component = v,
    percent = 100 * n / sum(n)
  )]
  a[]
}))
write_csv(component_distribution, "00_audit", "PSQI_component_distribution.csv")

scale_reliability <- data.table(
  scale = c(
    "PSQI 7 components", "Burnout EE", "Burnout DP",
    "Burnout PA", "Turnover intention"
  ),
  complete_n = c(
    sum(male$psqi_complete),
    sum(complete.cases(burn[, ee_items, with = FALSE])),
    sum(complete.cases(burn[, dp_items, with = FALSE])),
    sum(complete.cases(burn[, pa_items, with = FALSE])),
    sum(turn_complete)
  ),
  cronbach_alpha = c(
    safe_alpha(male[, ..psqi_short]),
    safe_alpha(burn[, ee_items, with = FALSE]),
    safe_alpha(burn[, dp_items, with = FALSE]),
    safe_alpha(burn[, pa_items, with = FALSE]),
    safe_alpha(turn)
  ),
  mcdonald_omega_total = c(
    safe_omega(male[, ..psqi_short]),
    safe_omega(burn[, ee_items, with = FALSE]),
    safe_omega(burn[, dp_items, with = FALSE]),
    safe_omega(burn[, pa_items, with = FALSE]),
    safe_omega(turn)
  )
)
write_csv(scale_reliability, "00_audit", "scale_reliability.csv")

analytic <- copy(male[psqi_complete == TRUE])
analytic[, psqi_total := psqi_recalc]
analytic[, `:=`(
  emotional_exhaustion = ee_recalc,
  depersonalization = dp_recalc,
  personal_accomplishment = pa_recalc
)]
analytic[, turnover := turnover_recalc]
analytic[, poor_sleep := factor(
  ifelse(psqi_total > CONFIG$psqi_poor_cutoff, "Yes", "No"),
  levels = c("No", "Yes")
)]
analytic[, psqi_per3 := psqi_total / 3]

covars <- vars$covariates
analytic[, age := numeric_clean(get(covars[["age"]]))]
analytic[age < 18 | age > 70, age := NA_real_]
analytic[, bmi := numeric_clean(get(covars[["bmi"]]))]
analytic[bmi < 12 | bmi > 60, bmi := NA_real_]
analytic[, education := collapse_rare(get(covars[["education"]]))]
analytic[, marital := collapse_rare(get(covars[["marital"]]))]
analytic[, department := collapse_rare(get(covars[["department"]]), 50L)]
analytic[, title := collapse_rare(get(covars[["title"]]))]
analytic[, management := collapse_rare(get(covars[["management"]]))]
analytic[, smoking := collapse_rare(get(covars[["smoking"]]))]
analytic[, drinking := collapse_rare(get(covars[["drinking"]]))]
chronic_raw <- trimws(as.character(analytic[[covars[["chronic"]]]]))
analytic[, chronic := factor(ifelse(
  is.na(chronic_raw) | chronic_raw == "", NA_character_,
  ifelse(chronic_raw == "\u65e0", "No", "Yes")
), levels = c("No", "Yes"))]
analytic[, shift := collapse_rare(get(covars[["shift"]]))]
analytic[, overtime := collapse_rare(get(covars[["overtime"]]))]

model_covars <- c(
  "age", "bmi", "education", "marital", "department", "title",
  "management", "smoking", "drinking", "chronic", "shift", "overtime"
)

outcomes <- c(
  "emotional_exhaustion", "depersonalization",
  "personal_accomplishment", "turnover"
)

model_variables <- c(
  outcomes, "psqi_total", "psqi_per3", model_covars
)
common_complete <- complete.cases(analytic[, ..model_variables])
analytic[, included_main := common_complete]
common <- droplevels(as.data.frame(analytic[common_complete]))

missingness <- rbindlist(lapply(model_variables, function(v) {
  data.table(
    variable = v,
    analytic_n = nrow(analytic),
    missing_n = sum(is.na(analytic[[v]])),
    missing_percent = 100 * mean(is.na(analytic[[v]]))
  )
}))
write_csv(missingness, "00_audit", "analysis_variable_missingness.csv")

sample_flow <- data.table(
  step = c(
    "Original TARGET cohort records",
    "A_q2=1 male nurses",
    "Complete and valid 7-component PSQI",
    "All four outcomes available",
    "Complete primary-model covariates"
  ),
  n = c(
    nrow(dt), nrow(male), nrow(analytic),
    sum(complete.cases(analytic[, ..outcomes])),
    nrow(common)
  )
)
write_csv(sample_flow, "00_audit", "sample_flow.csv")

log_msg(
  "INFO", "Audit complete: source n=", nrow(dt),
  "; male n=", nrow(male),
  "; PSQI-complete n=", nrow(analytic),
  "; primary common-complete n=", nrow(common)
)

# Descriptive statistics ----------------------------------------------------
describe_variable <- function(x, variable, scale_range) {
  z <- x[is.finite(x)]
  data.table(
    variable = variable,
    theoretical_range = scale_range,
    valid_n = length(z),
    missing_n = length(x) - length(z),
    mean = mean(z),
    sd = sd(z),
    median = median(z),
    q1 = unname(quantile(z, 0.25)),
    q3 = unname(quantile(z, 0.75)),
    minimum = min(z),
    maximum = max(z),
    skewness = psych::skew(z)
  )
}

scale_descriptives <- rbindlist(list(
  describe_variable(analytic$psqi_total, "PSQI total", "0-21"),
  describe_variable(
    analytic$emotional_exhaustion, "Emotional exhaustion", "0-54"
  ),
  describe_variable(
    analytic$depersonalization, "Depersonalization", "0-30"
  ),
  describe_variable(
    analytic$personal_accomplishment, "Personal accomplishment", "0-48"
  ),
  describe_variable(analytic$turnover, "Turnover intention", "6-24")
))
write_csv(scale_descriptives, "01_descriptive", "scale_descriptives.csv")

psqi_total_distribution <- analytic[, .N, by = .(psqi_total)][order(psqi_total)]
psqi_total_distribution[, percent := 100 * N / sum(N)]
psqi_total_distribution[, cumulative_percent := cumsum(percent)]
write_csv(
  psqi_total_distribution,
  "01_descriptive", "PSQI_total_distribution.csv"
)

poor_n <- sum(analytic$poor_sleep == "Yes", na.rm = TRUE)
poor_den <- sum(!is.na(analytic$poor_sleep))
poor_ci <- wilson_interval(poor_n, poor_den)
sleep_prevalence <- data.table(
  definition = paste0("PSQI > ", CONFIG$psqi_poor_cutoff),
  events = poor_n,
  denominator = poor_den,
  prevalence_percent = 100 * poor_n / poor_den,
  ci_lower_percent = 100 * poor_ci[[1L]],
  ci_upper_percent = 100 * poor_ci[[2L]]
)
write_csv(sleep_prevalence, "01_descriptive", "poor_sleep_prevalence.csv")

binary_from_codes <- function(x, yes_codes) {
  z <- numeric_clean(x)
  out <- z %in% yes_codes
  out[is.na(z)] <- NA
  out
}

table1_continuous <- list(
  age_years = analytic$age,
  bmi_kg_m2 = analytic$bmi
)
table1_binary <- list(
  education_bachelor_or_higher = binary_from_codes(
    analytic[[covars[["education"]]]], c(3, 4)
  ),
  currently_married_or_remarried = binary_from_codes(
    analytic[[covars[["marital"]]]], c(2, 5)
  ),
  management_role = binary_from_codes(
    analytic[[covars[["management"]]]], 2:7
  ),
  current_smoking = binary_from_codes(
    analytic[[covars[["smoking"]]]], c(4, 5)
  ),
  any_alcohol_use = binary_from_codes(
    analytic[[covars[["drinking"]]]], 2:4
  ),
  chronic_disease = analytic$chronic == "Yes",
  rotating_shift = binary_from_codes(
    analytic[[covars[["shift"]]]], 4
  ),
  any_overtime_past_month = binary_from_codes(
    analytic[[covars[["overtime"]]]], 2:5
  )
)

variable_labels <- c(
  age_years = "\u5e74\u9f84\uff08\u5c81\uff09",
  bmi_kg_m2 = "BMI\uff08kg/m\u00b2\uff09",
  education_bachelor_or_higher = "\u672c\u79d1\u53ca\u4ee5\u4e0a",
  currently_married_or_remarried = "\u5df2\u5a5a\u6216\u518d\u5a5a",
  management_role = "\u627f\u62c5\u7ba1\u7406\u5c97\u4f4d",
  current_smoking = "\u76ee\u524d\u5438\u70df",
  any_alcohol_use = "\u996e\u9152",
  chronic_disease = "\u60a3\u6162\u6027\u75c5",
  rotating_shift = "\u8f6e\u73ed",
  any_overtime_past_month = "\u8fd11\u4e2a\u6708\u52a0\u73ed"
)

format_cont <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return("NA")
  sprintf("%.2f (%.2f); n=%d", mean(z), sd(z), length(z))
}

format_binary <- function(x) {
  ok <- !is.na(x)
  if (!any(ok)) return("NA")
  sprintf("%d/%d (%.1f%%)", sum(x[ok]), sum(ok), 100 * mean(x[ok]))
}

table1_rows <- list()
for (nm in names(table1_continuous)) {
  x <- table1_continuous[[nm]]
  group <- analytic$poor_sleep
  fit_test <- tryCatch(t.test(x ~ group), error = function(e) NULL)
  table1_rows[[length(table1_rows) + 1L]] <- data.table(
    variable = nm,
    label = variable_labels[[nm]],
    overall = format_cont(x),
    PSQI_le_7 = format_cont(x[group == "No"]),
    PSQI_gt_7 = format_cont(x[group == "Yes"]),
    test = "Welch t test",
    statistic = if (is.null(fit_test)) NA_real_ else unname(fit_test$statistic),
    p_value = if (is.null(fit_test)) NA_real_ else fit_test$p.value
  )
}

for (nm in names(table1_binary)) {
  x <- table1_binary[[nm]]
  group <- analytic$poor_sleep
  tab <- table(x, group, useNA = "no")
  chi <- suppressWarnings(tryCatch(chisq.test(tab), error = function(e) NULL))
  use_fisher <- is.null(chi) || any(chi$expected < 5)
  test_obj <- if (use_fisher) {
    tryCatch(fisher.test(tab), error = function(e) NULL)
  } else {
    chi
  }
  table1_rows[[length(table1_rows) + 1L]] <- data.table(
    variable = nm,
    label = variable_labels[[nm]],
    overall = format_binary(x),
    PSQI_le_7 = format_binary(x[group == "No"]),
    PSQI_gt_7 = format_binary(x[group == "Yes"]),
    test = if (use_fisher) "Fisher exact test" else "Pearson chi-square",
    statistic = if (is.null(test_obj) || use_fisher) {
      NA_real_
    } else {
      unname(test_obj$statistic)
    },
    p_value = if (is.null(test_obj)) NA_real_ else test_obj$p.value
  )
}

table1 <- rbindlist(table1_rows, fill = TRUE)
write_csv(table1, "01_descriptive", "Table1_basic_characteristics_by_sleep.csv")
write_csv(table1, "05_tables", "Table1_basic_characteristics_by_sleep.csv")

plot_dist <- ggplot(analytic, aes(x = psqi_total)) +
  geom_histogram(binwidth = 1, boundary = -0.5, fill = "#3B82F6", color = "white") +
  geom_vline(
    xintercept = CONFIG$psqi_poor_cutoff,
    linetype = "dashed", color = "#B91C1C", size = 0.8
  ) +
  annotate(
    "text", x = CONFIG$psqi_poor_cutoff + 0.3,
    y = Inf, vjust = 1.5, hjust = 0,
    label = paste0("PSQI > ", CONFIG$psqi_poor_cutoff),
    color = "#B91C1C"
  ) +
  labs(
    x = "PSQI total score",
    y = "Number of male nurses",
    title = "Distribution of PSQI total score"
  ) +
  theme_classic(base_size = 12)

ggsave(
  out_path("06_figures", "Figure1_PSQI_distribution.png"),
  plot_dist, width = 7.5, height = 5.2, dpi = 300
)
ggsave(
  out_path("06_figures", "Figure1_PSQI_distribution.pdf"),
  plot_dist, width = 7.5, height = 5.2
)

# Main linear regression ----------------------------------------------------
model_specs <- list(
  Model0_unadjusted = character(),
  Model1_age_BMI = c("age", "bmi"),
  Model2_fully_adjusted = model_covars
)

main_results <- list()
full_coefficients <- list()
model_fit_results <- list()
diagnostics <- list()
vif_results <- list()
model_objects <- list()

for (outcome in outcomes) {
  outcome_sd <- sd(common[[outcome]])
  for (model_name in names(model_specs)) {
    rhs <- c("psqi_per3", model_specs[[model_name]])
    formula_now <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
    fit <- lm(formula_now, data = common)
    model_objects[[paste(outcome, model_name, sep = "_")]] <- fit
    coef_tab <- robust_coef_table(fit)
    coef_tab[, `:=`(
      outcome = outcome,
      model = model_name,
      n = nobs(fit)
    )]
    full_coefficients[[paste(outcome, model_name, sep = "_")]] <- coef_tab
    exposure_row <- copy(coef_tab[term == "psqi_per3"])
    exposure_row[, standardized_beta := standardized_effect(
      estimate, common$psqi_per3, common[[outcome]]
    )]
    main_results[[paste(outcome, model_name, sep = "_")]] <- exposure_row

    reduced_rhs <- model_specs[[model_name]]
    reduced_formula <- if (length(reduced_rhs)) {
      as.formula(paste(outcome, "~", paste(reduced_rhs, collapse = " + ")))
    } else {
      as.formula(paste(outcome, "~ 1"))
    }
    reduced_fit <- lm(reduced_formula, data = common)
    delta_r2 <- summary(fit)$r.squared - summary(reduced_fit)$r.squared
    model_fit_results[[paste(outcome, model_name, sep = "_")]] <- data.table(
      outcome = outcome,
      model = model_name,
      n = nobs(fit),
      R2 = summary(fit)$r.squared,
      adjusted_R2 = summary(fit)$adj.r.squared,
      delta_R2_for_PSQI = delta_r2,
      AIC = AIC(fit),
      BIC = BIC(fit)
    )

    if (model_name == "Model2_fully_adjusted") {
      vc <- sandwich::vcovHC(fit, type = "HC3")
      robust_t <- coef(fit)[["psqi_per3"]] / sqrt(vc["psqi_per3", "psqi_per3"])
      df_res <- df.residual(fit)
      partial_r2 <- robust_t^2 / (robust_t^2 + df_res)
      bp <- tryCatch(lmtest::bptest(fit)$p.value, error = function(e) NA_real_)
      res <- residuals(fit)
      zres <- as.numeric(scale(res))
      ks_p <- tryCatch(
        suppressWarnings(ks.test(zres, "pnorm")$p.value),
        error = function(e) NA_real_
      )
      cooks <- cooks.distance(fit)
      vif_now <- vif_columns(fit)
      vif_now[, `:=`(outcome = outcome, model = model_name)]
      vif_results[[outcome]] <- vif_now
      diagnostics[[outcome]] <- data.table(
        outcome = outcome,
        model = model_name,
        n = nobs(fit),
        R2 = summary(fit)$r.squared,
        adjusted_R2 = summary(fit)$adj.r.squared,
        delta_R2_for_PSQI = delta_r2,
        partial_R2_for_PSQI = partial_r2,
        Breusch_Pagan_p = bp,
        standardized_residual_KS_p = ks_p,
        residual_skewness = psych::skew(res),
        influential_n_Cook_gt_4_over_n = sum(cooks > 4 / nobs(fit)),
        maximum_Cook = max(cooks),
        maximum_VIF = max(vif_now$VIF, na.rm = TRUE)
      )
      save_diagnostic_plot(fit, paste0("diagnostics_", outcome))
    }
  }
}

main_results_dt <- rbindlist(main_results, fill = TRUE)
full_coefficients_dt <- rbindlist(full_coefficients, fill = TRUE)
model_fit_dt <- rbindlist(model_fit_results, fill = TRUE)
diagnostics_dt <- rbindlist(diagnostics, fill = TRUE)
vif_dt <- rbindlist(vif_results, fill = TRUE)

main_results_dt[, p_Holm_primary := NA_real_]
primary_index <- main_results_dt$model == "Model2_fully_adjusted"
main_results_dt[primary_index, p_Holm_primary := p.adjust(
  p_value, method = "holm"
)]

psqi_q <- quantile(common$psqi_total, c(0.25, 0.75), na.rm = TRUE)
psqi_iqr <- unname(diff(psqi_q))
iqr_translation <- main_results_dt[model == "Model2_fully_adjusted", .(
  outcome,
  n,
  PSQI_q1 = unname(psqi_q[[1L]]),
  PSQI_q3 = unname(psqi_q[[2L]]),
  PSQI_IQR = psqi_iqr,
  adjusted_difference_per_IQR = estimate * psqi_iqr / 3,
  ci_lower_per_IQR = ci_lower * psqi_iqr / 3,
  ci_upper_per_IQR = ci_upper * psqi_iqr / 3,
  p_Holm_primary
)]

write_csv(main_results_dt, "02_regression", "main_PSQI_associations.csv")
write_csv(full_coefficients_dt, "02_regression", "all_model_coefficients.csv")
write_csv(model_fit_dt, "02_regression", "model_fit_statistics.csv")
write_csv(diagnostics_dt, "02_regression", "model_diagnostics.csv")
write_csv(vif_dt, "02_regression", "VIF_all_columns.csv")
write_csv(iqr_translation, "02_regression", "PSQI_IQR_translation.csv")
saveRDS(
  model_objects,
  out_path("02_regression", "linear_model_objects.rds"),
  compress = "xz"
)

log_msg("INFO", "Main regression completed on common n=", nrow(common))

# Restricted cubic spline ---------------------------------------------------
rcs_basis <- function(x, knots) {
  stopifnot(length(knots) >= 4L)
  tp <- function(z) pmax(z, 0)^3
  k_first <- knots[[1L]]
  k_penultimate <- knots[[length(knots) - 1L]]
  k_last <- knots[[length(knots)]]
  denom <- (k_last - k_first)^2
  out <- sapply(seq_len(length(knots) - 2L), function(j) {
    kj <- knots[[j]]
    (
      tp(x - kj) -
        tp(x - k_penultimate) * (k_last - kj) /
          (k_last - k_penultimate) +
        tp(x - k_last) * (k_penultimate - kj) /
          (k_last - k_penultimate)
    ) / denom
  })
  if (is.null(dim(out))) out <- matrix(out, ncol = 1L)
  colnames(out) <- paste0("psqi_rcs", seq_len(ncol(out)))
  out
}

robust_wald <- function(fit, terms) {
  vc <- sandwich::vcovHC(fit, type = "HC3")
  b <- coef(fit)
  terms <- intersect(terms, intersect(names(b), rownames(vc)))
  if (!length(terms)) {
    return(c(chisq = NA_real_, df = NA_real_, p = NA_real_))
  }
  q <- b[terms]
  v <- vc[terms, terms, drop = FALSE]
  stat <- tryCatch(
    as.numeric(t(q) %*% solve(v, q)),
    error = function(e) NA_real_
  )
  c(
    chisq = stat,
    df = length(terms),
    p = pchisq(stat, df = length(terms), lower.tail = FALSE)
  )
}

spline_knots <- unname(quantile(
  common$psqi_total,
  probs = c(0.05, 0.35, 0.65, 0.95),
  na.rm = TRUE,
  type = 2
))
if (length(unique(spline_knots)) < 4L) {
  spline_knots <- unname(quantile(
    common$psqi_total,
    probs = c(0.025, 0.275, 0.725, 0.975),
    na.rm = TRUE,
    type = 1
  ))
}
if (length(unique(spline_knots)) < 4L) {
  stop("PSQI distribution does not support four unique spline knots.")
}

common_spline <- common
basis_observed <- rcs_basis(common_spline$psqi_total, spline_knots)
for (j in seq_len(ncol(basis_observed))) {
  common_spline[[colnames(basis_observed)[[j]]]] <- basis_observed[, j]
}
spline_terms <- colnames(basis_observed)

spline_tests <- list()
spline_predictions <- list()
spline_models <- list()
prediction_grid <- seq(
  min(common$psqi_total), max(common$psqi_total), length.out = 120L
)

for (outcome in outcomes) {
  spline_rhs <- c("psqi_total", spline_terms, model_covars)
  fit_spline <- lm(
    as.formula(paste(outcome, "~", paste(spline_rhs, collapse = " + "))),
    data = common_spline
  )
  spline_models[[outcome]] <- fit_spline
  overall_test <- robust_wald(fit_spline, c("psqi_total", spline_terms))
  nonlinear_test <- robust_wald(fit_spline, spline_terms)
  spline_tests[[outcome]] <- data.table(
    outcome = outcome,
    n = nobs(fit_spline),
    knot_1 = spline_knots[[1L]],
    knot_2 = spline_knots[[2L]],
    knot_3 = spline_knots[[3L]],
    knot_4 = spline_knots[[4L]],
    overall_Wald_chisq = overall_test[["chisq"]],
    overall_df = overall_test[["df"]],
    overall_p = overall_test[["p"]],
    nonlinear_Wald_chisq = nonlinear_test[["chisq"]],
    nonlinear_df = nonlinear_test[["df"]],
    nonlinear_p = nonlinear_test[["p"]],
    adjusted_R2 = summary(fit_spline)$adj.r.squared,
    AIC = AIC(fit_spline),
    BIC = BIC(fit_spline)
  )

  vc <- sandwich::vcovHC(fit_spline, type = "HC3")
  b <- coef(fit_spline)
  prediction_rows <- lapply(prediction_grid, function(x_value) {
    nd <- common_spline
    nd$psqi_total <- x_value
    basis_new <- rcs_basis(rep(x_value, nrow(nd)), spline_knots)
    for (j in seq_len(ncol(basis_new))) {
      nd[[colnames(basis_new)[[j]]]] <- basis_new[, j]
    }
    mm <- model.matrix(delete.response(terms(fit_spline)), nd)
    kept <- intersect(names(b), colnames(mm))
    avg_x <- colMeans(mm[, kept, drop = FALSE])
    pred <- sum(avg_x * b[kept])
    se <- sqrt(as.numeric(
      t(avg_x) %*% vc[kept, kept, drop = FALSE] %*% avg_x
    ))
    data.table(
      outcome = outcome,
      psqi_total = x_value,
      adjusted_mean = pred,
      ci_lower = pred - qnorm(0.975) * se,
      ci_upper = pred + qnorm(0.975) * se
    )
  })
  spline_predictions[[outcome]] <- rbindlist(prediction_rows)
}

spline_tests_dt <- rbindlist(spline_tests)
spline_tests_dt[, overall_p_Holm := p.adjust(overall_p, method = "holm")]
spline_tests_dt[, nonlinear_p_Holm := p.adjust(
  nonlinear_p, method = "holm"
)]
spline_predictions_dt <- rbindlist(spline_predictions)
write_csv(spline_tests_dt, "03_spline", "restricted_cubic_spline_tests.csv")
write_csv(spline_predictions_dt, "03_spline", "adjusted_spline_predictions.csv")
saveRDS(
  spline_models,
  out_path("03_spline", "spline_model_objects.rds"),
  compress = "xz"
)

spline_plot_data <- copy(spline_predictions_dt)
spline_plot_data[, outcome_label := factor(
  outcome,
  levels = outcomes,
  labels = c(
    "Emotional exhaustion", "Depersonalization",
    "Personal accomplishment", "Turnover intention"
  )
)]

plot_spline <- ggplot(
  spline_plot_data,
  aes(x = psqi_total, y = adjusted_mean)
) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "#93C5FD", alpha = 0.45) +
  geom_line(color = "#1D4ED8", size = 0.9) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 2) +
  labs(
    x = "PSQI total score",
    y = "Adjusted predicted score",
    title = "Adjusted dose-response associations of PSQI with study outcomes"
  ) +
  theme_classic(base_size = 12) +
  theme(strip.background = element_rect(fill = "grey95", color = "grey60"))

ggsave(
  out_path("06_figures", "Figure2_adjusted_PSQI_spline.png"),
  plot_spline, width = 10, height = 8, dpi = 300
)
ggsave(
  out_path("06_figures", "Figure2_adjusted_PSQI_spline.pdf"),
  plot_spline, width = 10, height = 8
)

# Sensitivity analyses ------------------------------------------------------
sensitivity_results <- list()

for (outcome in outcomes) {
  binary_fit <- lm(
    as.formula(paste(
      outcome, "~", paste(c("poor_sleep", model_covars), collapse = " + ")
    )),
    data = common
  )
  binary_row <- robust_coef_table(binary_fit)[term == "poor_sleepYes"]
  binary_row[, `:=`(
    analysis = "PSQI_gt_7",
    outcome = outcome,
    exposure_contrast = "PSQI >7 vs <=7",
    n = nobs(binary_fit),
    standardized_effect = estimate / sd(common[[outcome]])
  )]
  sensitivity_results[[paste0(outcome, "_binary")]] <- binary_row

  no_chronic <- droplevels(common[common$chronic == "No", , drop = FALSE])
  subgroup_covars <- setdiff(model_covars, "chronic")
  subgroup_fit <- lm(
    as.formula(paste(
      outcome, "~", paste(c("psqi_per3", subgroup_covars), collapse = " + ")
    )),
    data = no_chronic
  )
  subgroup_row <- robust_coef_table(subgroup_fit)[term == "psqi_per3"]
  subgroup_row[, `:=`(
    analysis = "No_chronic_disease_subgroup",
    outcome = outcome,
    exposure_contrast = "Per 3-point higher PSQI",
    n = nobs(subgroup_fit),
    standardized_effect = standardized_effect(
      estimate, no_chronic$psqi_per3, no_chronic[[outcome]]
    )
  )]
  sensitivity_results[[paste0(outcome, "_no_chronic")]] <- subgroup_row

  no_smoking_covars <- setdiff(model_covars, "smoking")
  no_smoking_fit <- lm(
    as.formula(paste(
      outcome, "~", paste(c("psqi_per3", no_smoking_covars), collapse = " + ")
    )),
    data = common
  )
  no_smoking_row <- robust_coef_table(no_smoking_fit)[term == "psqi_per3"]
  no_smoking_row[, `:=`(
    analysis = "Fully_adjusted_without_smoking",
    outcome = outcome,
    exposure_contrast = "Per 3-point higher PSQI",
    n = nobs(no_smoking_fit),
    standardized_effect = standardized_effect(
      estimate, common$psqi_per3, common[[outcome]]
    )
  )]
  sensitivity_results[[paste0(outcome, "_no_smoking")]] <- no_smoking_row

  quality_subset <- droplevels(common[!common$mbi_all_same, , drop = FALSE])
  quality_fit <- lm(
    as.formula(paste(
      outcome, "~", paste(c("psqi_per3", model_covars), collapse = " + ")
    )),
    data = quality_subset
  )
  quality_row <- robust_coef_table(quality_fit)[term == "psqi_per3"]
  quality_row[, `:=`(
    analysis = "Exclude_MBI_all_same_responders",
    outcome = outcome,
    exposure_contrast = "Per 3-point higher PSQI",
    n = nobs(quality_fit),
    standardized_effect = standardized_effect(
      estimate, quality_subset$psqi_per3, quality_subset[[outcome]]
    )
  )]
  sensitivity_results[[paste0(outcome, "_quality")]] <- quality_row
}

# Multiple imputation is a sensitivity analysis because 53 PSQI-complete
# records are excluded from the complete-case primary model. Outcomes and PSQI
# are fully observed in the imputation sample; only missing covariates are
# imputed. CART is used for categorical covariates to avoid unstable logistic
# models for very sparse categories such as current smoking.
mi_variables <- c(outcomes, "psqi_per3", model_covars)
mi_data <- as.data.frame(analytic[, ..mi_variables])
mi_data[] <- lapply(mi_data, function(x) {
  if (is.factor(x)) droplevels(x) else x
})
mi_methods <- mice::make.method(mi_data)
mi_methods[c(outcomes, "psqi_per3")] <- ""
for (v in model_covars) {
  if (!anyNA(mi_data[[v]])) {
    mi_methods[[v]] <- ""
  } else if (is.numeric(mi_data[[v]])) {
    mi_methods[[v]] <- "pmm"
  } else {
    mi_methods[[v]] <- "cart"
  }
}
mi_predictor_matrix <- mice::make.predictorMatrix(mi_data)
diag(mi_predictor_matrix) <- 0

set.seed(CONFIG$seed)
mi_m <- 20L
mi_maxit <- 10L
mi_object <- mice::mice(
  mi_data,
  m = mi_m,
  maxit = mi_maxit,
  method = mi_methods,
  predictorMatrix = mi_predictor_matrix,
  seed = CONFIG$seed,
  printFlag = FALSE
)

mi_diagnostics <- data.table(
  variable = names(mi_data),
  storage_class = vapply(mi_data, function(x) class(x)[[1L]], character(1L)),
  missing_n = vapply(mi_data, function(x) sum(is.na(x)), integer(1L)),
  imputation_method = unname(mi_methods),
  m = mi_m,
  maxit = mi_maxit,
  seed = CONFIG$seed
)
write_csv(
  mi_diagnostics,
  "04_sensitivity", "multiple_imputation_diagnostics.csv"
)
saveRDS(
  mi_object,
  out_path("04_sensitivity", "multiple_imputation_object.rds"),
  compress = "xz"
)

pool_hc3 <- function(estimates, variances, m) {
  q_bar <- mean(estimates)
  u_bar <- mean(variances)
  b <- stats::var(estimates)
  total_variance <- u_bar + (1 + 1 / m) * b
  if (!is.finite(b) || b <= .Machine$double.eps) {
    degrees_freedom <- Inf
  } else {
    degrees_freedom <- (m - 1) * (
      1 + u_bar / ((1 + 1 / m) * b)
    )^2
  }
  critical_value <- if (is.finite(degrees_freedom)) {
    qt(0.975, df = degrees_freedom)
  } else {
    qnorm(0.975)
  }
  standard_error <- sqrt(total_variance)
  test_statistic <- q_bar / standard_error
  p_value <- if (is.finite(degrees_freedom)) {
    2 * pt(abs(test_statistic), df = degrees_freedom, lower.tail = FALSE)
  } else {
    2 * pnorm(abs(test_statistic), lower.tail = FALSE)
  }
  data.table(
    term = "psqi_per3",
    estimate = q_bar,
    robust_se = standard_error,
    statistic = test_statistic,
    df = degrees_freedom,
    p_value = p_value,
    ci_lower = q_bar - critical_value * standard_error,
    ci_upper = q_bar + critical_value * standard_error,
    within_variance = u_bar,
    between_variance = b,
    total_variance = total_variance
  )
}

for (outcome in outcomes) {
  completed_estimates <- numeric(mi_m)
  completed_variances <- numeric(mi_m)
  for (i in seq_len(mi_m)) {
    completed_now <- mice::complete(mi_object, action = i)
    mi_fit <- lm(
      as.formula(paste(
        outcome, "~", paste(c("psqi_per3", model_covars), collapse = " + ")
      )),
      data = completed_now
    )
    mi_vcov <- sandwich::vcovHC(mi_fit, type = "HC3")
    completed_estimates[[i]] <- coef(mi_fit)[["psqi_per3"]]
    completed_variances[[i]] <- mi_vcov["psqi_per3", "psqi_per3"]
  }
  mi_row <- pool_hc3(completed_estimates, completed_variances, mi_m)
  mi_row[, `:=`(
    analysis = "Multiple_imputation_m20",
    outcome = outcome,
    exposure_contrast = "Per 3-point higher PSQI",
    n = nrow(mi_data),
    standardized_effect = estimate *
      sd(analytic$psqi_per3) / sd(analytic[[outcome]])
  )]
  sensitivity_results[[paste0(outcome, "_mi")]] <- mi_row
}

sensitivity_dt <- rbindlist(sensitivity_results, fill = TRUE)
sensitivity_dt[, p_Holm_within_analysis := p.adjust(
  p_value,
  method = "holm"
), by = analysis]
write_csv(sensitivity_dt, "04_sensitivity", "sensitivity_analysis_results.csv")

comparison_continuous <- list(
  age_years = analytic$age,
  bmi_kg_m2 = analytic$bmi,
  psqi_total = analytic$psqi_total,
  emotional_exhaustion = analytic$emotional_exhaustion,
  depersonalization = analytic$depersonalization,
  personal_accomplishment = analytic$personal_accomplishment,
  turnover_intention = analytic$turnover
)
inclusion_group <- factor(
  ifelse(analytic$included_main, "Included", "Excluded"),
  levels = c("Included", "Excluded")
)
comparison_rows <- lapply(names(comparison_continuous), function(nm) {
  x <- comparison_continuous[[nm]]
  test <- tryCatch(t.test(x ~ inclusion_group), error = function(e) NULL)
  data.table(
    variable = nm,
    included = format_cont(x[inclusion_group == "Included"]),
    excluded = format_cont(x[inclusion_group == "Excluded"]),
    test = "Welch t test",
    statistic = if (is.null(test)) NA_real_ else unname(test$statistic),
    p_value = if (is.null(test)) NA_real_ else test$p.value
  )
})
inclusion_comparison <- rbindlist(comparison_rows)
write_csv(
  inclusion_comparison,
  "04_sensitivity", "included_vs_excluded_comparison.csv"
)

log_msg("INFO", "Spline and sensitivity analyses completed")

# Publication tables and automated report ----------------------------------
outcome_labels <- c(
  emotional_exhaustion = "Emotional exhaustion",
  depersonalization = "Depersonalization",
  personal_accomplishment = "Personal accomplishment",
  turnover = "Turnover intention"
)
model_labels <- c(
  Model0_unadjusted = "\u6a21\u578b0\uff1a\u672a\u8c03\u6574",
  Model1_age_BMI = "\u6a21\u578b1\uff1a\u5e74\u9f84\u3001BMI",
  Model2_fully_adjusted = "\u6a21\u578b2\uff1a\u5b8c\u5168\u8c03\u6574"
)

table2 <- copy(main_results_dt)
table2[, `:=`(
  outcome_label = unname(outcome_labels[outcome]),
  model_label = unname(model_labels[model]),
  effect = "PSQI\u6bcf\u589e\u52a03\u5206",
  beta_95CI = sprintf("%.2f (%.2f, %.2f)", estimate, ci_lower, ci_upper),
  p_display = fmt_p(p_value),
  p_Holm_display = ifelse(is.na(p_Holm_primary), "\u2014", fmt_p(p_Holm_primary))
)]
setcolorder(
  table2,
  c(
    "outcome_label", "model_label", "effect", "n", "beta_95CI",
    "robust_se", "standardized_beta", "p_display", "p_Holm_display",
    "outcome", "model", "term", "estimate", "ci_lower", "ci_upper",
    "p_value", "p_Holm_primary"
  )
)
write_csv(table2, "05_tables", "Table2_main_associations.csv")

table3 <- copy(sensitivity_dt)
table3[, `:=`(
  outcome_label = unname(outcome_labels[outcome]),
  beta_95CI = sprintf("%.2f (%.2f, %.2f)", estimate, ci_lower, ci_upper),
  p_display = fmt_p(p_value),
  p_Holm_display = fmt_p(p_Holm_within_analysis)
)]
write_csv(table3, "05_tables", "Table3_sensitivity_analyses.csv")

table4 <- copy(spline_tests_dt)
table4[, `:=`(
  outcome_label = unname(outcome_labels[outcome]),
  overall_p_display = fmt_p(overall_p),
  overall_p_Holm_display = fmt_p(overall_p_Holm),
  nonlinear_p_display = fmt_p(nonlinear_p),
  nonlinear_p_Holm_display = fmt_p(nonlinear_p_Holm)
)]
write_csv(table4, "05_tables", "Table4_spline_tests.csv")

workbook_path <- out_path("05_tables", "Main_Tables.xlsx")
wb <- createWorkbook()
header_style <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#1F4E78",
  textDecoration = "bold", halign = "center", valign = "center"
)
subheader_style <- createStyle(
  fgFill = "#D9EAF7", textDecoration = "bold",
  halign = "center", valign = "center"
)
border_style <- createStyle(border = "TopBottomLeftRight", borderColour = "#B7B7B7")

add_table_sheet <- function(sheet_name, x, freeze_col = 1L) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, as.data.frame(x), headerStyle = header_style)
  addStyle(
    wb, sheet_name, border_style,
    rows = seq_len(nrow(x) + 1L), cols = seq_len(ncol(x)), gridExpand = TRUE
  )
  freezePane(wb, sheet_name, firstRow = TRUE, firstCol = freeze_col > 1L)
  setColWidths(wb, sheet_name, cols = seq_len(ncol(x)), widths = "auto")
  if (nrow(x)) addFilter(wb, sheet_name, rows = 1L, cols = seq_len(ncol(x)))
}

add_table_sheet("Sample_flow", sample_flow)
add_table_sheet("Table1_characteristics", table1)
add_table_sheet("Scale_descriptives", scale_descriptives)
add_table_sheet("Table2_main_models", table2)
add_table_sheet("Model_fit", model_fit_dt)
add_table_sheet("Table3_sensitivity", table3)
add_table_sheet("Table4_spline", table4)
add_table_sheet("Diagnostics", diagnostics_dt)
add_table_sheet("Missingness", missingness)
add_table_sheet("Scoring_audit", score_audit)
add_table_sheet("Legacy_composite_audit", legacy_burnout_audit)
add_table_sheet("MBI_response_quality", mbi_response_quality)
add_table_sheet("MBI_straightline", mbi_straightline_distribution)
add_table_sheet("Reliability", scale_reliability)
add_table_sheet("Included_vs_excluded", inclusion_comparison)
add_table_sheet("MI_diagnostics", mi_diagnostics)
add_table_sheet("PSQI_distribution", psqi_total_distribution)
saveWorkbook(wb, workbook_path, overwrite = TRUE)

primary_rows <- main_results_dt[
  model == "Model2_fully_adjusted"
][match(outcomes, outcome)]
binary_rows <- sensitivity_dt[
  analysis == "PSQI_gt_7"
][match(outcomes, outcome)]
mi_rows <- sensitivity_dt[
  analysis == "Multiple_imputation_m20"
][match(outcomes, outcome)]
quality_rows <- sensitivity_dt[
  analysis == "Exclude_MBI_all_same_responders"
][match(outcomes, outcome)]
spline_rows <- spline_tests_dt[match(outcomes, outcome)]

primary_lines <- vapply(seq_len(nrow(primary_rows)), function(i) {
  row <- primary_rows[i]
  sprintf(
    "- %s: mean score difference per 3-point PSQI = %.2f (95%% CI %.2f to %.2f; Holm-adjusted P=%s; standardized beta=%.3f).",
    outcome_labels[[row$outcome]], row$estimate, row$ci_lower, row$ci_upper,
    fmt_p(row$p_Holm_primary), row$standardized_beta
  )
}, character(1L))

binary_lines <- vapply(seq_len(nrow(binary_rows)), function(i) {
  row <- binary_rows[i]
  sprintf(
    "- %s: PSQI >7 versus <=7 mean difference = %.2f (95%% CI %.2f to %.2f; Holm-adjusted P=%s).",
    outcome_labels[[row$outcome]], row$estimate, row$ci_lower, row$ci_upper,
    fmt_p(row$p_Holm_within_analysis)
  )
}, character(1L))

mi_lines <- vapply(seq_len(nrow(mi_rows)), function(i) {
  row <- mi_rows[i]
  sprintf(
    "- %s: multiple-imputation estimate per 3-point PSQI = %.2f (95%% CI %.2f to %.2f; Holm-adjusted P=%s; n=%d).",
    outcome_labels[[row$outcome]], row$estimate, row$ci_lower, row$ci_upper,
    fmt_p(row$p_Holm_within_analysis), row$n
  )
}, character(1L))

quality_lines <- vapply(seq_len(nrow(quality_rows)), function(i) {
  row <- quality_rows[i]
  sprintf(
    "- %s: after excluding identical responses across all 22 MBI items, estimate per 3-point PSQI = %.2f (95%% CI %.2f to %.2f; Holm-adjusted P=%s; n=%d).",
    outcome_labels[[row$outcome]], row$estimate, row$ci_lower, row$ci_upper,
    fmt_p(row$p_Holm_within_analysis), row$n
  )
}, character(1L))

spline_lines <- vapply(seq_len(nrow(spline_rows)), function(i) {
  row <- spline_rows[i]
  sprintf(
    "- %s: overall Holm-adjusted P=%s; nonlinear Holm-adjusted P=%s.",
    outcome_labels[[row$outcome]], fmt_p(row$overall_p_Holm),
    fmt_p(row$nonlinear_p_Holm)
  )
}, character(1L))

diagnostic_lines <- vapply(seq_len(nrow(diagnostics_dt)), function(i) {
  row <- diagnostics_dt[i]
  sprintf(
    "- %s: adjusted R-squared=%.3f; PSQI partial R-squared=%.3f; maximum VIF=%.2f; Cook distance >4/n in %d records.",
    outcome_labels[[row$outcome]], row$adjusted_R2,
    row$partial_R2_for_PSQI, row$maximum_VIF,
    row$influential_n_Cook_gt_4_over_n
  )
}, character(1L))

report <- c(
  "# Sleep quality, burnout dimensions, and turnover intention among male nurses: statistical analysis report",
  "",
  "## Study positioning",
  "",
  paste0(
    "This is a cross-sectional analysis. The exposure is continuous PSQI total score. ",
    "The four co-primary outcomes are the three separate MBI dimensions and turnover intention. ",
    "Holm correction is applied across the four primary tests."
  ),
  "",
  "## Sample audit",
  "",
  sprintf("- Source records: %s.", format(nrow(dt), big.mark = ",")),
  sprintf(
    "- Male nurses identified by confirmed A_q2=1: %s.",
    format(nrow(male), big.mark = ",")
  ),
  sprintf(
    "- Valid complete PSQI records: %s; complete-case primary sample: %s (%.1f%%).",
    format(nrow(analytic), big.mark = ","),
    format(nrow(common), big.mark = ","),
    100 * nrow(common) / nrow(analytic)
  ),
  "- PSQI, the three MBI dimensions, and turnover intention were recalculated from items.",
  "- The legacy EE+DP+(48-PA) composite is not an official MBI total and was excluded from inference.",
  "",
  "## Descriptive results",
  "",
  sprintf(
    "PSQI total mean (SD) was %.2f (%.2f), with median %.1f and IQR %.1f to %.1f.",
    scale_descriptives[variable == "PSQI total", mean],
    scale_descriptives[variable == "PSQI total", sd],
    scale_descriptives[variable == "PSQI total", median],
    scale_descriptives[variable == "PSQI total", q1],
    scale_descriptives[variable == "PSQI total", q3]
  ),
  sprintf(
    "Poor sleep defined as PSQI >%d occurred in %.1f%% (95%% CI %.1f%% to %.1f%%).",
    CONFIG$psqi_poor_cutoff,
    sleep_prevalence$prevalence_percent,
    sleep_prevalence$ci_lower_percent,
    sleep_prevalence$ci_upper_percent
  ),
  "",
  "## Primary association analyses",
  "",
  "After full covariate adjustment:",
  primary_lines,
  "",
  "## Nonlinear analyses",
  "",
  spline_lines,
  "",
  "## Sensitivity analyses",
  "",
  "Binary PSQI analysis:",
  binary_lines,
  "",
  "Multiple imputation by chained equations (m=20):",
  mi_lines,
  "",
  "Exclusion of respondents with identical answers to all 22 MBI items:",
  quality_lines,
  "",
  "Additional analyses excluded smoking adjustment, restricted to participants without chronic disease, and compared included with excluded records.",
  "",
  "## Model diagnostics and interpretation boundaries",
  "",
  "- Primary models used HC3 robust standard errors.",
  diagnostic_lines,
  "- Cross-sectional data do not establish temporality or causality.",
  "- No hospital identifier was available to account for within-hospital correlation.",
  "",
  "## Proposed title",
  "",
  "Sleep quality, burnout dimensions, and turnover intention among male nurses: a cross-sectional TARGET study"
)
writeLines(
  report,
  out_path("07_report", "STATISTICAL_ANALYSIS_REPORT.md"),
  useBytes = TRUE
)

capture.output(sessionInfo(), file = out_path("08_logs", "sessionInfo.txt"))

log_msg("INFO", "Tables, figures, workbook, and report generated")

# Output manifest -----------------------------------------------------------
all_files <- list.files(
  RUN_ROOT,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)
all_files <- all_files[!grepl("(^|[/\\\\])_work([/\\\\]|$)", all_files)]
manifest <- data.table(
  relative_path = substring(all_files, nchar(RUN_ROOT) + 2L),
  bytes = file.info(all_files)$size,
  modified = format(file.info(all_files)$mtime, "%Y-%m-%d %H:%M:%S")
)
write_csv(manifest, "08_logs", "results_manifest.csv")

output_lines <- c(
  "# Analysis outputs",
  "",
  paste0("Pipeline version: `", CONFIG$pipeline_version, "`"),
  paste0("Random seed: `", CONFIG$seed, "`"),
  paste0("Source records: `", nrow(dt), "`"),
  paste0("Male nurses: `", nrow(male), "`"),
  paste0("Primary analysis sample: `", nrow(common), "`"),
  "",
  "## Files",
  "",
  paste0("- `", manifest$relative_path, "` (", manifest$bytes, " bytes)")
)
writeLines(output_lines, out_path("_analysis_outputs.md"), useBytes = TRUE)

log_msg("INFO", "Pipeline completed successfully")
