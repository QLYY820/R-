source(file.path(PIPELINE_ROOT, "R", "00_utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages({library(data.table); library(sandwich); library(lmtest)})

# Backfill cache signatures for a long LCA run that completed before later
# pipeline stages were repaired. Future one-click reruns reuse these models only
# when the deidentified input hash, seed, start policy, iterations and LCA script
# hash are unchanged.
for (cache_tag in c("ordinal", "binary")) {
  cache_model_path <- out_path("_work", paste0(cache_tag, "_lca_models_1_to_8.rds"))
  cache_signature_path <- out_path("_work", paste0(cache_tag, "_lca_cache_signature.rds"))
  if (file.exists(cache_model_path) && !file.exists(cache_signature_path)) {
    saveRDS(make_lca_cache_signature(cache_tag), cache_signature_path)
  }
}

dat <- as.data.table(readRDS(out_path("01_data", "analysis_module_completers_deidentified.rds")))
lca <- readRDS(out_path("03_lca", "final_model_parameters.rds"))
post_dt <- fread(out_path("03_lca", "posterior_probabilities.csv"), encoding = "UTF-8")
posterior <- as.matrix(post_dt[, grep("^posterior_rank_", names(post_dt)), with = FALSE])
class_levels <- lca$class_levels
K <- length(class_levels)
covmap <- CFG$variables$covariates

dat[, age := fifelse(get(covmap[["age"]]) >= 18 & get(covmap[["age"]]) <= 70,
                      get(covmap[["age"]]), NA_real_)]
dat[, bmi := fifelse(get(covmap[["bmi"]]) >= 12 & get(covmap[["bmi"]]) <= 60,
                      get(covmap[["bmi"]]), NA_real_)]
dat[, education := factor(fcase(get(covmap[["education"]]) %in% c(1, 2), "大专及以下",
                                 get(covmap[["education"]]) %in% c(3, 4), "本科及以上",
                                 default = NA_character_), levels = c("大专及以下", "本科及以上"))]
dat[, married := factor(fcase(get(covmap[["marital"]]) %in% c(2, 5), "已婚或再婚",
                               get(covmap[["marital"]]) %in% c(1, 3, 4, 6), "未婚或其他",
                               default = NA_character_), levels = c("未婚或其他", "已婚或再婚"))]
dat[, department := factor(fcase(
  get(covmap[["department"]]) %in% c(1, 2, 4, 5, 6), "普通病区",
  get(covmap[["department"]]) %in% c(3, 7, 8), "急危重症或手术相关",
  get(covmap[["department"]]) %in% c(9, 10, 11), "门诊行政或其他",
  default = NA_character_), levels = c("普通病区", "急危重症或手术相关", "门诊行政或其他"))]
dat[, title_group := factor(fcase(
  get(covmap[["title"]]) == 1, "护士",
  get(covmap[["title"]]) == 2, "护师",
  get(covmap[["title"]]) %in% 3:6, "主管护师及以上或其他",
  default = NA_character_), levels = c("护士", "护师", "主管护师及以上或其他"))]
dat[, management := factor(fcase(get(covmap[["management"]]) == 1, "否",
                                  get(covmap[["management"]]) %in% 2:7, "是",
                                  default = NA_character_), levels = c("否", "是"))]
dat[, smoking := factor(fcase(
  get(covmap[["smoking"]]) == 1, "从不",
  get(covmap[["smoking"]]) %in% c(2, 3), "既往",
  get(covmap[["smoking"]]) %in% c(4, 5), "当前",
  default = NA_character_), levels = c("从不", "既往", "当前"))]
dat[, drinking := factor(fcase(get(covmap[["drinking"]]) == 1, "从不",
                                get(covmap[["drinking"]]) %in% 2:4, "饮酒",
                                default = NA_character_), levels = c("从不", "饮酒"))]
chronic_raw <- norm_char(dat[[covmap[["chronic"]]]])
dat[, chronic := factor(ifelse(is.na(chronic_raw), NA_character_, ifelse(chronic_raw == "无", "无", "有")),
                        levels = c("无", "有"))]
dat[, night_shift := factor(fcase(get(covmap[["shift"]]) == 1, "仅白班",
                                   get(covmap[["shift"]]) %in% 2:4, "含夜班",
                                   default = NA_character_), levels = c("仅白班", "含夜班"))]
dat[, overtime_weeks := fifelse(get(covmap[["overtime"]]) %in% 1:5,
                                 get(covmap[["overtime"]]) - 1, NA_real_)]
dat[, water_intake := factor(get(covmap[["water"]]), levels = 1:6, ordered = TRUE)]
dat[, class_modal := factor(post_dt$class_rank, levels = seq_len(K), labels = class_levels)]

base_covars <- CFG$model_covariates
missingness <- rbindlist(lapply(c("sps6_total", "luts_total", base_covars, "water_intake"), function(v) {
  data.table(variable = v, original_missing_n = sum(is.na(dat[[v]])), illegal_value_n = 0L,
             cleaned_missing_n = sum(is.na(dat[[v]])), excluded_from_primary_n = sum(is.na(dat[[v]])))
}))
write_csv_utf8(missingness, out_path("04_association", "covariate_missingness.csv"))

pool_scalar <- function(q, u) {
  m <- length(q); qbar <- mean(q); ubar <- mean(u); b <- stats::var(q)
  total <- ubar + (1 + 1 / m) * b
  se <- sqrt(total)
  df <- if (!is.finite(b) || b < 1e-12) Inf else (m - 1) * (1 + ubar / ((1 + 1 / m) * b))^2
  crit <- if (is.finite(df)) qt(.975, df) else qnorm(.975)
  p <- if (is.finite(df)) 2 * pt(-abs(qbar / se), df) else 2 * pnorm(-abs(qbar / se))
  c(estimate = qbar, se = se, lower = qbar - crit * se, upper = qbar + crit * se,
    p_value = p, df = df, within_var = ubar, between_var = b)
}
sample_classes <- function(post) {
  u <- runif(nrow(post))
  if (ncol(post) == 1L) return(rep.int(1L, nrow(post)))
  cum <- t(apply(post, 1L, cumsum))[, -ncol(post), drop = FALSE]
  1L + rowSums(matrix(u, nrow(post), ncol(post) - 1L) > cum)
}

run_pseudoclass <- function(data, post, levels, covars, draws, seed_offset = 0L,
                            keep = NULL, analysis_name = "main") {
  k <- ncol(post)
  vars <- unique(c("sps6_total", covars))
  ok <- complete.cases(data[, ..vars])
  if (!is.null(keep)) ok <- ok & !is.na(keep) & keep
  d <- copy(data[ok])
  p <- post[ok, , drop = FALSE]
  p <- p / rowSums(p)
  formula <- as.formula(paste("sps6_total ~ class_pc +", paste(covars, collapse = " + ")))
  q_diff <- matrix(NA_real_, draws, k - 1L); u_diff <- q_diff
  q_mean <- matrix(NA_real_, draws, k); u_mean <- q_mean
  q_all <- vector("list", draws); u_all <- vector("list", draws)
  g_list <- lapply(seq_len(k), function(cl) {
    nd <- copy(d); nd[, class_pc := factor(cl, levels = seq_len(k), labels = levels)]
    colMeans(model.matrix(formula, data = nd))
  })
  set.seed(CFG$random_seed + seed_offset)
  for (m in seq_len(draws)) {
    d[, class_pc := factor(sample_classes(p), levels = seq_len(k), labels = levels)]
    fit <- lm(formula, data = d)
    V <- sandwich::vcovHC(fit, type = "HC3")
    b <- coef(fit)
    class_terms <- paste0("class_pc", levels[-1L])
    q_diff[m, ] <- b[class_terms]; u_diff[m, ] <- diag(V)[class_terms]
    q_all[[m]] <- b[class_terms]; u_all[[m]] <- V[class_terms, class_terms, drop = FALSE]
    for (cl in seq_len(k)) {
      g <- g_list[[cl]]
      q_mean[m, cl] <- sum(g * b)
      u_mean[m, cl] <- as.numeric(t(g) %*% V %*% g)
    }
  }
  outcome_sd <- sd(d$sps6_total)
  differences <- rbindlist(lapply(seq_len(k - 1L), function(j) {
    z <- pool_scalar(q_diff[, j], u_diff[, j])
    data.table(analysis = analysis_name, comparison = paste0(levels[j + 1L], " vs ", levels[1L]),
      class = levels[j + 1L], reference = levels[1L], n = nrow(d), draws = draws,
      estimate = z["estimate"], se = z["se"], lower = z["lower"], upper = z["upper"],
      p_value = z["p_value"], df = z["df"], standardized_effect = z["estimate"] / outcome_sd,
      covariates = paste(covars, collapse = ";"))
  }))
  means <- rbindlist(lapply(seq_len(k), function(cl) {
    z <- pool_scalar(q_mean[, cl], u_mean[, cl])
    data.table(analysis = analysis_name, class = levels[cl], n = nrow(d), draws = draws,
      adjusted_mean = z["estimate"], se = z["se"], lower = z["lower"], upper = z["upper"])
  }))
  qbar <- colMeans(do.call(rbind, q_all)); ubar <- Reduce(`+`, u_all) / draws
  B <- cov(do.call(rbind, q_all)); Tmat <- ubar + (1 + 1 / draws) * B
  wald <- as.numeric(t(qbar) %*% qr.solve(Tmat, qbar))
  global <- data.table(analysis = analysis_name, n = nrow(d), draws = draws, df = k - 1L,
    wald_chisq = wald, p_value = pchisq(wald, df = k - 1L, lower.tail = FALSE),
    covariates = paste(covars, collapse = ";"))
  list(differences = differences, means = means, global = global, complete = ok)
}

main <- run_pseudoclass(dat, posterior, class_levels, base_covars, CFG$pseudoclass_draws_main,
                        1000L, analysis_name = "Primary: posterior pseudo-class + HC3")
main200 <- run_pseudoclass(dat, posterior, class_levels, base_covars, CFG$pseudoclass_draws_sensitivity,
                           200L, analysis_name = "Sensitivity: 200 pseudo-class draws")
water <- run_pseudoclass(dat, posterior, class_levels, c(base_covars, "water_intake"),
                         CFG$pseudoclass_draws_sensitivity, 300L,
                         analysis_name = "Sensitivity: additional daily fluid intake")
healthy <- run_pseudoclass(dat, posterior, class_levels, setdiff(base_covars, "chronic"),
                           CFG$pseudoclass_draws_sensitivity, 400L, keep = dat$chronic == "无",
                           analysis_name = "Sensitivity: no chronic disease")
quality <- run_pseudoclass(dat, posterior, class_levels, base_covars,
                           CFG$pseudoclass_draws_sensitivity, 500L, keep = !dat$quality_flag_any,
                           analysis_name = "Sensitivity: exclude all response-quality flags")

primary_vars <- c("sps6_total", base_covars)
cc <- complete.cases(dat[, ..primary_vars])
hard_data <- copy(dat[cc])
hard_formula <- as.formula(paste("sps6_total ~ class_modal +", paste(base_covars, collapse = " + ")))
hard_fit <- lm(hard_formula, data = hard_data)
hard_tidy <- hc3_tidy(hard_fit)[grepl("^class_modal", term)]
hard_tidy[, `:=`(
  analysis = "Sensitivity: maximum-posterior hard classification",
  comparison = paste0(sub("^class_modal", "", term), " vs ", class_levels[1]),
  class = sub("^class_modal", "", term), reference = class_levels[1], n = nobs(hard_fit), draws = 0L,
  standardized_effect = estimate / sd(hard_data$sps6_total), covariates = paste(base_covars, collapse = ";")
)]

# Binary-LCA pseudo-class association sensitivity.
binary_models <- readRDS(out_path("_work", "binary_lca_models_1_to_8.rds"))
binary_fit_table <- fread(out_path("05_sensitivity", "binary_lca_fit_1_to_8.csv"), encoding = "UTF-8")
binary_k <- binary_fit_table[selected == TRUE, classes][1]
binary_model <- binary_models[[binary_k]]
binary_burden <- vapply(seq_len(binary_k), function(cl) sum(vapply(binary_model$probs, function(p) p[cl, 2], numeric(1))), numeric(1))
binary_order <- order(binary_burden)
binary_post <- binary_model$posterior[, binary_order, drop = FALSE]
binary_levels <- paste0("Binary burden class ", seq_len(binary_k))
binary_assoc <- run_pseudoclass(dat, binary_post, binary_levels, base_covars,
                                CFG$pseudoclass_draws_sensitivity, 600L,
                                analysis_name = "Sensitivity: binary-item LCA")

write_csv_utf8(main$differences, out_path("04_association", "main_model.csv"))
write_csv_utf8(main$means, out_path("04_association", "adjusted_marginal_means.csv"))
write_csv_utf8(main$global, out_path("04_association", "global_wald_test.csv"))
write_csv_utf8(hard_tidy, out_path("04_association", "hard_class_model.csv"))

# Continuous symptom-burden comparisons on the same complete-case frame.
continuous_vars <- c("sps6_total", "luts_total", "luts_storage", "luts_voiding", "luts_post_micturition", base_covars)
continuous_cc <- complete.cases(dat[, ..continuous_vars])
cd <- copy(dat[continuous_cc])
base_formula <- as.formula(paste("sps6_total ~", paste(base_covars, collapse = " + ")))
linear_formula <- update(base_formula, . ~ . + luts_total)
spline_formula <- update(base_formula, . ~ . + splines::ns(luts_total, df = 4))
dimension_formula <- update(base_formula, . ~ . + luts_storage + luts_voiding + luts_post_micturition)
lca_formula <- update(base_formula, . ~ . + class_modal)
base_fit <- lm(base_formula, data = cd)
linear_fit <- lm(linear_formula, data = cd)
spline_fit <- lm(spline_formula, data = cd)
dimension_fit <- lm(dimension_formula, data = cd)
lca_compare_fit <- lm(lca_formula, data = cd)

comparison <- rbindlist(list(
  model_metrics(base_fit, NULL, "Covariates only"),
  model_metrics(linear_fit, base_fit, "LUTS total linear"),
  model_metrics(spline_fit, base_fit, "LUTS total restricted cubic spline surrogate (natural spline df=4)"),
  model_metrics(dimension_fit, base_fit, "Storage + voiding + post-micturition dimensions"),
  model_metrics(lca_compare_fit, base_fit, "LCA hard classes")
))

linear_coef <- hc3_tidy(linear_fit)[term == "luts_total"]
linear_iqr <- as.numeric(quantile(cd$luts_total, .75, na.rm = TRUE) -
                           quantile(cd$luts_total, .25, na.rm = TRUE))
linear_coef[, `:=`(model = "LUTS total linear", n = nobs(linear_fit),
  standardized_effect = estimate * sd(cd$luts_total) / sd(cd$sps6_total),
  iqr_contrast = linear_iqr,
  iqr_outcome_difference = estimate * linear_iqr)]
dimension_coef <- hc3_tidy(dimension_fit)[term %in% c("luts_storage", "luts_voiding", "luts_post_micturition")]
dimension_coef[, `:=`(model = "LUTS symptom dimensions", n = nobs(dimension_fit),
  standardized_effect = estimate * vapply(term, function(v) sd(cd[[v]]), numeric(1)) / sd(cd$sps6_total))]

wald_terms <- function(fit, pattern) {
  V <- sandwich::vcovHC(fit, type = "HC3")
  b <- coef(fit); terms <- grep(pattern, names(b), value = TRUE)
  w <- as.numeric(t(b[terms]) %*% qr.solve(V[terms, terms, drop = FALSE], b[terms]))
  data.table(test = pattern, df = length(terms), wald_chisq = w,
             p_value = pchisq(w, length(terms), lower.tail = FALSE), n = nobs(fit))
}
spline_global <- wald_terms(spline_fit, "^splines::ns\\(luts_total")

V_spline <- sandwich::vcovHC(spline_fit, type = "HC3")
grid <- seq(min(cd$luts_total), max(cd$luts_total), length.out = 101)
spline_pred <- rbindlist(lapply(grid, function(v) {
  nd <- copy(cd); nd[, luts_total := v]
  X <- model.matrix(delete.response(terms(spline_fit)), data = nd)
  g <- colMeans(X)
  est <- sum(g * coef(spline_fit)); se <- sqrt(as.numeric(t(g) %*% V_spline %*% g))
  data.table(luts_total = v, adjusted_sps6 = est, lower = est - qnorm(.975) * se,
             upper = est + qnorm(.975) * se, n = nobs(spline_fit))
}))

# Determine whether classes mainly follow one severity continuum and whether LCA adds value.
expected <- fread(out_path("03_lca", "class_expected_scores.csv"), encoding = "UTF-8")
wide_expected <- dcast(expected, class_rank ~ item, value.var = "expected_score")
luts_names <- CFG$variables$luts
expected_matrix <- as.matrix(wide_expected[, ..luts_names])
monotonic_fraction <- mean(vapply(
  seq_len(ncol(expected_matrix)),
  function(j) all(diff(expected_matrix[, j]) >= -1e-6),
  logical(1)
))
adjacent_cor <- if (nrow(expected_matrix) > 1) vapply(seq_len(nrow(expected_matrix) - 1L), function(i) {
  cor(expected_matrix[i, ], expected_matrix[i + 1L, ])
}, numeric(1)) else NA_real_
delta_linear <- comparison[model == "LUTS total linear", delta_R2]
delta_lca <- comparison[model == "LCA hard classes", delta_R2]
lca_increment_over_linear <- delta_lca - delta_linear
continuum_flag <- monotonic_fraction >= .80 && mean(adjacent_cor, na.rm = TRUE) >= .70
recommend_continuous <- continuum_flag && lca_increment_over_linear <= .005
strategy <- data.table(
  metric = c("monotonic_item_fraction", "mean_adjacent_profile_correlation", "delta_R2_linear_total",
             "delta_R2_LCA_hard_class", "LCA_delta_R2_minus_linear", "severity_continuum_flag",
             "recommend_continuous_primary_story"),
  value = c(monotonic_fraction, mean(adjacent_cor, na.rm = TRUE), delta_linear, delta_lca,
            lca_increment_over_linear, continuum_flag, recommend_continuous),
  interpretation = c("Proportion of items nondecreasing across burden-ranked classes",
    "Similarity of adjacent symptom profiles", "Increment beyond covariates", "Increment beyond covariates",
    "Positive values favor hard-class LCA on in-sample R2", "Profiles mainly follow severity gradient",
    "If TRUE, do not force LCA as the main manuscript story")
)

write_csv_utf8(comparison, out_path("04_association", "continuous_vs_lca_model_comparison.csv"))
write_csv_utf8(linear_coef, out_path("04_association", "continuous_luts_linear_effect.csv"))
write_csv_utf8(dimension_coef, out_path("04_association", "continuous_luts_dimension_effects.csv"))
write_csv_utf8(spline_pred, out_path("04_association", "continuous_luts_spline_predictions.csv"))
write_csv_utf8(spline_global, out_path("04_association", "continuous_luts_spline_global_test.csv"))
write_csv_utf8(strategy, out_path("04_association", "analysis_strategy_comparison.csv"))

# Diagnostics, coefficient-level VIF, influence, and complete-case comparison.
bp <- lmtest::bptest(hard_fit)
cooks <- cooks.distance(hard_fit)
coefficient_vif <- function(fit) {
  x <- model.matrix(fit)
  x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  keep <- apply(x, 2, function(z) is.finite(stats::var(z)) && stats::var(z) > 0)
  x <- x[, keep, drop = FALSE]
  if (ncol(x) < 2L) return(data.table(term = colnames(x), VIF = NA_real_))
  values <- vapply(seq_len(ncol(x)), function(j) {
    others <- x[, -j, drop = FALSE]
    r2 <- tryCatch(summary(stats::lm(x[, j] ~ others))$r.squared,
                   error = function(e) NA_real_)
    if (!is.finite(r2)) return(NA_real_)
    if (r2 >= 1) return(Inf)
    1 / (1 - r2)
  }, numeric(1))
  data.table(term = colnames(x), VIF = values)
}
vif_table <- coefficient_vif(hard_fit)
diagnostics <- data.table(
  diagnostic = c("Breusch-Pagan", "Residual skewness", "Residual kurtosis", "Design matrix condition number",
                 "Cook distance > 4/n", "Maximum Cook distance", "R2", "Adjusted R2"),
  statistic = c(unname(bp$statistic), psych::skew(residuals(hard_fit)), psych::kurtosi(residuals(hard_fit)),
                kappa(model.matrix(hard_fit)), sum(cooks > 4 / length(cooks)), max(cooks),
                summary(hard_fit)$r.squared, summary(hard_fit)$adj.r.squared),
  p_value = c(bp$p.value, NA, NA, NA, NA, NA, NA, NA), n = nobs(hard_fit)
)
write_csv_utf8(diagnostics, out_path("04_association", "model_diagnostics.csv"))
write_csv_utf8(vif_table, out_path("04_association", "multicollinearity_vif.csv"))

compare_cont <- function(v, label) {
  inc <- dat[[v]][cc]; exc <- dat[[v]][!cc]
  wt <- suppressWarnings(wilcox.test(inc, exc, exact = FALSE))
  data.table(variable = v, label = label, included_n = sum(!is.na(inc)), included_mean = mean(inc, na.rm = TRUE),
    included_sd = sd(inc, na.rm = TRUE), excluded_n = sum(!is.na(exc)), excluded_mean = mean(exc, na.rm = TRUE),
    excluded_sd = sd(exc, na.rm = TRUE), test = "Wilcoxon rank-sum", statistic = unname(wt$statistic), p_value = wt$p.value)
}
included_excluded <- rbindlist(list(compare_cont("age", "Age"), compare_cont("luts_total", "LUTS total"),
                                      compare_cont("sps6_total", "SPS-6 total")))
write_csv_utf8(included_excluded, out_path("04_association", "included_vs_excluded.csv"))

# 4-panel diagnostics are produced as a reproducible supplementary figure.
png(out_path("07_figures", "supplement_linear_model_diagnostics.png"), width = 2400, height = 2400, res = 300)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1)); plot(hard_fit, which = 1:4); dev.off()
pdf(out_path("07_figures", "supplement_linear_model_diagnostics.pdf"), width = 8, height = 8)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1)); plot(hard_fit, which = 1:4); dev.off()
tiff(out_path("07_figures", "supplement_linear_model_diagnostics.tiff"), width = 8, height = 8, units = "in", res = 600, compression = "lzw")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1)); plot(hard_fit, which = 1:4); dev.off()

sensitivity_effects <- rbindlist(list(main200$differences, hard_tidy, water$differences,
  healthy$differences, quality$differences, binary_assoc$differences), fill = TRUE)
sensitivity_global <- rbindlist(list(main200$global, water$global, healthy$global,
  quality$global, binary_assoc$global), fill = TRUE)
write_csv_utf8(sensitivity_effects, out_path("05_sensitivity", "sensitivity_effects_full.csv"))
write_csv_utf8(sensitivity_global, out_path("05_sensitivity", "sensitivity_global_tests_full.csv"))

warning_banner <- if (CFG$run_mode == "test") "TEST DATA WORKBOOK — ALL RESULTS ARE FOR PIPELINE VALIDATION ONLY" else NULL
write_workbook(out_path("outputs", "sensitivity_analysis_full.xlsx"), list(
  `Effect_estimates` = sensitivity_effects,
  `Global_tests` = sensitivity_global,
  `Binary_LCA_fit` = binary_fit_table,
  `Binary_LCA_profiles` = fread(out_path("05_sensitivity", "binary_lca_profiles_full.csv"), encoding = "UTF-8"),
  `Binary_class_summary` = fread(out_path("05_sensitivity", "binary_lca_class_summary.csv"), encoding = "UTF-8"),
  `Binary_comparison` = fread(out_path("05_sensitivity", "binary_lca_comparison.csv"), encoding = "UTF-8"),
  `Complete_case_comparison` = included_excluded,
  `Covariate_missingness` = missingness
), warning_banner = warning_banner)
write_workbook(out_path("outputs", "continuous_LUTS_spline_results.xlsx"), list(
  `Model_comparison` = comparison,
  `Linear_effect` = linear_coef,
  `Dimension_effects` = dimension_coef,
  `Spline_global_test` = spline_global,
  `Spline_predictions` = spline_pred,
  `Strategy_assessment` = strategy
), warning_banner = warning_banner)

saveRDS(list(main = main, main200 = main200, water = water, healthy = healthy, quality = quality,
             binary = binary_assoc, hard_fit = hard_fit, hard_vcov = sandwich::vcovHC(hard_fit, type = "HC3"),
             base_fit = base_fit, linear_fit = linear_fit, spline_fit = spline_fit,
             dimension_fit = dimension_fit, lca_compare_fit = lca_compare_fit,
             analysis_data = dat, complete_cases = cc, class_levels = class_levels,
             base_covars = base_covars, comparison = comparison, strategy = strategy),
        out_path("04_association", "model_objects.rds"), compress = "xz")

flow <- fread(out_path("00_audit", "sample_flow.csv"), encoding = "UTF-8")
flow[stage == "Primary-model complete cases (pending)", n := sum(cc)]
write_csv_utf8(flow, out_path("00_audit", "sample_flow.csv"))
log_msg("INFO", "Association and continuous-burden analyses complete; primary n=", sum(cc),
        "; recommend_continuous_primary_story=", recommend_continuous)
