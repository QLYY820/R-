#!/usr/bin/env Rscript

# Analysis: Reviewer-requested statistics for nurse fatigue, depression,
# social support, and health-related productivity loss.
# Date: 2026-07-10
# Random seed: 42
# R: >= 4.0
# Key packages: readxl, psych, lavaan, lm.beta, ggplot2, dplyr, broom, readr

set.seed(42)

options(stringsAsFactors = FALSE)

parse_args <- function(args) {
  out <- list(
    data = NULL,
    sheet = NULL,
    out = "analysis_outputs_full",
    bootstrap = 5000,
    min_subgroup_n = 100,
    run_cfa = TRUE
  )
  i <- 1
  while (i <= length(args)) {
    key <- args[[i]]
    if (!startsWith(key, "--")) {
      stop(sprintf("Unexpected argument: %s", key), call. = FALSE)
    }
    if (key %in% c("--help", "-h")) {
      out$help <- TRUE
      i <- i + 1
      next
    }
    if (i == length(args)) {
      stop(sprintf("Missing value for %s", key), call. = FALSE)
    }
    val <- args[[i + 1]]
    nm <- sub("^--", "", key)
    if (!nm %in% names(out)) {
      stop(sprintf("Unknown option: %s", key), call. = FALSE)
    }
    out[[nm]] <- val
    i <- i + 2
  }
  out$bootstrap <- as.integer(out$bootstrap)
  out$min_subgroup_n <- as.integer(out$min_subgroup_n)
  out$run_cfa <- tolower(as.character(out$run_cfa)) %in% c("true", "1", "yes", "y")
  out
}

print_help <- function() {
  cat("Usage:\n")
  cat("  Rscript R/reviewer_response_stats.R --data DATAFILE [--sheet SHEET] [--out OUTDIR] [--bootstrap 5000]\n\n")
  cat("Options:\n")
  cat("  --data            Input .xlsx, .xls, .csv, .tsv, or .rds file.\n")
  cat("  --sheet           Excel sheet name or 1-based index. Defaults to first sheet.\n")
  cat("  --out             Output directory. Defaults to analysis_outputs_full.\n")
  cat("  --bootstrap       Bootstrap resamples for mediation CIs. Defaults to 5000.\n")
  cat("  --min_subgroup_n  Minimum rows per subgroup category. Defaults to 100.\n")
  cat("  --run_cfa         TRUE/FALSE. Defaults to TRUE.\n")
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(args$help) || is.null(args$data)) {
  print_help()
  quit(status = ifelse(isTRUE(args$help), 0, 1))
}

required_packages <- c("readxl", "psych", "lavaan", "lm.beta", "ggplot2", "dplyr", "broom", "readr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing R packages: ", paste(missing_packages, collapse = ", "),
      "\nInstall them first, for example:\ninstall.packages(c(\"",
      paste(missing_packages, collapse = "\", \""),
      "\"))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(psych)
  library(lavaan)
  library(lm.beta)
  library(ggplot2)
  library(dplyr)
  library(broom)
  library(readr)
})

# -------------------------------------------------------------------------
# Variable mapping. Edit here if the full dataset uses different column names.
# -------------------------------------------------------------------------
vars <- list(
  hrpl = "s",
  fatigue = "pifa",
  depression = "yiyu",
  support = "shehui",
  support_family = "F_jiatingneizhichi",
  support_friend = "F_pengyouzhichi",
  support_other = "F_qitazhichi",
  tea_binary = "tea",
  milk_binary = "milk",
  coffee_days = "q34_1",
  coffee_daily_amount = "q34_2",
  tea_days = "q34_3",
  tea_daily_amount = "q34_4",
  yogurt_days = "q36_1",
  yogurt_daily_amount = "q36_2",
  milk_days = "q36_3",
  milk_daily_amount = "q36_4",
  other_milk_days = "q36_6",
  other_milk_daily_amount = "q36_7",
  covariates = c(
    "gender", "age", "type", "zhic", "p", "income", "tea", "milk",
    "education", "EDU", "HUNYIN", "kesi", "BMI", "A_gongzuoshichang", "nianx"
  ),
  subgroup_vars = c("gender", "age", "A_gongzuoshichang", "kesi", "zhic", "tea", "milk"),
  hrpl_items = paste0("q21_", 1:6),
  fatigue_items = paste0("q3_", 1:14),
  support_items = paste0("q4_", 1:12),
  depression_items = paste0("q23_", 1:9)
)

input_path <- normalizePath(args$data, winslash = "/", mustWork = TRUE)
out_dir <- args$out
if (!grepl("^[A-Za-z]:|^/|^\\\\\\\\", out_dir)) {
  out_dir <- file.path(getwd(), out_dir)
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

write_csv_utf8 <- function(x, file) {
  readr::write_excel_csv(x, file.path(out_dir, file), na = "")
}

safe_num <- function(x) {
  if (is.numeric(x)) return(x)
  y <- trimws(as.character(x))
  y[y %in% c("", "NA", "N/A", "na", "Na", "None", "NULL", "null", "missing", "(skip)", "skip")] <- NA
  y[y %in% c("无", "(跳过)", "跳过")] <- NA
  suppressWarnings(as.numeric(y))
}

exists_col <- function(nm) !is.null(nm) && nm %in% names(dat)
available <- function(nms) nms[nms %in% names(dat)]
scale_z <- function(x) as.numeric(scale(x))

make_term_safe <- function(nm) {
  paste0("`", gsub("`", "", nm), "`")
}

rhs_terms <- function(cols) {
  if (length(cols) == 0) return("1")
  paste(vapply(cols, make_term_safe, character(1)), collapse = " + ")
}

read_input <- function(path, sheet = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet)) {
      sheet <- readxl::excel_sheets(path)[[1]]
    } else if (grepl("^[0-9]+$", sheet)) {
      sheet <- as.integer(sheet)
    }
    as.data.frame(readxl::read_excel(path, sheet = sheet), stringsAsFactors = FALSE)
  } else if (ext == "csv") {
    as.data.frame(readr::read_csv(path, show_col_types = FALSE), stringsAsFactors = FALSE)
  } else if (ext == "tsv") {
    as.data.frame(readr::read_tsv(path, show_col_types = FALSE), stringsAsFactors = FALSE)
  } else if (ext == "rds") {
    as.data.frame(readRDS(path), stringsAsFactors = FALSE)
  } else {
    stop(sprintf("Unsupported input file extension: %s", ext), call. = FALSE)
  }
}

dat <- read_input(input_path, args$sheet)
names(dat) <- trimws(names(dat))
dat_raw <- dat

for (nm in names(dat)) {
  if (!inherits(dat[[nm]], "Date") && !is.numeric(dat[[nm]])) {
    candidate <- safe_num(dat[[nm]])
    if (sum(!is.na(candidate)) > 0 && sum(is.na(candidate)) < length(candidate)) {
      dat[[nm]] <- candidate
    }
  }
}

make_score_if_missing <- function(score_name, item_names, label) {
  if (exists_col(score_name)) return(score_name)
  present <- available(item_names)
  if (length(present) < 2) return(score_name)
  mat <- dat[, present, drop = FALSE]
  mat[] <- lapply(mat, safe_num)
  dat[[score_name]] <<- rowSums(mat, na.rm = TRUE)
  dat[[score_name]][rowSums(!is.na(mat)) == 0] <<- NA
  message(sprintf("Created %s score from %d items: %s", label, length(present), score_name))
  score_name
}

make_binary_if_missing <- function(target_col, raw_col, label) {
  if (exists_col(target_col)) return(target_col)
  if (!exists_col(raw_col)) return(target_col)
  x <- safe_num(dat[[raw_col]])
  vals <- sort(unique(x[!is.na(x)]))
  if (length(vals) == 0) return(target_col)
  if (all(vals %in% c(0, 1))) {
    dat[[target_col]] <<- x
    message(sprintf("Created %s binary variable from %s using existing 0/1 coding: %s", label, raw_col, target_col))
  } else if (all(vals %in% c(1, 2))) {
    dat[[target_col]] <<- ifelse(is.na(x), NA_real_, ifelse(x == 1, 1, ifelse(x == 2, 0, NA_real_)))
    message(sprintf("Created %s binary variable from %s assuming 1=yes and 2=no: %s", label, raw_col, target_col))
  }
  target_col
}

vars$hrpl <- make_score_if_missing(vars$hrpl, vars$hrpl_items, "HRPL")
vars$fatigue <- make_score_if_missing(vars$fatigue, vars$fatigue_items, "fatigue")
vars$depression <- make_score_if_missing(vars$depression, vars$depression_items, "depression")
vars$support <- make_score_if_missing(vars$support, vars$support_items, "social support")
vars$tea_binary <- make_binary_if_missing(vars$tea_binary, "q33", "coffee/tea")
vars$milk_binary <- make_binary_if_missing(vars$milk_binary, "q35", "milk")

if (!exists_col(vars$support_family)) {
  fam_items <- available(c("q4_3", "q4_4", "q4_8", "q4_11"))
  if (length(fam_items) >= 2) dat[[vars$support_family]] <- rowSums(dat[, fam_items, drop = FALSE], na.rm = TRUE)
}
if (!exists_col(vars$support_friend)) {
  friend_items <- available(c("q4_6", "q4_7", "q4_9", "q4_12"))
  if (length(friend_items) >= 2) dat[[vars$support_friend]] <- rowSums(dat[, friend_items, drop = FALSE], na.rm = TRUE)
}
if (!exists_col(vars$support_other)) {
  other_items <- available(c("q4_1", "q4_2", "q4_5", "q4_10"))
  if (length(other_items) >= 2) dat[[vars$support_other]] <- rowSums(dat[, other_items, drop = FALSE], na.rm = TRUE)
}

required_core <- c(vars$hrpl, vars$fatigue, vars$depression, vars$support)
missing_core <- required_core[!required_core %in% names(dat)]
if (length(missing_core) > 0) {
  stop(sprintf("Missing core variables after score creation: %s", paste(missing_core, collapse = ", ")), call. = FALSE)
}

covariates <- setdiff(available(vars$covariates), c(vars$hrpl, vars$fatigue, vars$depression, vars$support))
covariates <- covariates[vapply(covariates, function(v) sum(!is.na(dat[[v]])) > 0 && length(unique(dat[[v]][!is.na(dat[[v]])])) > 1, logical(1))]

shape <- data.frame(rows = nrow(dat), columns = ncol(dat))
write_csv_utf8(shape, "data_shape.csv")

missing <- data.frame(
  variable = names(dat),
  missing_n = vapply(dat, function(x) sum(is.na(x)), integer(1)),
  missing_pct = round(vapply(dat, function(x) mean(is.na(x)) * 100, numeric(1)), 1),
  non_missing_n = vapply(dat, function(x) sum(!is.na(x)), integer(1)),
  unique_n = vapply(dat, function(x) length(unique(x[!is.na(x)])), integer(1))
)
write_csv_utf8(missing, "missing_values.csv")

core_vars <- unique(c(
  required_core, vars$support_family, vars$support_friend, vars$support_other,
  vars$tea_binary, vars$milk_binary, vars$coffee_days, vars$coffee_daily_amount,
  vars$tea_days, vars$tea_daily_amount, vars$yogurt_days, vars$yogurt_daily_amount,
  vars$milk_days, vars$milk_daily_amount, vars$other_milk_days, vars$other_milk_daily_amount,
  covariates
))
write_csv_utf8(missing[missing$variable %in% core_vars, ], "core_variable_overview.csv")

alpha_one <- function(label, cols) {
  cols <- available(cols)
  if (length(cols) < 2) {
    return(data.frame(scale = label, items = length(cols), n_complete = NA_integer_,
                      cronbach_alpha = NA_real_, note = "Fewer than 2 available items"))
  }
  mat <- dat[, cols, drop = FALSE]
  mat[] <- lapply(mat, safe_num)
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  if (nrow(mat) < 3) {
    return(data.frame(scale = label, items = length(cols), n_complete = nrow(mat),
                      cronbach_alpha = NA_real_, note = "Too few complete rows"))
  }
  a <- tryCatch(psych::alpha(mat, warnings = FALSE), error = function(e) e)
  data.frame(
    scale = label,
    items = length(cols),
    n_complete = nrow(mat),
    cronbach_alpha = if (inherits(a, "error")) NA_real_ else unname(a$total$raw_alpha),
    note = if (inherits(a, "error")) conditionMessage(a) else ""
  )
}

alpha_results <- bind_rows(
  alpha_one("HRPL items", vars$hrpl_items),
  alpha_one("Fatigue items", vars$fatigue_items),
  alpha_one("Social support items", vars$support_items),
  alpha_one("Depression items", vars$depression_items)
)
write_csv_utf8(alpha_results, "reliability_alpha.csv")

all_items <- available(c(vars$hrpl_items, vars$fatigue_items, vars$support_items, vars$depression_items))
item_mat <- dat[, all_items, drop = FALSE]
item_mat[] <- lapply(item_mat, safe_num)
item_mat_complete <- item_mat[stats::complete.cases(item_mat), , drop = FALSE]

if (nrow(item_mat_complete) >= 3 && ncol(item_mat_complete) >= 2) {
  pca <- prcomp(item_mat_complete, center = TRUE, scale. = TRUE)
  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  common_method <- data.frame(
    method = "Harman single-factor PCA, unrotated",
    n_complete = nrow(item_mat_complete),
    item_count = ncol(item_mat_complete),
    first_factor_variance_pct = round(var_exp[1] * 100, 2),
    note = "Descriptive common method variance check"
  )
} else {
  common_method <- data.frame(
    method = "Harman single-factor PCA, unrotated",
    n_complete = nrow(item_mat_complete),
    item_count = ncol(item_mat_complete),
    first_factor_variance_pct = NA_real_,
    note = "Too few complete rows/items"
  )
}
write_csv_utf8(common_method, "common_method_bias_harman.csv")

make_cfa_model <- function(one_factor = FALSE) {
  hrpl <- available(vars$hrpl_items)
  fat <- available(vars$fatigue_items)
  dep <- available(vars$depression_items)
  sup <- available(vars$support_items)
  if (one_factor) {
    paste("Common =~", paste(c(hrpl, fat, dep, sup), collapse = " + "))
  } else {
    paste(
      paste("HRPL =~", paste(hrpl, collapse = " + ")),
      paste("Fatigue =~", paste(fat, collapse = " + ")),
      paste("Depression =~", paste(dep, collapse = " + ")),
      paste("Support =~", paste(sup, collapse = " + ")),
      sep = "\n"
    )
  }
}

cfa_rows <- list()
can_cfa <- args$run_cfa && length(all_items) >= 8 && nrow(dat) >= max(200, length(all_items) * 10)
if (can_cfa) {
  for (model_name in c("one_factor", "four_factor")) {
    model_text <- make_cfa_model(one_factor = model_name == "one_factor")
    fit <- tryCatch(
      lavaan::cfa(model_text, data = item_mat, std.lv = TRUE, missing = "fiml", estimator = "MLR"),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      cfa_rows[[model_name]] <- data.frame(
        model = model_name, status = "failed", n = nrow(dat), item_count = length(all_items),
        cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_,
        aic = NA_real_, bic = NA_real_, note = conditionMessage(fit)
      )
    } else {
      fm <- lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr", "aic", "bic"))
      cfa_rows[[model_name]] <- data.frame(
        model = model_name, status = "run", n = nrow(dat), item_count = length(all_items),
        cfi = unname(fm[["cfi"]]), tli = unname(fm[["tli"]]),
        rmsea = unname(fm[["rmsea"]]), srmr = unname(fm[["srmr"]]),
        aic = unname(fm[["aic"]]), bic = unname(fm[["bic"]]), note = ""
      )
    }
  }
} else {
  cfa_rows[["skip"]] <- data.frame(
    model = "one_factor_and_four_factor", status = "skipped", n = nrow(dat),
    item_count = length(all_items), cfi = NA_real_, tli = NA_real_,
    rmsea = NA_real_, srmr = NA_real_, aic = NA_real_, bic = NA_real_,
    note = "Skipped because run_cfa is FALSE or sample size/item count is insufficient"
  )
}
cfa_results <- bind_rows(cfa_rows)
write_csv_utf8(cfa_results, "common_method_bias_cfa.csv")

cor_one <- function(x, y, label_x, label_y) {
  d <- data.frame(x = safe_num(dat[[x]]), y = safe_num(dat[[y]]))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 4) {
    return(data.frame(var1 = label_x, var2 = label_y, n = nrow(d), rho = NA_real_,
                      p_value = NA_real_, p = NA_character_, note = "Too few rows"))
  }
  ct <- suppressWarnings(stats::cor.test(d$x, d$y, method = "spearman", exact = FALSE))
  data.frame(var1 = label_x, var2 = label_y, n = nrow(d), rho = unname(ct$estimate),
             p_value = ct$p.value, p = fmt_p(ct$p.value), note = "")
}

core_pairs <- list(
  c(vars$fatigue, vars$hrpl, "Fatigue", "HRPL"),
  c(vars$fatigue, vars$depression, "Fatigue", "Depression"),
  c(vars$depression, vars$hrpl, "Depression", "HRPL"),
  c(vars$support, vars$fatigue, "Social support", "Fatigue"),
  c(vars$support, vars$depression, "Social support", "Depression"),
  c(vars$support, vars$hrpl, "Social support", "HRPL")
)
cor_results <- bind_rows(lapply(core_pairs, function(v) cor_one(v[1], v[2], v[3], v[4])))
cor_results$p_fdr <- p.adjust(cor_results$p_value, method = "BH")
cor_results$p_fdr_text <- vapply(cor_results$p_fdr, fmt_p, character(1))
write_csv_utf8(cor_results, "correlation_core_spearman.csv")

model_frame_core <- function(extra_cols = character()) {
  cols <- unique(c(required_core, covariates, extra_cols))
  cols <- available(cols)
  d <- dat[, cols, drop = FALSE]
  for (nm in cols) {
    d[[nm]] <- if (is.numeric(d[[nm]])) d[[nm]] else safe_num(d[[nm]])
  }
  d
}

lm_tidy <- function(model) {
  sm <- summary(model)
  ci <- suppressMessages(confint(model))
  out <- broom::tidy(model)
  out$conf_low <- ci[, 1]
  out$conf_high <- ci[, 2]
  out$r_squared <- sm$r.squared
  out$adj_r_squared <- sm$adj.r.squared
  out
}

coef_get <- function(coefs, candidates) {
  hit <- candidates[candidates %in% names(coefs)][1]
  if (is.na(hit) || length(hit) == 0) return(NA_real_)
  unname(coefs[[hit]])
}

delta_r2 <- function(reduced, full) {
  summary(full)$r.squared - summary(reduced)$r.squared
}

bootstrap_mediation <- function(d, covar_cols, b = 5000) {
  n <- nrow(d)
  vals <- matrix(NA_real_, nrow = b, ncol = 5)
  colnames(vals) <- c("a", "b", "indirect", "direct", "total")
  cov_rhs <- rhs_terms(covar_cols)
  med_formula <- as.formula(paste("M ~ X +", cov_rhs))
  y_formula <- as.formula(paste("Y ~ X + M +", cov_rhs))
  total_formula <- as.formula(paste("Y ~ X +", cov_rhs))
  for (i in seq_len(b)) {
    idx <- sample.int(n, n, replace = TRUE)
    bd <- d[idx, , drop = FALSE]
    m_mod <- tryCatch(lm(med_formula, data = bd), error = function(e) NULL)
    y_mod <- tryCatch(lm(y_formula, data = bd), error = function(e) NULL)
    t_mod <- tryCatch(lm(total_formula, data = bd), error = function(e) NULL)
    if (is.null(m_mod) || is.null(y_mod) || is.null(t_mod)) next
    a <- coef(m_mod)[["X"]]
    bcoef <- coef(y_mod)[["M"]]
    vals[i, ] <- c(a, bcoef, a * bcoef, coef(y_mod)[["X"]], coef(t_mod)[["X"]])
  }
  vals
}

run_mediation <- function() {
  d <- data.frame(
    Y = safe_num(dat[[vars$hrpl]]),
    X = safe_num(dat[[vars$fatigue]]),
    M = safe_num(dat[[vars$depression]])
  )
  covar_cols <- character()
  for (cv in covariates) {
    d[[cv]] <- dat[[cv]]
    if (!is.numeric(d[[cv]]) || length(unique(d[[cv]][!is.na(d[[cv]])])) <= 10) {
      d[[cv]] <- as.factor(d[[cv]])
    }
    covar_cols <- c(covar_cols, cv)
  }
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 20) {
    return(data.frame(effect = "Mediation", estimate = NA_real_, ci_low = NA_real_,
                      ci_high = NA_real_, p_value = NA_real_, p = NA_character_,
                      n = nrow(d), note = "Too few complete rows"))
  }
  cov_rhs <- rhs_terms(covar_cols)
  med_formula <- as.formula(paste("M ~ X +", cov_rhs))
  y_formula <- as.formula(paste("Y ~ X + M +", cov_rhs))
  total_formula <- as.formula(paste("Y ~ X +", cov_rhs))
  med_m <- lm(med_formula, data = d)
  med_y <- lm(y_formula, data = d)
  med_t <- lm(total_formula, data = d)
  a <- coef(med_m)[["X"]]
  bcoef <- coef(med_y)[["M"]]
  direct <- coef(med_y)[["X"]]
  total <- coef(med_t)[["X"]]
  boot <- bootstrap_mediation(d, covar_cols, args$bootstrap)
  res <- data.frame(
    effect = c(
      "a: fatigue -> depression",
      "b: depression -> HRPL adjusted for fatigue",
      "indirect: a*b",
      "direct: fatigue -> HRPL adjusted for depression",
      "total: fatigue -> HRPL",
      "proportion mediated"
    ),
    estimate = c(a, bcoef, a * bcoef, direct, total, (a * bcoef) / total),
    ci_low = c(
      confint(med_m)["X", 1],
      confint(med_y)["M", 1],
      quantile(boot[, "indirect"], .025, na.rm = TRUE),
      confint(med_y)["X", 1],
      confint(med_t)["X", 1],
      NA_real_
    ),
    ci_high = c(
      confint(med_m)["X", 2],
      confint(med_y)["M", 2],
      quantile(boot[, "indirect"], .975, na.rm = TRUE),
      confint(med_y)["X", 2],
      confint(med_t)["X", 2],
      NA_real_
    ),
    p_value = c(
      summary(med_m)$coefficients["X", "Pr(>|t|)"],
      summary(med_y)$coefficients["M", "Pr(>|t|)"],
      NA_real_,
      summary(med_y)$coefficients["X", "Pr(>|t|)"],
      summary(med_t)$coefficients["X", "Pr(>|t|)"],
      NA_real_
    ),
    n = nrow(d),
    note = c("", "", sprintf("Bootstrap percentile CI, %d resamples", args$bootstrap),
             "", "", "Use cautiously when total effect is small")
  )
  res$p <- vapply(res$p_value, fmt_p, character(1))
  res
}

mediation_results <- run_mediation()
write_csv_utf8(mediation_results, "mediation_results.csv")

prepare_moderated_data <- function(w_col) {
  d <- data.frame(
    Y_raw = safe_num(dat[[vars$hrpl]]),
    X_raw = safe_num(dat[[vars$fatigue]]),
    M_raw = safe_num(dat[[vars$depression]]),
    W_raw = safe_num(dat[[w_col]])
  )
  for (cv in covariates) {
    d[[cv]] <- dat[[cv]]
    if (!is.numeric(d[[cv]]) || length(unique(d[[cv]][!is.na(d[[cv]])])) <= 10) {
      d[[cv]] <- as.factor(d[[cv]])
    }
  }
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d$Yz <- scale_z(d$Y_raw)
  d$Xz <- scale_z(d$X_raw)
  d$Mz <- scale_z(d$M_raw)
  d$Wz <- scale_z(d$W_raw)
  d
}

fit_moderated_mediation <- function(w_col, w_label) {
  if (!exists_col(w_col)) {
    return(list(
      interactions = data.frame(support_variable = w_label, n = 0, model = NA_character_,
                                term = NA_character_, estimate = NA_real_, conf_low = NA_real_,
                                conf_high = NA_real_, p_value = NA_real_, p = NA_character_,
                                r_squared = NA_real_, adj_r_squared = NA_real_, r2_delta = NA_real_,
                                note = "Support variable not found"),
      conditional = data.frame(support_variable = w_label, social_support_level = NA_character_,
                               W_value_z = NA_real_, conditional_indirect = NA_real_,
                               ci_low = NA_real_, ci_high = NA_real_, n = 0,
                               note = "Support variable not found")
    ))
  }
  d <- prepare_moderated_data(w_col)
  if (nrow(d) < 20 || length(unique(d$W_raw)) < 3) {
    return(list(
      interactions = data.frame(support_variable = w_label, n = nrow(d), model = NA_character_,
                                term = NA_character_, estimate = NA_real_, conf_low = NA_real_,
                                conf_high = NA_real_, p_value = NA_real_, p = NA_character_,
                                r_squared = NA_real_, adj_r_squared = NA_real_, r2_delta = NA_real_,
                                note = "Too few complete rows or insufficient support-score variation"),
      conditional = data.frame(support_variable = w_label, social_support_level = NA_character_,
                               W_value_z = NA_real_, conditional_indirect = NA_real_,
                               ci_low = NA_real_, ci_high = NA_real_, n = nrow(d),
                               note = "Too few complete rows or insufficient support-score variation")
    ))
  }
  cov_rhs <- rhs_terms(covariates)
  m0 <- lm(as.formula(paste("Mz ~ Xz + Wz +", cov_rhs)), data = d)
  m1 <- lm(as.formula(paste("Mz ~ Xz * Wz +", cov_rhs)), data = d)
  y0 <- lm(as.formula(paste("Yz ~ Xz + Mz + Wz +", cov_rhs)), data = d)
  y1 <- lm(as.formula(paste("Yz ~ Xz * Wz + Mz * Wz +", cov_rhs)), data = d)
  tm <- lm_tidy(m1)
  ty <- lm_tidy(y1)
  keep_m <- tm[tm$term %in% c("Xz:Wz"), , drop = FALSE]
  keep_y <- ty[ty$term %in% c("Xz:Wz", "Mz:Wz", "Wz:Mz"), , drop = FALSE]
  keep_m$model <- "Mediator model: depression_z ~ fatigue_z * support_z"
  keep_y$model <- "Outcome model: HRPL_z ~ fatigue_z * support_z + depression_z * support_z"
  keep_m$r2_delta <- delta_r2(m0, m1)
  keep_y$r2_delta <- delta_r2(y0, y1)
  inter <- bind_rows(keep_m, keep_y)
  inter$support_variable <- w_label
  inter$n <- nrow(d)
  inter$p <- vapply(inter$p.value, fmt_p, character(1))
  inter$note <- ""
  inter <- inter[, c("support_variable", "n", "model", "term", "estimate", "conf_low",
                     "conf_high", "p.value", "p", "r_squared", "adj_r_squared",
                     "r2_delta", "note")]
  names(inter)[names(inter) == "p.value"] <- "p_value"

  w_values <- c(low = -1, mean = 0, high = 1)
  boot_vals <- matrix(NA_real_, nrow = args$bootstrap, ncol = length(w_values))
  colnames(boot_vals) <- names(w_values)
  n <- nrow(d)
  for (i in seq_len(args$bootstrap)) {
    idx <- sample.int(n, n, replace = TRUE)
    bd <- d[idx, , drop = FALSE]
    bm <- tryCatch(lm(as.formula(paste("Mz ~ Xz * Wz +", cov_rhs)), data = bd), error = function(e) NULL)
    by <- tryCatch(lm(as.formula(paste("Yz ~ Xz * Wz + Mz * Wz +", cov_rhs)), data = bd), error = function(e) NULL)
    if (is.null(bm) || is.null(by)) next
    cm <- coef(bm)
    cy <- coef(by)
    a1 <- coef_get(cm, "Xz")
    a3 <- coef_get(cm, c("Xz:Wz", "Wz:Xz"))
    b1 <- coef_get(cy, "Mz")
    b3 <- coef_get(cy, c("Mz:Wz", "Wz:Mz"))
    if (any(is.na(c(a1, a3, b1, b3)))) next
    for (nm in names(w_values)) {
      w <- w_values[[nm]]
      boot_vals[i, nm] <- (a1 + a3 * w) * (b1 + b3 * w)
    }
  }
  cm <- coef(m1)
  cy <- coef(y1)
  a1 <- coef_get(cm, "Xz")
  a3 <- coef_get(cm, c("Xz:Wz", "Wz:Xz"))
  b1 <- coef_get(cy, "Mz")
  b3 <- coef_get(cy, c("Mz:Wz", "Wz:Mz"))
  cond <- bind_rows(lapply(names(w_values), function(nm) {
    w <- w_values[[nm]]
    indirect <- if (any(is.na(c(a1, a3, b1, b3)))) NA_real_ else
      (a1 + a3 * w) * (b1 + b3 * w)
    data.frame(
      support_variable = w_label,
      social_support_level = nm,
      W_value_z = w,
      conditional_indirect = indirect,
      ci_low = quantile(boot_vals[, nm], .025, na.rm = TRUE),
      ci_high = quantile(boot_vals[, nm], .975, na.rm = TRUE),
      n = nrow(d),
      note = sprintf("Standardized variables; bootstrap percentile CI, %d resamples", args$bootstrap)
    )
  }))

  try({
    plot_df <- data.frame(
      Xz = rep(seq(-2, 2, length.out = 100), 3),
      Wz = rep(c(-1, 0, 1), each = 100),
      Mz = 0
    )
    for (cv in covariates) {
      if (is.factor(d[[cv]])) {
        plot_df[[cv]] <- factor(levels(d[[cv]])[which.max(tabulate(d[[cv]]))], levels = levels(d[[cv]]))
      } else {
        plot_df[[cv]] <- mean(d[[cv]], na.rm = TRUE)
      }
    }
    plot_df$pred <- predict(y1, newdata = plot_df)
    plot_df$level <- factor(plot_df$Wz, levels = c(-1, 0, 1), labels = c("Low support", "Mean support", "High support"))
    p <- ggplot(plot_df, aes(x = Xz, y = pred, color = level)) +
      geom_line(linewidth = 1) +
      theme_minimal(base_size = 12) +
      labs(x = "Fatigue (standardized)", y = "Predicted HRPL (standardized)", color = "Support")
    ggsave(file.path(out_dir, paste0("simple_slopes_", gsub("[^A-Za-z0-9]+", "_", w_label), ".png")), p, width = 7, height = 5, dpi = 300)
    ggsave(file.path(out_dir, paste0("simple_slopes_", gsub("[^A-Za-z0-9]+", "_", w_label), ".pdf")), p, width = 7, height = 5)
  }, silent = TRUE)

  list(interactions = inter, conditional = cond)
}

overall_mod <- fit_moderated_mediation(vars$support, "Total social support")
write_csv_utf8(overall_mod$interactions, "moderated_mediation_interactions.csv")
write_csv_utf8(overall_mod$conditional, "moderated_mediation_conditional_indirect.csv")

dimension_specs <- c(
  "Family support" = vars$support_family,
  "Friend support" = vars$support_friend,
  "Significant-other support" = vars$support_other
)
dimension_runs <- lapply(names(dimension_specs), function(lbl) fit_moderated_mediation(dimension_specs[[lbl]], lbl))
dimension_interactions <- bind_rows(lapply(dimension_runs, `[[`, "interactions"))
write_csv_utf8(dimension_interactions, "support_dimension_moderation.csv")

is_no_binary <- function(binary_col) {
  if (is.null(binary_col) || !exists_col(binary_col)) return(rep(FALSE, nrow(dat)))
  x <- safe_num(dat[[binary_col]])
  x == 0
}

raw_is_skip <- function(col) {
  if (!col %in% names(dat_raw)) return(rep(FALSE, nrow(dat)))
  y <- trimws(as.character(dat_raw[[col]]))
  is.na(dat_raw[[col]]) | y %in% c("", "NA", "N/A", "na", "Na", "None", "NULL", "null", "(skip)", "skip", "无", "(跳过)", "跳过")
}

make_product <- function(a, b, nm, binary_col = NULL, zero_if_both_missing = TRUE) {
  if (exists_col(a) && exists_col(b)) {
    ax <- safe_num(dat[[a]])
    bx <- safe_num(dat[[b]])
    product <- ax * bx
    both_missing <- is.na(ax) & is.na(bx)
    one_missing <- xor(is.na(ax), is.na(bx))
    no_overall <- is_no_binary(binary_col)
    both_raw_skip <- raw_is_skip(a) & raw_is_skip(b)

    product[no_overall & (both_missing | one_missing)] <- 0
    product[(both_raw_skip | (zero_if_both_missing & both_missing)) & !one_missing] <- 0
    product[one_missing & !no_overall] <- NA_real_

    dat[[nm]] <<- product
  }
}
make_product(vars$coffee_days, vars$coffee_daily_amount, "coffee_month_amount", vars$tea_binary)
make_product(vars$tea_days, vars$tea_daily_amount, "tea_month_amount", vars$tea_binary)
make_product(vars$yogurt_days, vars$yogurt_daily_amount, "yogurt_month_amount", vars$milk_binary)
make_product(vars$milk_days, vars$milk_daily_amount, "pure_milk_month_amount", vars$milk_binary)
make_product(vars$other_milk_days, vars$other_milk_daily_amount, "other_milk_month_amount", vars$milk_binary)
milk_components <- available(c("yogurt_month_amount", "pure_milk_month_amount", "other_milk_month_amount"))
if (length(milk_components) > 0) {
  mat <- dat[, milk_components, drop = FALSE]
  dat$milk_month_amount_total <- rowSums(mat, na.rm = TRUE)
  dat$milk_month_amount_total[rowSums(!is.na(mat)) == 0] <- NA
}

bev_vars <- available(c(
  vars$tea_binary, vars$milk_binary, "coffee_month_amount", "tea_month_amount",
  "yogurt_month_amount", "pure_milk_month_amount", "milk_month_amount_total"
))
outcomes <- c(HRPL = vars$hrpl, Fatigue = vars$fatigue, Depression = vars$depression)
bev_cor <- bind_rows(lapply(bev_vars, function(bv) {
  bind_rows(lapply(names(outcomes), function(lbl) cor_one(bv, outcomes[[lbl]], bv, lbl)))
}))
if (nrow(bev_cor) > 0) {
  bev_cor$p_fdr <- p.adjust(bev_cor$p_value, method = "BH")
  bev_cor$p_fdr_text <- vapply(bev_cor$p_fdr, fmt_p, character(1))
}
write_csv_utf8(bev_cor, "beverage_spearman_analysis.csv")

adjusted_bev_rows <- list()
for (bv in bev_vars) {
  for (lbl in names(outcomes)) {
    ov <- outcomes[[lbl]]
    d <- data.frame(Y = safe_num(dat[[ov]]), Beverage = safe_num(dat[[bv]]))
    for (cv in covariates) {
      d[[cv]] <- dat[[cv]]
      if (!is.numeric(d[[cv]]) || length(unique(d[[cv]][!is.na(d[[cv]])])) <= 10) {
        d[[cv]] <- as.factor(d[[cv]])
      }
    }
    d <- d[stats::complete.cases(d), , drop = FALSE]
    if (nrow(d) < 20 || length(unique(d$Beverage)) < 2) {
      adjusted_bev_rows[[length(adjusted_bev_rows) + 1]] <- data.frame(
        beverage = bv, outcome = lbl, n = nrow(d), estimate = NA_real_,
        conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
        p = NA_character_, adj_r_squared = NA_real_, note = "Too few rows or no variation"
      )
      next
    }
    f <- as.formula(paste("Y ~ Beverage +", rhs_terms(covariates)))
    fit <- tryCatch(lm(f, data = d), error = function(e) e)
    if (inherits(fit, "error")) {
      adjusted_bev_rows[[length(adjusted_bev_rows) + 1]] <- data.frame(
        beverage = bv, outcome = lbl, n = nrow(d), estimate = NA_real_,
        conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
        p = NA_character_, adj_r_squared = NA_real_, note = conditionMessage(fit)
      )
    } else {
      ci <- confint(fit)
      sm <- summary(fit)
      adjusted_bev_rows[[length(adjusted_bev_rows) + 1]] <- data.frame(
        beverage = bv, outcome = lbl, n = nrow(d),
        estimate = coef(fit)[["Beverage"]],
        conf_low = ci["Beverage", 1],
        conf_high = ci["Beverage", 2],
        p_value = sm$coefficients["Beverage", "Pr(>|t|)"],
        p = fmt_p(sm$coefficients["Beverage", "Pr(>|t|)"]),
        adj_r_squared = sm$adj.r.squared,
        note = ""
      )
    }
  }
}
adjusted_bev <- bind_rows(adjusted_bev_rows)
if (nrow(adjusted_bev) > 0) {
  adjusted_bev$p_fdr <- p.adjust(adjusted_bev$p_value, method = "BH")
  adjusted_bev$p_fdr_text <- vapply(adjusted_bev$p_fdr, fmt_p, character(1))
}
write_csv_utf8(adjusted_bev, "beverage_adjusted_models.csv")

subgroup_rows <- list()
for (sg in available(vars$subgroup_vars)) {
  v <- dat[[sg]]
  nonmiss <- v[!is.na(v)]
  tab <- sort(table(nonmiss), decreasing = TRUE)
  feasible <- length(tab) >= 2 && sum(as.integer(tab) >= args$min_subgroup_n) >= 2
  subgroup_rows[[length(subgroup_rows) + 1]] <- data.frame(
    variable = sg,
    non_missing_n = length(nonmiss),
    category_count = length(tab),
    categories_above_min_n = sum(as.integer(tab) >= args$min_subgroup_n),
    largest_categories = paste(paste0(names(tab), "=", as.integer(tab)), collapse = "; "),
    feasible_basic_check = feasible,
    note = if (feasible) "Subgroup interaction check planned" else "Sparse for stable subgroup interaction check"
  )
}
subgroup_feas <- bind_rows(subgroup_rows)
write_csv_utf8(subgroup_feas, "subgroup_feasibility.csv")

subgroup_interactions <- list()
for (sg in available(vars$subgroup_vars)) {
  d <- prepare_moderated_data(vars$support)
  d$G <- as.factor(dat[[sg]][as.integer(rownames(d))])
  d <- d[stats::complete.cases(d[, c("Yz", "Xz", "Mz", "Wz", "G"), drop = FALSE]), , drop = FALSE]
  good_levels <- names(table(d$G))[as.integer(table(d$G)) >= args$min_subgroup_n]
  d <- d[d$G %in% good_levels, , drop = FALSE]
  d$G <- droplevels(d$G)
  if (nrow(d) < args$min_subgroup_n * 2 || nlevels(d$G) < 2) {
    subgroup_interactions[[length(subgroup_interactions) + 1]] <- data.frame(
      subgroup = sg, n = nrow(d), term = NA_character_, estimate = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
      p = NA_character_, note = "Skipped due to sparse categories"
    )
    next
  }
  cov_for_model <- setdiff(covariates, sg)
  cov_rhs <- rhs_terms(cov_for_model)
  fit <- tryCatch(
    lm(as.formula(paste("Yz ~ Xz * Wz * G + Mz * Wz * G +", cov_rhs)), data = d),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    subgroup_interactions[[length(subgroup_interactions) + 1]] <- data.frame(
      subgroup = sg, n = nrow(d), term = NA_character_, estimate = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
      p = NA_character_, note = conditionMessage(fit)
    )
  } else {
    tt <- lm_tidy(fit)
    tt <- tt[grepl("Xz:Wz:G|Mz:Wz:G|Wz:Mz:G", tt$term), , drop = FALSE]
    if (nrow(tt) == 0) {
      tt <- data.frame(term = NA_character_, estimate = NA_real_, conf_low = NA_real_,
                       conf_high = NA_real_, p.value = NA_real_)
    }
    tt$subgroup <- sg
    tt$n <- nrow(d)
    tt$p <- vapply(tt$p.value, fmt_p, character(1))
    tt$note <- ""
    tt <- tt[, c("subgroup", "n", "term", "estimate", "conf_low", "conf_high", "p.value", "p", "note")]
    names(tt)[names(tt) == "p.value"] <- "p_value"
    subgroup_interactions[[length(subgroup_interactions) + 1]] <- tt
  }
}
write_csv_utf8(bind_rows(subgroup_interactions), "subgroup_interactions.csv")

sink(file.path(out_dir, "analysis_console_summary.txt"), split = FALSE)
cat("Reviewer-requested full-data analysis\n")
cat("Input:", input_path, "\n")
cat("Output directory:", normalizePath(out_dir, winslash = "/", mustWork = FALSE), "\n")
cat("Rows x columns:", nrow(dat), "x", ncol(dat), "\n")
cat("Bootstrap resamples:", args$bootstrap, "\n")
cat("Covariates used when available:", paste(covariates, collapse = ", "), "\n\n")
cat("Core correlations:\n")
print(cor_results)
cat("\nReliability alpha:\n")
print(alpha_results)
cat("\nCommon method bias Harman:\n")
print(common_method)
cat("\nCFA:\n")
print(cfa_results)
cat("\nMediation:\n")
print(mediation_results)
cat("\nModerated mediation interactions:\n")
print(overall_mod$interactions)
cat("\nConditional indirect effects:\n")
print(overall_mod$conditional)
cat("\nSupport dimension moderation:\n")
print(dimension_interactions)
cat("\nBeverage correlations:\n")
print(bev_cor)
cat("\nBeverage adjusted models:\n")
print(adjusted_bev)
cat("\nSubgroup feasibility:\n")
print(subgroup_feas)
sink()

manifest <- c(
  "# Analysis Outputs",
  paste0("Generated: ", Sys.Date()),
  "Study type: cross-sectional reviewer-response analysis",
  "",
  "## Tables",
  "- `data_shape.csv` -- Dataset dimensions.",
  "- `missing_values.csv` -- Missingness and uniqueness by variable.",
  "- `core_variable_overview.csv` -- Missingness for analysis variables.",
  "- `reliability_alpha.csv` -- Cronbach alpha for main scales.",
  "- `common_method_bias_harman.csv` -- Harman single-factor PCA result.",
  "- `common_method_bias_cfa.csv` -- One-factor and four-factor CFA fit indices.",
  "- `correlation_core_spearman.csv` -- Core Spearman correlations with FDR p values.",
  "- `mediation_results.csv` -- Linear mediation estimates with bootstrap indirect-effect CI.",
  "- `moderated_mediation_interactions.csv` -- Interaction terms and delta R-squared.",
  "- `moderated_mediation_conditional_indirect.csv` -- Conditional indirect effects.",
  "- `support_dimension_moderation.csv` -- PSSS dimension moderation checks.",
  "- `beverage_spearman_analysis.csv` -- Beverage frequency/dose correlations.",
  "- `beverage_adjusted_models.csv` -- Covariate-adjusted beverage linear models.",
  "- `subgroup_feasibility.csv` -- Candidate subgroup sample-size feasibility.",
  "- `subgroup_interactions.csv` -- Formal subgroup interaction checks.",
  "- `analysis_console_summary.txt` -- Console summary.",
  "",
  "## Notes",
  "- Interpret cross-sectional mediation as association-level evidence.",
  "- Report standardized beta, 95% CI, exact p value, and delta R-squared for interaction effects.",
  "- Review `common_method_bias_cfa.csv` for convergence before citing CFA results."
)
writeLines(manifest, file.path(out_dir, "_analysis_outputs.md"), useBytes = TRUE)

cat("Analysis completed.\n")
cat(normalizePath(out_dir, winslash = "/", mustWork = FALSE), "\n")
