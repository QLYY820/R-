# Bastion/RStudio Stage 8 resume script.
#
# Run this only after run_bastion_pipeline_from_data.R has completed stages 6/7.
# It reads existing stage6 model/data outputs and reruns only sensitivity/subgroup analyses.
#
# Usage:
#   BASTION_OUTPUT_DIR <- "nurse_psych_profiles_bastion_output_full"
#   source("https://raw.githubusercontent.com/QLYY820/R-/codex/shift-sleep-depression-code/scripts/run_bastion_stage8_from_existing_outputs.R")

options(stringsAsFactors = FALSE)

output_root <- if (exists("BASTION_OUTPUT_DIR", envir = .GlobalEnv)) {
  get("BASTION_OUTPUT_DIR", envir = .GlobalEnv)
} else {
  file.path(getwd(), "nurse_psych_profiles_bastion_output_full")
}
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)

dirs <- c(
  file.path(output_root, "results/tables"),
  file.path(output_root, "results/models"),
  file.path(output_root, "results/figures"),
  file.path(output_root, "results/logs")
)
for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(output_root, "results/logs/08_sensitivity_subgroup_analysis_log.txt")
if (file.exists(log_path)) unlink(log_path)
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  cat(msg, "\n", file = log_path, append = TRUE)
}

maybe_install <- isTRUE(get0("BASTION_INSTALL_PACKAGES", ifnotfound = FALSE, envir = .GlobalEnv))
need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (maybe_install) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    } else {
      stop(
        "Required package missing: ", pkg,
        ". Install it first or set BASTION_INSTALL_PACKAGES <- TRUE before source().",
        call. = FALSE
      )
    }
  }
}
for (pkg in c("ggplot2", "glmnet", "pROC", "Matrix")) need_pkg(pkg)

as_num <- function(x) suppressWarnings(as.numeric(x))
clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- NA_character_
  x
}
existing <- function(df, vars) intersect(vars, names(df))
empty_table <- function(note = "No available data.") data.frame(note = note, stringsAsFactors = FALSE)
round_df <- function(x, digits = 5) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(x) == 0 || ncol(x) == 0) return(empty_table())
  num_cols <- vapply(x, is.numeric, logical(1))
  x[num_cols] <- lapply(x[num_cols], round, digits = digits)
  x
}
write_workbook <- function(sheets, path) {
  sheets <- lapply(sheets, round_df)
  sheet_names <- make.unique(substr(names(sheets), 1, 31), sep = "_")
  if (file.exists(path)) unlink(path)
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    for (i in seq_along(sheets)) {
      openxlsx::addWorksheet(wb, sheet_names[i])
      openxlsx::writeData(wb, sheet_names[i], sheets[[i]])
      if (ncol(sheets[[i]]) > 0) {
        try(openxlsx::setColWidths(wb, sheet_names[i], cols = seq_len(ncol(sheets[[i]])), widths = "auto"), silent = TRUE)
      }
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  } else {
    csv_dir <- sub("\\.xlsx$", "_csv", path, ignore.case = TRUE)
    if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
    for (i in seq_along(sheets)) {
      utils::write.csv(sheets[[i]], file.path(csv_dir, paste0(sheet_names[i], ".csv")), row.names = FALSE)
    }
    log_msg("No openxlsx package found; wrote CSV sheets to:", csv_dir)
  }
}

average_precision <- function(y, p) {
  ok <- !is.na(y) & !is.na(p)
  y <- as.integer(y[ok])
  p <- p[ok]
  if (length(unique(y)) < 2 || sum(y == 1) == 0) return(NA_real_)
  ord <- order(p, decreasing = TRUE)
  y <- y[ord]
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  precision <- tp / pmax(tp + fp, 1)
  recall_step <- y / sum(y == 1)
  sum(precision * recall_step)
}

classification_metrics <- function(y, p, threshold) {
  ok <- !is.na(y) & !is.na(p)
  y <- as.integer(y[ok])
  p <- p[ok]
  pred <- as.integer(p >= threshold)
  tp <- sum(pred == 1 & y == 1)
  tn <- sum(pred == 0 & y == 0)
  fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1)
  data.frame(
    threshold = threshold,
    tp = tp,
    fp = fp,
    tn = tn,
    fn = fn,
    sensitivity = if ((tp + fn) > 0) tp / (tp + fn) else NA_real_,
    specificity = if ((tn + fp) > 0) tn / (tn + fp) else NA_real_,
    accuracy = if (length(y) > 0) (tp + tn) / length(y) else NA_real_,
    ppv = if ((tp + fp) > 0) tp / (tp + fp) else NA_real_,
    npv = if ((tn + fn) > 0) tn / (tn + fn) else NA_real_,
    f1 = if ((2 * tp + fp + fn) > 0) 2 * tp / (2 * tp + fp + fn) else NA_real_,
    stringsAsFactors = FALSE
  )
}

model_performance <- function(split, y, p, threshold) {
  y <- as.integer(y)
  ok <- !is.na(y) & !is.na(p)
  y <- y[ok]
  p <- p[ok]
  if (length(unique(y)) < 2) return(empty_table(paste("Only one outcome class in", split)))
  roc_obj <- pROC::roc(y, p, quiet = TRUE, levels = c(0, 1), direction = "<")
  cls <- classification_metrics(y, p, threshold)
  data.frame(
    split = split,
    n = length(y),
    positives = sum(y == 1),
    prevalence = mean(y == 1),
    auc = as.numeric(pROC::auc(roc_obj)),
    pr_auc_average_precision = average_precision(y, p),
    brier = mean((p - y)^2),
    cls,
    stringsAsFactors = FALSE
  )
}

auc_with_ci <- function(y, p) {
  y <- as.integer(y)
  ok <- !is.na(y) & !is.na(p)
  y <- y[ok]
  p <- p[ok]
  if (length(unique(y)) < 2) return(c(auc = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_))
  roc_obj <- pROC::roc(y, p, quiet = TRUE, levels = c(0, 1), direction = "<")
  ci <- tryCatch(as.numeric(pROC::ci.auc(roc_obj)), error = function(e) c(NA_real_, NA_real_, NA_real_))
  c(auc = as.numeric(pROC::auc(roc_obj)), ci_lower = ci[1], ci_upper = ci[3])
}

find_raw_predictor <- function(feature, final_predictors) {
  hits <- final_predictors[vapply(final_predictors, function(p) feature == p || startsWith(feature, p), logical(1))]
  if (length(hits) == 0) return(NA_character_)
  hits[which.max(nchar(hits))]
}

learn_preprocess <- function(train_df, candidate_vars, max_missing_prop = 0.5, max_factor_levels = 30) {
  specs <- list()
  excluded <- data.frame(variable = character(), reason = character(), stringsAsFactors = FALSE)
  symptom_level_pattern <- "(睡眠|失眠|抑郁|焦虑|精神|心理|疲劳|疲乏|压力|倦怠|sleep|insomnia|depress|anxiety|stress|burnout|fatigue)"
  for (var in candidate_vars) {
    x <- train_df[[var]]
    missing_prop <- mean(is.na(x) | (is.character(x) & trimws(x) == ""))
    if (missing_prop > max_missing_prop) {
      excluded <- rbind(excluded, data.frame(variable = var, reason = "training_missing_prop_gt_0.50"))
      next
    }
    if (is.logical(x)) x <- as.integer(x)
    if (is.numeric(x) || is.integer(x)) {
      xn <- as_num(x)
      non_missing <- xn[!is.na(xn)]
      if (length(unique(non_missing)) < 2) {
        excluded <- rbind(excluded, data.frame(variable = var, reason = "numeric_zero_variance"))
        next
      }
      specs[[var]] <- list(type = "numeric", median = stats::median(non_missing, na.rm = TRUE))
      next
    }
    xc <- clean_chr(x)
    non_missing_chr <- xc[!is.na(xc)]
    if (length(non_missing_chr) > 0 && any(grepl(symptom_level_pattern, non_missing_chr, ignore.case = TRUE))) {
      excluded <- rbind(excluded, data.frame(variable = var, reason = "factor_contains_symptom_level"))
      next
    }
    xnum <- suppressWarnings(as.numeric(xc))
    parse_prop <- if (length(non_missing_chr) > 0) mean(!is.na(xnum[!is.na(xc)])) else 0
    if (parse_prop >= 0.95) {
      non_missing_num <- xnum[!is.na(xnum)]
      if (length(unique(non_missing_num)) < 2) {
        excluded <- rbind(excluded, data.frame(variable = var, reason = "numeric_like_zero_variance"))
        next
      }
      specs[[var]] <- list(type = "numeric_from_character", median = stats::median(non_missing_num, na.rm = TRUE))
      next
    }
    n_levels <- length(unique(non_missing_chr))
    if (n_levels < 2) {
      excluded <- rbind(excluded, data.frame(variable = var, reason = "factor_zero_variance"))
      next
    }
    if (n_levels > max_factor_levels) {
      excluded <- rbind(excluded, data.frame(variable = var, reason = "factor_too_many_levels"))
      next
    }
    tab <- sort(table(non_missing_chr), decreasing = TRUE)
    keep_levels <- names(tab)[seq_len(min(length(tab), 25))]
    specs[[var]] <- list(type = "factor", keep_levels = keep_levels, levels = unique(c(keep_levels, "Other", "Missing")))
  }
  list(specs = specs, excluded = excluded)
}

apply_preprocess <- function(df, preprocess) {
  out <- data.frame(row_id_internal = seq_len(nrow(df)))
  for (var in names(preprocess$specs)) {
    spec <- preprocess$specs[[var]]
    if (spec$type %in% c("numeric", "numeric_from_character")) {
      x <- as_num(df[[var]])
      x[is.na(x)] <- spec$median
      out[[var]] <- x
    } else {
      x <- clean_chr(df[[var]])
      x[is.na(x)] <- "Missing"
      x[!(x %in% spec$keep_levels) & x != "Missing"] <- "Other"
      out[[var]] <- factor(x, levels = spec$levels)
    }
  }
  out$row_id_internal <- NULL
  out
}

fit_lasso_pipeline <- function(model_df, y, candidate_vars, seed = 20260612) {
  y <- as.integer(y)
  positive_idx <- which(y == 1)
  negative_idx <- which(y == 0)
  if (length(positive_idx) < 20 || length(negative_idx) < 20) {
    stop("Too few events/non-events for LASSO modelling.", call. = FALSE)
  }
  set.seed(seed)
  train_idx <- sort(c(
    sample(positive_idx, size = floor(length(positive_idx) * 0.70)),
    sample(negative_idx, size = floor(length(negative_idx) * 0.70))
  ))
  test_idx <- setdiff(seq_len(nrow(model_df)), train_idx)
  train_df <- model_df[train_idx, , drop = FALSE]
  test_df <- model_df[test_idx, , drop = FALSE]
  y_train <- y[train_idx]
  y_test <- y[test_idx]

  preprocess <- learn_preprocess(train_df, candidate_vars)
  x_train <- Matrix::sparse.model.matrix(~ . - 1, data = apply_preprocess(train_df, preprocess))
  x_test <- Matrix::sparse.model.matrix(~ . - 1, data = apply_preprocess(test_df, preprocess))
  nonzero_train_cols <- Matrix::colSums(abs(x_train)) > 0
  x_train <- x_train[, nonzero_train_cols, drop = FALSE]
  x_test <- x_test[, colnames(x_train), drop = FALSE]

  foldid <- rep(NA_integer_, length(y_train))
  for (cls in c(0, 1)) {
    cls_idx <- which(y_train == cls)
    foldid[cls_idx] <- sample(rep(seq_len(5), length.out = length(cls_idx)))
  }
  set.seed(seed)
  cv_fit <- glmnet::cv.glmnet(
    x_train, y_train,
    family = "binomial",
    alpha = 1,
    type.measure = "auc",
    nfolds = 5,
    foldid = foldid,
    standardize = TRUE,
    nlambda = 60,
    parallel = FALSE
  )
  train_pred <- as.numeric(stats::predict(cv_fit, newx = x_train, s = "lambda.1se", type = "response"))
  test_pred <- as.numeric(stats::predict(cv_fit, newx = x_test, s = "lambda.1se", type = "response"))
  train_roc <- pROC::roc(y_train, train_pred, quiet = TRUE, levels = c(0, 1), direction = "<")
  threshold <- as.numeric(pROC::coords(train_roc, x = "best", best.method = "youden", ret = "threshold")[1])
  if (is.na(threshold) || !is.finite(threshold)) threshold <- 0.5

  perf <- rbind(
    model_performance("train", y_train, train_pred, threshold),
    model_performance("test", y_test, test_pred, threshold)
  )
  perf$model <- "strict_lasso_logistic_lambda_1se"
  perf <- perf[, c("model", setdiff(names(perf), "model"))]

  coef_mat <- as.matrix(stats::coef(cv_fit, s = "lambda.1se"))
  coef_table <- data.frame(term = rownames(coef_mat), coefficient = as.numeric(coef_mat[, 1]), stringsAsFactors = FALSE)
  coef_table <- coef_table[coef_table$term != "(Intercept)" & coef_table$coefficient != 0, , drop = FALSE]
  coef_table$abs_coefficient <- abs(coef_table$coefficient)
  coef_table$raw_predictor <- vapply(coef_table$term, find_raw_predictor, character(1), final_predictors = candidate_vars)
  coef_table <- coef_table[order(-coef_table$abs_coefficient, coef_table$term), , drop = FALSE]
  rownames(coef_table) <- NULL

  list(
    fit = cv_fit,
    threshold = threshold,
    train_idx = train_idx,
    test_idx = test_idx,
    y_train = y_train,
    y_test = y_test,
    train_pred = train_pred,
    test_pred = test_pred,
    performance = perf,
    coefficients = coef_table,
    preprocess = preprocess,
    retained_model_columns = colnames(x_train)
  )
}

make_subgroup_frame <- function(test_df) {
  subgroup_df <- data.frame(row_id_internal = seq_len(nrow(test_df)))
  if ("age" %in% names(test_df)) {
    age <- as_num(test_df$age)
    subgroup_df$age_group <- cut(age, breaks = c(-Inf, 29, 39, 49, Inf), labels = c("<30", "30-39", "40-49", ">=50"), right = TRUE)
  }
  if ("BMI" %in% names(test_df)) {
    bmi <- as_num(test_df$BMI)
    subgroup_df$BMI_group <- cut(bmi, breaks = c(-Inf, 18.5, 24, 28, Inf), labels = c("<18.5", "18.5-23.9", "24.0-27.9", ">=28.0"), right = FALSE)
  }
  raw_names <- names(test_df)
  pattern_vars <- unique(c(
    grep("sex|gender|xingbie|A_q2|性别", raw_names, value = TRUE, ignore.case = TRUE),
    grep("night|shift|yeban|daoban|夜班|倒班", raw_names, value = TRUE, ignore.case = TRUE),
    grep("dept|department|keshi|title|zhicheng|hospital|grade|医院|科室|职称|等级", raw_names, value = TRUE, ignore.case = TRUE)
  ))
  for (v in pattern_vars) {
    x <- clean_chr(test_df[[v]])
    n_levels <- length(unique(x[!is.na(x)]))
    if (n_levels >= 2 && n_levels <= 12) subgroup_df[[v]] <- factor(x)
  }
  subgroup_df$row_id_internal <- NULL
  subgroup_df
}

run_subgroup_analysis <- function(test_df, y_test, pred_test, threshold) {
  subgroup_df <- make_subgroup_frame(test_df)
  rows <- list()
  for (v in names(subgroup_df)) {
    x <- subgroup_df[[v]]
    for (lev in unique(as.character(x[!is.na(x)]))) {
      idx <- which(as.character(x) == lev)
      if (length(idx) < 50 || length(unique(y_test[idx])) < 2 || sum(y_test[idx] == 1) < 5) next
      auc_ci <- auc_with_ci(y_test[idx], pred_test[idx])
      cls <- classification_metrics(y_test[idx], pred_test[idx], threshold)
      rows[[length(rows) + 1]] <- data.frame(
        subgroup_variable = v,
        subgroup_level = lev,
        n = length(idx),
        positives = sum(y_test[idx] == 1),
        event_rate = mean(y_test[idx] == 1),
        auc = auc_ci["auc"],
        auc_ci_lower = auc_ci["ci_lower"],
        auc_ci_upper = auc_ci["ci_upper"],
        pr_auc_average_precision = average_precision(y_test[idx], pred_test[idx]),
        brier = mean((pred_test[idx] - y_test[idx])^2),
        sensitivity = cls$sensitivity,
        specificity = cls$specificity,
        ppv = cls$ppv,
        npv = cls$npv,
        f1 = cls$f1,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) empty_table("No eligible subgroup strata.") else do.call(rbind, rows)
}

safe_fit_sensitivity <- function(label, expr) {
  log_msg("Starting:", label)
  out <- tryCatch(expr, error = function(e) {
    log_msg("Failed:", label, "-", conditionMessage(e))
    structure(list(error = conditionMessage(e)), class = "stage8_fit_error")
  })
  if (!inherits(out, "stage8_fit_error")) log_msg("Finished:", label)
  out
}

plot_stage8 <- function(sensitivity_perf, subgroup_metrics, output_root) {
  sens_test <- sensitivity_perf[sensitivity_perf$split == "test" & !is.na(sensitivity_perf$auc), , drop = FALSE]
  if (nrow(sens_test) > 0) {
    sens_test$analysis_label <- factor(sens_test$analysis_label, levels = rev(sens_test$analysis_label))
    p_auc <- ggplot2::ggplot(sens_test, ggplot2::aes(analysis_label, auc)) +
      ggplot2::geom_col(fill = "#2563EB") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::labs(title = "Sensitivity Analysis: AUC Comparison", x = NULL, y = "AUC")
    ggplot2::ggsave(file.path(output_root, "results/figures/stage8_sensitivity_auc_comparison.png"), p_auc, width = 8.5, height = 5.5, dpi = 300)

    p_pr <- ggplot2::ggplot(sens_test, ggplot2::aes(analysis_label, pr_auc_average_precision)) +
      ggplot2::geom_col(fill = "#0F766E") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::labs(title = "Sensitivity Analysis: PR-AUC Comparison", x = NULL, y = "PR-AUC")
    ggplot2::ggsave(file.path(output_root, "results/figures/stage8_sensitivity_pr_auc_comparison.png"), p_pr, width = 8.5, height = 5.5, dpi = 300)
  }

  if (!is.null(subgroup_metrics) && nrow(subgroup_metrics) > 0 && !"note" %in% names(subgroup_metrics)) {
    sg <- subgroup_metrics[order(subgroup_metrics$auc), , drop = FALSE]
    sg$label <- paste(sg$subgroup_variable, sg$subgroup_level, sep = ": ")
    sg$label <- factor(sg$label, levels = sg$label)
    p_forest <- ggplot2::ggplot(sg, ggplot2::aes(auc, label)) +
      ggplot2::geom_segment(ggplot2::aes(x = auc_ci_lower, xend = auc_ci_upper, y = label, yend = label), color = "grey55", na.rm = TRUE) +
      ggplot2::geom_point(color = "#7C3AED", size = 2) +
      ggplot2::theme_minimal(base_size = 9) +
      ggplot2::labs(title = "Subgroup AUC", x = "AUC", y = NULL)
    ggplot2::ggsave(file.path(output_root, "results/figures/stage8_subgroup_auc_forest.png"), p_forest, width = 8.5, height = max(5.5, min(14, 0.22 * nrow(sg) + 2)), dpi = 300)

    ev <- sg[order(-sg$event_rate), , drop = FALSE]
    ev$label <- factor(ev$label, levels = rev(ev$label))
    p_event <- ggplot2::ggplot(ev, ggplot2::aes(label, event_rate)) +
      ggplot2::geom_col(fill = "#DC2626") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 9) +
      ggplot2::labs(title = "Subgroup Event Rate", x = NULL, y = "Observed high-risk rate")
    ggplot2::ggsave(file.path(output_root, "results/figures/stage8_subgroup_event_rate.png"), p_event, width = 8.5, height = max(5.5, min(14, 0.22 * nrow(ev) + 2)), dpi = 300)
  }
}

model_path <- file.path(output_root, "results/models/stage6_binary_prediction_models.rds")
data_path <- file.path(output_root, "data/processed/prediction_binary_data.rds")
if (!file.exists(model_path)) stop("Missing file: ", model_path, call. = FALSE)
if (!file.exists(data_path)) stop("Missing file: ", data_path, call. = FALSE)

log_msg("Stage 8 resume started. Output root:", output_root)
stage6_model <- readRDS(model_path)
prediction_data <- readRDS(data_path)
model_df <- prediction_data[!is.na(prediction_data$high_risk_class), , drop = FALSE]
y <- as.integer(model_df$high_risk_class)
candidate_vars <- stage6_model$final_predictors
if (is.null(candidate_vars) || length(candidate_vars) == 0) {
  candidate_vars <- unique(na.omit(stage6_model$coefficients$raw_predictor))
}
candidate_vars <- existing(model_df, candidate_vars)
class_burden <- stage6_model$class_burden
performance <- stage6_model$performance

sensitivity_rows <- list()
sensitivity_models <- list()
if (!is.null(performance) && nrow(performance) > 0) {
  sensitivity_rows[["primary"]] <- cbind(analysis_label = "primary early-identification model", performance[performance$split == "test", , drop = FALSE])
}
sensitivity_models[["primary"]] <- list(note = "Main strict model object is saved in stage6_binary_prediction_models.rds.")

top2_classes <- class_burden$LPA_class[seq_len(min(2, nrow(class_burden)))]
alt_y <- ifelse(is.na(model_df$LPA_class), NA_integer_, as.integer(model_df$LPA_class %in% top2_classes))
ok <- !is.na(alt_y)
if (length(unique(alt_y[ok])) == 2) {
  fit_alt <- safe_fit_sensitivity("alternative high-risk definition: top 2 LPA burden classes", fit_lasso_pipeline(model_df[ok, , drop = FALSE], alt_y[ok], candidate_vars, seed = 20260612))
  if (!inherits(fit_alt, "stage8_fit_error")) {
    sensitivity_rows[["top2"]] <- cbind(analysis_label = "alternative high-risk definition: top 2 LPA burden classes", fit_alt$performance[fit_alt$performance$split == "test", , drop = FALSE])
    sensitivity_models[["top2_lpa_burden_classes"]] <- fit_alt
  }
}

if ("lpa_burden_score" %in% names(model_df)) {
  q75 <- as.numeric(stats::quantile(model_df$lpa_burden_score, 0.75, na.rm = TRUE))
  alt2_y <- ifelse(is.na(model_df$lpa_burden_score), NA_integer_, as.integer(model_df$lpa_burden_score >= q75))
  ok2 <- !is.na(alt2_y)
  if (length(unique(alt2_y[ok2])) == 2) {
    fit_alt2 <- safe_fit_sensitivity("alternative high-risk definition: lpa_burden_score top quartile", fit_lasso_pipeline(model_df[ok2, , drop = FALSE], alt2_y[ok2], candidate_vars, seed = 20260613))
    if (!inherits(fit_alt2, "stage8_fit_error")) {
      sensitivity_rows[["burden_q75"]] <- cbind(analysis_label = "alternative high-risk definition: lpa_burden_score top quartile", fit_alt2$performance[fit_alt2$performance$split == "test", , drop = FALSE])
      sensitivity_models[["lpa_burden_top_quartile"]] <- fit_alt2
    }
  }
} else {
  log_msg("Skipped lpa_burden_score top quartile model because lpa_burden_score is absent.")
}

if ("LPA_probability" %in% names(model_df)) {
  ok_hc <- !is.na(model_df$LPA_probability) & model_df$LPA_probability >= 0.70
  if (sum(ok_hc) > 100 && length(unique(y[ok_hc])) == 2) {
    fit_hc <- safe_fit_sensitivity("high-confidence LPA sample: posterior probability >= 0.70", fit_lasso_pipeline(model_df[ok_hc, , drop = FALSE], y[ok_hc], candidate_vars, seed = 20260614))
    if (!inherits(fit_hc, "stage8_fit_error")) {
      sensitivity_rows[["high_conf"]] <- cbind(analysis_label = "high-confidence LPA sample: posterior probability >= 0.70", fit_hc$performance[fit_hc$performance$split == "test", , drop = FALSE])
      sensitivity_models[["high_confidence_lpa"]] <- fit_hc
    }
  }
}

enhanced_vars <- unique(c(candidate_vars, existing(model_df, c("gad7_total", "phq9_total", "psqi_total", "ess_total", "pss_total", "mbi_ee_total", "mbi_dp_total", "mbi_low_pa_total"))))
fit_enh <- safe_fit_sensitivity("enhanced screening model with symptom scores", fit_lasso_pipeline(model_df, y, enhanced_vars, seed = 20260615))
if (!inherits(fit_enh, "stage8_fit_error")) {
  sensitivity_rows[["enhanced"]] <- cbind(analysis_label = "enhanced screening model with symptom scores", fit_enh$performance[fit_enh$performance$split == "test", , drop = FALSE])
  sensitivity_models[["enhanced_screening_symptom_scores"]] <- fit_enh
}

if (length(sensitivity_rows) == 0) {
  sensitivity_perf <- empty_table("No sensitivity models completed.")
} else {
  sensitivity_perf <- do.call(rbind, sensitivity_rows)
  rownames(sensitivity_perf) <- NULL
}
sensitivity_models$performance <- sensitivity_perf

test_df <- model_df[model_df$binary_prediction_split == "test" & !is.na(model_df$binary_prediction_probability), , drop = FALSE]
if (nrow(test_df) > 0) {
  subgroup_metrics <- run_subgroup_analysis(
    test_df,
    as.integer(test_df$high_risk_class),
    as_num(test_df$binary_prediction_probability),
    stage6_model$best_threshold_youden
  )
} else {
  subgroup_metrics <- empty_table("No saved test-set predictions found for subgroup analysis.")
}

saveRDS(sensitivity_models, file.path(output_root, "results/models/stage8_sensitivity_models.rds"))
write_workbook(
  list(
    sensitivity_performance = sensitivity_perf,
    subgroup_metrics = subgroup_metrics,
    analysis_note = data.frame(
      note = c(
        "Primary model is the strict early-identification LASSO logistic model.",
        "Sensitivity models and enhanced screening model are secondary analyses only.",
        "This resume script does not rerun cleaning, scoring, LPA, stage 6, or stage 7."
      ),
      stringsAsFactors = FALSE
    )
  ),
  file.path(output_root, "results/tables/stage8_sensitivity_subgroup_analysis.xlsx")
)
plot_stage8(sensitivity_perf, subgroup_metrics, output_root)

log_msg("Stage 8 sensitivity and subgroup analysis finished.")
log_msg("Sensitivity rows:", nrow(sensitivity_perf), "Subgroup rows:", nrow(subgroup_metrics))
print(data.frame(item = c("stage8_status", "sensitivity_rows", "subgroup_rows"), value = c("finished", nrow(sensitivity_perf), nrow(subgroup_metrics))))
