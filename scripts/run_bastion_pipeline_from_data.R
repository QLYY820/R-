# Bastion/RStudio single-file pipeline.
#
# Usage in the bastion RStudio session:
#   data <- <your final deduplicated data.frame>
#   BASTION_OUTPUT_DIR <- "nurse_psych_profiles_bastion_output"  # optional
#   BASTION_RUN_SENSITIVITY <- FALSE                             # optional, TRUE is slower
#   source("https://raw.githubusercontent.com/<owner>/<repo>/<branch>/scripts/run_bastion_pipeline_from_data.R")
#
# This script starts from the in-memory object named `data`.
# It does not read local Excel files and does not require the Windows project path.

options(stringsAsFactors = FALSE)

if (!exists("data", envir = .GlobalEnv)) {
  stop("Object `data` was not found in the global environment. Please create data first.", call. = FALSE)
}

output_root <- if (exists("BASTION_OUTPUT_DIR", envir = .GlobalEnv)) {
  get("BASTION_OUTPUT_DIR", envir = .GlobalEnv)
} else {
  file.path(getwd(), "nurse_psych_profiles_bastion_output")
}
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)

dirs <- c(
  file.path(output_root, "data/processed"),
  file.path(output_root, "results/tables"),
  file.path(output_root, "results/models"),
  file.path(output_root, "results/figures"),
  file.path(output_root, "results/figures/lpa_profiles"),
  file.path(output_root, "results/figures/binary_prediction"),
  file.path(output_root, "results/logs")
)
for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(output_root, "results/logs/bastion_pipeline_log.txt")
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

for (pkg in c("ggplot2", "mclust", "glmnet", "pROC", "Matrix")) need_pkg(pkg)
suppressPackageStartupMessages(library(mclust))

as_num <- function(x) suppressWarnings(as.numeric(x))
clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- NA_character_
  x
}
existing <- function(df, vars) intersect(vars, names(df))
extract_year <- function(x) {
  y <- suppressWarnings(as.integer(substr(as.character(x), 1, 4)))
  y[y < 1900 | y > 2100] <- NA_integer_
  y
}

empty_table <- function(note = "No available data.") {
  data.frame(note = note, stringsAsFactors = FALSE)
}

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
      openxlsx::setColWidths(wb, sheet_names[i], cols = seq_along(sheets[[i]]), widths = "auto")
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  } else if (requireNamespace("xlsx", quietly = TRUE)) {
    first <- TRUE
    for (i in seq_along(sheets)) {
      xlsx::write.xlsx(sheets[[i]], path, sheetName = sheet_names[i], row.names = FALSE, append = !first)
      first <- FALSE
    }
  } else {
    csv_dir <- sub("\\.xlsx$", "_csv", path, ignore.case = TRUE)
    if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
    for (i in seq_along(sheets)) {
      utils::write.csv(sheets[[i]], file.path(csv_dir, paste0(sheet_names[i], ".csv")), row.names = FALSE)
    }
    log_msg("No openxlsx/xlsx package found; wrote CSV sheets to:", csv_dir)
  }
}

summarise_numeric <- function(df, vars, group = "numeric") {
  rows <- lapply(vars, function(v) {
    x <- as_num(df[[v]])
    data.frame(
      group = group,
      variable = v,
      n_total = length(x),
      n_non_missing = sum(!is.na(x)),
      n_missing = sum(is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      q1 = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
      q3 = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
      min = suppressWarnings(min(x, na.rm = TRUE)),
      max = suppressWarnings(max(x, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) empty_table() else do.call(rbind, rows)
}

row_sum_complete <- function(df, items, score_min = NULL, score_max = NULL, reverse_items = character()) {
  item_matrix <- as.data.frame(lapply(df[, items, drop = FALSE], as_num))
  for (item in intersect(reverse_items, names(item_matrix))) {
    item_matrix[[item]] <- score_max + score_min - item_matrix[[item]]
  }
  rowSums(item_matrix, na.rm = FALSE)
}

cronbach_alpha <- function(df, items, score_min = NULL, score_max = NULL, reverse_items = character()) {
  items <- existing(df, items)
  if (length(items) < 2) return(NA_real_)
  item_matrix <- as.data.frame(lapply(df[, items, drop = FALSE], as_num))
  for (item in intersect(reverse_items, names(item_matrix))) {
    item_matrix[[item]] <- score_max + score_min - item_matrix[[item]]
  }
  item_matrix <- item_matrix[stats::complete.cases(item_matrix), , drop = FALSE]
  if (nrow(item_matrix) < 2 || ncol(item_matrix) < 2) return(NA_real_)
  item_vars <- vapply(item_matrix, stats::var, numeric(1), na.rm = TRUE)
  total_var <- stats::var(rowSums(item_matrix), na.rm = TRUE)
  k <- ncol(item_matrix)
  if (is.na(total_var) || total_var <= 0) return(NA_real_)
  as.numeric(k / (k - 1) * (1 - sum(item_vars, na.rm = TRUE) / total_var))
}

reliability_row <- function(df, scale, items, score_min, score_max, reverse_items = character(), note = "") {
  present <- existing(df, items)
  data.frame(
    scale = scale,
    requested_items = length(items),
    available_items = length(present),
    missing_items = paste(setdiff(items, names(df)), collapse = " | "),
    n_complete = if (length(present) > 0) sum(stats::complete.cases(df[, present, drop = FALSE])) else 0L,
    cronbach_alpha = cronbach_alpha(df, items, score_min, score_max, reverse_items),
    note = note,
    stringsAsFactors = FALSE
  )
}

make_indicator <- function(x, threshold, op = c(">=", ">", "<=", "<")) {
  op <- match.arg(op)
  x <- as_num(x)
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  out[ok] <- switch(
    op,
    ">=" = as.integer(x[ok] >= threshold),
    ">" = as.integer(x[ok] > threshold),
    "<=" = as.integer(x[ok] <= threshold),
    "<" = as.integer(x[ok] < threshold)
  )
  out
}

indicator_summary <- function(df, vars) {
  rows <- lapply(vars, function(v) {
    x <- df[[v]]
    data.frame(
      indicator = v,
      n_valid = sum(!is.na(x)),
      n_positive = sum(x == 1, na.rm = TRUE),
      prevalence = mean(x == 1, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) empty_table() else do.call(rbind, rows)
}

binary_jaccard <- function(binary_df) {
  vars <- names(binary_df)
  mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
  for (i in seq_along(vars)) for (j in seq_along(vars)) {
    xi <- binary_df[[i]]
    xj <- binary_df[[j]]
    ok <- !is.na(xi) & !is.na(xj)
    both <- sum(xi[ok] == 1 & xj[ok] == 1)
    either <- sum(xi[ok] == 1 | xj[ok] == 1)
    mat[i, j] <- if (either > 0) both / either else NA_real_
  }
  mat
}

binary_phi <- function(binary_df) {
  vars <- names(binary_df)
  mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
  for (i in seq_along(vars)) for (j in seq_along(vars)) {
    xi <- binary_df[[i]]
    xj <- binary_df[[j]]
    ok <- !is.na(xi) & !is.na(xj)
    if (sum(ok) < 2) next
    mat[i, j] <- suppressWarnings(stats::cor(as_num(xi[ok]), as_num(xj[ok]), method = "pearson"))
  }
  mat
}

matrix_to_long <- function(mat, value_name = "value") {
  if (length(mat) == 0) return(empty_table())
  out <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(out) <- c("var1", "var2", value_name)
  out
}

plot_heatmap <- function(mat, path, title, legend_name) {
  if (length(mat) == 0 || all(is.na(mat))) return(invisible(NULL))
  long <- matrix_to_long(mat, "value")
  p <- ggplot2::ggplot(long, ggplot2::aes(x = var1, y = var2, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", value)), size = 2.4, na.rm = TRUE) +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = 0, na.value = "grey90", name = legend_name) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), plot.title = ggplot2::element_text(face = "bold"))
  ggplot2::ggsave(path, p, width = 8.5, height = 7, dpi = 300)
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
    accuracy = (tp + tn) / length(y),
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

strict_allowed_raw <- function(var, allow_symptom_scores = FALSE) {
  if (allow_symptom_scores && var %in% c(
    "gad7_total", "phq9_total", "phq8_total", "phq9_item9", "psqi_total",
    "ess_total", "pss_total", "fatigue_total", "mbi_ee_total",
    "mbi_dp_total", "mbi_pa_total", "mbi_low_pa_total"
  )) return(TRUE)
  lower <- tolower(var)
  allow <- var %in% c("age", "BMI", "height", "weight", "work_years", "A_year", "submit_year") ||
    grepl("^A_q(2|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18)($|_)", var) ||
    grepl("^B_", var) ||
    grepl("^E_", var) ||
    grepl("^C_q(?!47|50)", var, perl = TRUE) ||
    grepl("^F_q1|fuxingxingwei", lower)
  exclude_pattern <- paste(c(
    "gad", "phq", "psqi", "ess", "pss", "mbi", "fatigue", "jiaolv", "yiyu",
    "yali", "shuimian", "rushuishijian", "shengchan", "lizhiyiyuan",
    "sleep", "anxiety", "depression", "stress", "burnout", "turnover",
    "productivity", "social", "support", "coping", "personality",
    "睡眠", "失眠", "抑郁", "焦虑", "压力", "倦怠", "疲劳", "疲乏",
    "心理", "功能障碍", "生产力", "离职", "社会支持", "应对", "人格", "工作家庭"
  ), collapse = "|")
  exclude <- grepl(exclude_pattern, var, ignore.case = TRUE) ||
    grepl("^D_", var) ||
    grepl("^G_|^H_|C_q50|C_q47|C_gzjt|C_jtqrgz|C_SWD", var, ignore.case = TRUE)
  allow && !exclude
}

learn_preprocess <- function(train_df, candidate_vars, max_missing_prop = 0.50, max_factor_levels = 30) {
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

find_raw_predictor <- function(feature, final_predictors) {
  hits <- final_predictors[vapply(final_predictors, function(p) feature == p || startsWith(feature, p), logical(1))]
  if (length(hits) == 0) return(NA_character_)
  hits[which.max(nchar(hits))]
}

fit_lasso_pipeline <- function(model_df, y, candidate_vars, seed = 20260610, allow_symptom_scores = FALSE) {
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
  coef_mat <- as.matrix(stats::coef(cv_fit, s = "lambda.1se"))
  coef_table <- data.frame(term = rownames(coef_mat), coefficient = as.numeric(coef_mat[, 1]), stringsAsFactors = FALSE)
  coef_table <- coef_table[coef_table$term != "(Intercept)" & coef_table$coefficient != 0, , drop = FALSE]
  coef_table$abs_coefficient <- abs(coef_table$coefficient)
  coef_table$raw_predictor <- vapply(coef_table$term, find_raw_predictor, character(1), final_predictors = names(preprocess$specs))
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

bootstrap_metric_ci <- function(y, p, threshold, n_boot = 1000, seed = 20260611) {
  point <- c(
    pr_auc_average_precision = average_precision(y, p),
    brier = mean((p - y)^2),
    sensitivity = classification_metrics(y, p, threshold)$sensitivity,
    specificity = classification_metrics(y, p, threshold)$specificity,
    ppv = classification_metrics(y, p, threshold)$ppv,
    npv = classification_metrics(y, p, threshold)$npv,
    f1 = classification_metrics(y, p, threshold)$f1,
    accuracy = classification_metrics(y, p, threshold)$accuracy
  )
  set.seed(seed)
  boot <- matrix(NA_real_, nrow = n_boot, ncol = length(point), dimnames = list(NULL, names(point)))
  for (b in seq_len(n_boot)) {
    idx <- sample.int(length(y), length(y), replace = TRUE)
    if (length(unique(y[idx])) < 2) next
    boot[b, ] <- c(
      pr_auc_average_precision = average_precision(y[idx], p[idx]),
      brier = mean((p[idx] - y[idx])^2),
      sensitivity = classification_metrics(y[idx], p[idx], threshold)$sensitivity,
      specificity = classification_metrics(y[idx], p[idx], threshold)$specificity,
      ppv = classification_metrics(y[idx], p[idx], threshold)$ppv,
      npv = classification_metrics(y[idx], p[idx], threshold)$npv,
      f1 = classification_metrics(y[idx], p[idx], threshold)$f1,
      accuracy = classification_metrics(y[idx], p[idx], threshold)$accuracy
    )
  }
  ci <- t(apply(boot, 2, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE))
  data.frame(metric = names(point), estimate = as.numeric(point), ci_lower = ci[, 1], ci_upper = ci[, 2], ci_method = paste0("bootstrap n=", n_boot), stringsAsFactors = FALSE)
}

log_msg("Bastion pipeline started. Output root:", output_root)

raw_data <- as.data.frame(get("data", envir = .GlobalEnv), stringsAsFactors = FALSE)
input_rows <- nrow(raw_data)
input_cols <- ncol(raw_data)
required_cols <- c("id", "submittime", "A_year", "A_q15", "A_q16", "work_y")
missing_required <- setdiff(required_cols, names(raw_data))
if (length(missing_required) > 0) {
  stop("Missing required columns: ", paste(missing_required, collapse = ", "), call. = FALSE)
}

duplicate_id_count <- sum(duplicated(raw_data$id[!is.na(raw_data$id)]))
raw_data <- raw_data[!duplicated(raw_data$id), , drop = FALSE]
rows_after_distinct <- nrow(raw_data)

gad7_items <- paste0("D_q22_", 1:7)
phq9_items <- paste0("D_q23_", 1:9)
phq8_items <- paste0("D_q23_", 1:8)
pss_items <- paste0("D_q24_", 1:10)
pss_reverse <- c("D_q24_4", "D_q24_5", "D_q24_7", "D_q24_8")
ess_items <- paste0("D_q19_", 1:8)
mbi_items <- paste0("F_q3_", 1:22)
mbi_ee_items <- paste0("F_q3_", c(1, 2, 3, 6, 8, 13, 14, 16, 20))
mbi_dp_items <- paste0("F_q3_", c(5, 10, 11, 15, 22))
mbi_pa_items <- paste0("F_q3_", c(4, 7, 9, 12, 17, 18, 19, 21))
psqi_requested <- paste0("D_q10_", 1:7)

core_vars <- existing(raw_data, unique(c(gad7_items, phq9_items, pss_items, ess_items, mbi_items, psqi_requested, "D_PSQI_ALL")))
core_missing_prop <- if (length(core_vars) > 0) rowMeans(is.na(raw_data[, core_vars, drop = FALSE])) else rep(NA_real_, nrow(raw_data))
raw_data$core_missing_prop <- core_missing_prop
core_keep <- is.na(core_missing_prop) | core_missing_prop <= 0.30
removed_core <- sum(!core_keep, na.rm = TRUE)

raw_submit_year <- extract_year(raw_data$submittime)
raw_birth_year <- as_num(raw_data$A_year)
raw_data$submit_year <- raw_submit_year
raw_data$age <- raw_submit_year - raw_birth_year
raw_data$age <- ifelse(raw_data$age < 16 | raw_data$age > 65, NA_real_, raw_data$age)
raw_h <- as_num(raw_data$A_q15)
raw_w <- as_num(raw_data$A_q16)
raw_data$height <- ifelse(raw_h < 140 | raw_h > 210, NA_real_, raw_h)
raw_data$weight <- ifelse(raw_w < 35 | raw_w > 120, NA_real_, raw_w)
raw_data$BMI <- raw_data$weight / (raw_data$height / 100)^2
raw_data$work_start_year <- as_num(raw_data$work_y)
raw_data$work_years <- raw_data$submit_year - raw_data$work_start_year
raw_data$work_start_age <- raw_data$work_start_year - raw_birth_year

rt_vars <- names(raw_data)[grepl("^timetaken", names(raw_data), ignore.case = TRUE)]
if (length(rt_vars) > 0) {
  rt_matrix <- as.data.frame(lapply(raw_data[, rt_vars, drop = FALSE], as_num))
  raw_data$total_response_time_seconds <- rowSums(rt_matrix, na.rm = TRUE)
  raw_data$total_response_time_seconds[rowSums(!is.na(rt_matrix)) == 0] <- NA_real_
} else {
  raw_data$total_response_time_seconds <- NA_real_
}

work_lt1 <- !is.na(raw_data$work_years) & raw_data$work_years < 1
work_keep1 <- core_keep & !work_lt1
work_age_bad <- !is.na(raw_data$work_start_age) & raw_data$work_start_age < 16
work_keep2 <- work_keep1 & !work_age_bad
short_rt <- !is.na(raw_data$total_response_time_seconds) & raw_data$total_response_time_seconds < 600
clean_keep <- work_keep2 & !short_rt
clean_data <- raw_data[clean_keep, , drop = FALSE]

clean_report <- data.frame(
  total_raw_rows_imported = input_rows,
  total_columns_imported = input_cols,
  duplicate_id_rows_removed = duplicate_id_count,
  rows_after_duplicate_id_removal = rows_after_distinct,
  removed_due_to_core_missing_gt_30pct = removed_core,
  removed_due_to_work_years_lt_1 = sum(core_keep & work_lt1, na.rm = TRUE),
  removed_due_to_work_years_age_inconsistent = sum(work_keep1 & work_age_bad, na.rm = TRUE),
  removed_due_to_response_time_lt_10min = sum(work_keep2 & short_rt, na.rm = TRUE),
  total_clean_rows = nrow(clean_data),
  stringsAsFactors = FALSE
)

variable_cleaning <- rbind(
  data.frame(variable = "submit_year_from_submittime", rule = "year parsed from submittime", clean_non_missing = sum(!is.na(clean_data$submit_year)), clean_min = min(clean_data$submit_year, na.rm = TRUE), clean_max = max(clean_data$submit_year, na.rm = TRUE)),
  data.frame(variable = "age_from_A_year_and_submittime", rule = "submit_year - A_year; set NA if <16 or >65", clean_non_missing = sum(!is.na(clean_data$age)), clean_min = min(clean_data$age, na.rm = TRUE), clean_max = max(clean_data$age, na.rm = TRUE)),
  data.frame(variable = "height_from_A_q15", rule = "set NA if <140 or >210 cm", clean_non_missing = sum(!is.na(clean_data$height)), clean_min = min(clean_data$height, na.rm = TRUE), clean_max = max(clean_data$height, na.rm = TRUE)),
  data.frame(variable = "weight_from_A_q16", rule = "set NA if <35 or >120 kg", clean_non_missing = sum(!is.na(clean_data$weight)), clean_min = min(clean_data$weight, na.rm = TRUE), clean_max = max(clean_data$weight, na.rm = TRUE)),
  data.frame(variable = "BMI", rule = "weight / (height/100)^2 after height/weight cleaning", clean_non_missing = sum(!is.na(clean_data$BMI)), clean_min = min(clean_data$BMI, na.rm = TRUE), clean_max = max(clean_data$BMI, na.rm = TRUE)),
  data.frame(variable = "work_years_from_submittime_minus_work_y", rule = "submit_year - work_y; exclude if <1 or work_y - A_year <16", clean_non_missing = sum(!is.na(clean_data$work_years)), clean_min = min(clean_data$work_years, na.rm = TRUE), clean_max = max(clean_data$work_years, na.rm = TRUE)),
  data.frame(variable = "total_response_time_seconds", rule = "row sum of timetaken* variables; exclude if <600 seconds", clean_non_missing = sum(!is.na(clean_data$total_response_time_seconds)), clean_min = min(clean_data$total_response_time_seconds, na.rm = TRUE), clean_max = max(clean_data$total_response_time_seconds, na.rm = TRUE))
)

write_workbook(
  list(
    sample_flow = clean_report,
    variable_cleaning_summary = variable_cleaning,
    row_exclusion_summary = data.frame(
      exclusion_rule = c("core_missing_prop > 0.30", "work_years < 1", "work_y - A_year < 16", "total_response_time_seconds < 600"),
      sequential_removed_n = c(clean_report$removed_due_to_core_missing_gt_30pct, clean_report$removed_due_to_work_years_lt_1, clean_report$removed_due_to_work_years_age_inconsistent, clean_report$removed_due_to_response_time_lt_10min),
      stringsAsFactors = FALSE
    ),
    core_items_used = data.frame(variable = core_vars, stringsAsFactors = FALSE)
  ),
  file.path(output_root, "results/tables/data_cleaning_report.xlsx")
)
saveRDS(clean_data, file.path(output_root, "data/processed/clean_base_data.rds"))
log_msg("Cleaning finished. Clean rows:", nrow(clean_data))

dat <- clean_data
if (!all(gad7_items %in% names(dat))) stop("Missing GAD-7 item columns.", call. = FALSE)
if (!all(phq9_items %in% names(dat))) stop("Missing PHQ-9 item columns.", call. = FALSE)
if (!all(pss_items %in% names(dat))) stop("Missing PSS item columns.", call. = FALSE)

dat$gad7_total <- row_sum_complete(dat, gad7_items, 0, 3)
dat$phq9_total <- row_sum_complete(dat, phq9_items, 0, 3)
dat$phq8_total <- row_sum_complete(dat, phq8_items, 0, 3)
dat$phq9_item9 <- as_num(dat$D_q23_9)
dat$pss_total <- row_sum_complete(dat, pss_items, 0, 4, pss_reverse)
if ("D_PSQI_ALL" %in% names(dat)) {
  dat$psqi_total <- as_num(dat$D_PSQI_ALL)
} else if (all(psqi_requested %in% names(dat))) {
  dat$psqi_total <- row_sum_complete(dat, psqi_requested)
} else {
  dat$psqi_total <- NA_real_
}
if (all(ess_items %in% names(dat))) dat$ess_total <- row_sum_complete(dat, ess_items, 0, 3)
if (all(mbi_ee_items %in% names(dat))) dat$mbi_ee_total <- row_sum_complete(dat, mbi_ee_items, 0, 6)
if (all(mbi_dp_items %in% names(dat))) dat$mbi_dp_total <- row_sum_complete(dat, mbi_dp_items, 0, 6)
if (all(mbi_pa_items %in% names(dat))) dat$mbi_pa_total <- row_sum_complete(dat, mbi_pa_items, 0, 6)
if (all(mbi_items %in% names(dat))) dat$mbi_total <- row_sum_complete(dat, mbi_items, 0, 6)
if ("mbi_pa_total" %in% names(dat)) dat$mbi_low_pa_total <- max(dat$mbi_pa_total, na.rm = TRUE) - dat$mbi_pa_total

score_vars <- existing(dat, c("gad7_total", "phq9_total", "phq8_total", "psqi_total", "ess_total", "pss_total", "mbi_ee_total", "mbi_dp_total", "mbi_pa_total", "mbi_low_pa_total"))
scale_summary <- summarise_numeric(dat, score_vars, "scale_score")
reliability <- do.call(rbind, list(
  reliability_row(dat, "GAD-7", gad7_items, 0, 3),
  reliability_row(dat, "PHQ-9", phq9_items, 0, 3),
  reliability_row(dat, "PHQ-8", phq8_items, 0, 3),
  reliability_row(dat, "PSS-10", pss_items, 0, 4, pss_reverse),
  reliability_row(dat, "ESS", ess_items, 0, 3),
  reliability_row(dat, "MBI emotional exhaustion", mbi_ee_items, 0, 6),
  reliability_row(dat, "MBI depersonalization", mbi_dp_items, 0, 6),
  reliability_row(dat, "MBI personal accomplishment", mbi_pa_items, 0, 6)
))

dat$gad7_ge10 <- make_indicator(dat$gad7_total, 10, ">=")
dat$phq9_ge10 <- make_indicator(dat$phq9_total, 10, ">=")
dat$gad7_ge10_and_phq9_ge10 <- ifelse(is.na(dat$gad7_ge10) | is.na(dat$phq9_ge10), NA_integer_, as.integer(dat$gad7_ge10 == 1 & dat$phq9_ge10 == 1))
dat$psqi_gt7 <- make_indicator(dat$psqi_total, 7, ">")
dat$psqi_gt5 <- make_indicator(dat$psqi_total, 5, ">")
dat$ess_gt10 <- make_indicator(dat$ess_total, 10, ">")
dat$pss_ge_p75 <- make_indicator(dat$pss_total, as.numeric(stats::quantile(dat$pss_total, 0.75, na.rm = TRUE)), ">=")
dat$mbi_ee_ge_p75 <- make_indicator(dat$mbi_ee_total, as.numeric(stats::quantile(dat$mbi_ee_total, 0.75, na.rm = TRUE)), ">=")
dat$mbi_dp_ge_p75 <- make_indicator(dat$mbi_dp_total, as.numeric(stats::quantile(dat$mbi_dp_total, 0.75, na.rm = TRUE)), ">=")
dat$mbi_pa_le_p25 <- make_indicator(dat$mbi_pa_total, as.numeric(stats::quantile(dat$mbi_pa_total, 0.25, na.rm = TRUE)), "<=")

indicator_vars <- existing(dat, c("gad7_ge10", "phq9_ge10", "gad7_ge10_and_phq9_ge10", "psqi_gt7", "psqi_gt5", "ess_gt10", "pss_ge_p75", "mbi_ee_ge_p75", "mbi_dp_ge_p75", "mbi_pa_le_p25"))
symptom_prevalence <- indicator_summary(dat, indicator_vars)
score_cor <- suppressWarnings(stats::cor(as.data.frame(lapply(dat[, score_vars, drop = FALSE], as_num)), use = "pairwise.complete.obs", method = "spearman"))
binary_df <- dat[, indicator_vars, drop = FALSE]
jaccard_mat <- binary_jaccard(binary_df)
phi_mat <- binary_phi(binary_df)
plot_heatmap(jaccard_mat, file.path(output_root, "results/figures/stage4_symptom_cooccurrence_jaccard_heatmap.png"), "Symptom Co-occurrence: Jaccard Index", "Jaccard")
plot_heatmap(phi_mat, file.path(output_root, "results/figures/stage4_symptom_cooccurrence_phi_heatmap.png"), "Symptom Co-occurrence: Phi Correlation", "Phi")
plot_heatmap(score_cor, file.path(output_root, "results/figures/stage4_spearman_correlation_heatmap.png"), "Spearman Correlation Among Scale Scores", "rho")

write_workbook(
  list(
    score_descriptive = scale_summary,
    reliability = reliability,
    symptom_prevalence = symptom_prevalence,
    spearman_correlation = matrix_to_long(score_cor, "rho"),
    cooccurrence_jaccard = matrix_to_long(jaccard_mat, "jaccard"),
    cooccurrence_phi = matrix_to_long(phi_mat, "phi")
  ),
  file.path(output_root, "results/tables/stage4_descriptive_missing_reliability.xlsx")
)
saveRDS(dat, file.path(output_root, "data/processed/analysis_stage4_descriptive_data.rds"))
log_msg("Scoring/descriptive stage finished.")

z_score <- function(x) {
  x <- as_num(x)
  (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
}
entropy_from_z <- function(z) {
  if (is.null(z) || ncol(z) < 2) return(NA_real_)
  z_safe <- pmax(z, .Machine$double.eps)
  n <- nrow(z_safe)
  k <- ncol(z_safe)
  as.numeric(1 + sum(z_safe * log(z_safe)) / (n * log(k)))
}

lpa_vars <- existing(dat, c("gad7_total", "phq9_total", "psqi_total", "ess_total", "pss_total", "fatigue_total", "mbi_ee_total", "mbi_dp_total", "mbi_low_pa_total"))
if (length(lpa_vars) < 3) stop("Fewer than 3 LPA indicators available.", call. = FALSE)
lpa_raw <- as.data.frame(lapply(dat[, lpa_vars, drop = FALSE], as_num))
complete_idx <- stats::complete.cases(lpa_raw)
lpa_complete <- lpa_raw[complete_idx, , drop = FALSE]
lpa_z <- as.data.frame(lapply(lpa_complete, z_score))
dat$LPA_included <- complete_idx
dat$LPA_class <- NA_integer_
dat$LPA_probability <- NA_real_

model_name <- "EEI"
lpa_models <- list()
fit_rows <- list()
for (k in 1:7) {
  log_msg("Fitting LPA model:", k, "classes")
  fit_rows[[paste0("k", k)]] <- tryCatch({
    mod <- mclust::Mclust(lpa_z, G = k, modelNames = model_name, verbose = FALSE)
    lpa_models[[paste0("k", k)]] <- mod
    class_n <- tabulate(mod$classification, nbins = k)
    log_lik <- as.numeric(mod$loglik)
    df <- as.numeric(mod$df)
    n <- nrow(lpa_z)
    max_p <- apply(mod$z, 1, max)
    data.frame(
      classes = k,
      model_name = model_name,
      converged = TRUE,
      admissible_nonempty = all(class_n > 0),
      logLik = log_lik,
      df = df,
      AIC = -2 * log_lik + 2 * df,
      BIC = -2 * log_lik + log(n) * df,
      sample_size_adjusted_BIC = -2 * log_lik + df * log((n + 2) / 24),
      mclust_BIC = as.numeric(mod$bic),
      entropy = entropy_from_z(mod$z),
      min_class_n = min(class_n),
      min_class_pct = min(class_n) / n,
      mean_max_posterior_probability = mean(max_p),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    log_msg("LPA model failed:", k, "classes;", conditionMessage(e))
    data.frame(classes = k, model_name = model_name, converged = FALSE, admissible_nonempty = FALSE, logLik = NA_real_, df = NA_real_, AIC = NA_real_, BIC = NA_real_, sample_size_adjusted_BIC = NA_real_, mclust_BIC = NA_real_, entropy = NA_real_, min_class_n = NA_integer_, min_class_pct = NA_real_, mean_max_posterior_probability = NA_real_, stringsAsFactors = FALSE)
  })
}
fit_stats <- do.call(rbind, fit_rows)
valid_fit <- fit_stats[fit_stats$converged & fit_stats$admissible_nonempty & !is.na(fit_stats$BIC), , drop = FALSE]
best_k <- valid_fit$classes[which.min(valid_fit$BIC)]
best_model <- lpa_models[[paste0("k", best_k)]]
best_class <- as.integer(best_model$classification)
best_prob <- if (!is.null(best_model$z) && length(dim(best_model$z)) == 2) {
  apply(best_model$z, 1, max)
} else {
  rep(1, length(best_class))
}
dat$LPA_class[complete_idx] <- best_class
dat$LPA_probability[complete_idx] <- best_prob

class_counts <- as.data.frame(table(dat$LPA_class, useNA = "ifany"), stringsAsFactors = FALSE)
names(class_counts) <- c("LPA_class", "n")
class_counts$pct_total <- class_counts$n / nrow(dat)
class_counts$pct_lpa_included <- ifelse(is.na(suppressWarnings(as.numeric(class_counts$LPA_class))), NA_real_, class_counts$n / sum(complete_idx))

class_means_raw <- aggregate(lpa_raw[complete_idx, , drop = FALSE], list(LPA_class = best_class), mean, na.rm = TRUE)
class_means_raw$n <- as.integer(table(best_class)[as.character(class_means_raw$LPA_class)])
class_means_raw <- class_means_raw[, c("LPA_class", "n", lpa_vars), drop = FALSE]
lpa_z_class <- lpa_z
lpa_z_class$LPA_class <- best_class
class_means_z <- aggregate(lpa_z_class[, lpa_vars, drop = FALSE], list(LPA_class = best_class), mean, na.rm = TRUE)
class_means_z$n <- as.integer(table(best_class)[as.character(class_means_z$LPA_class)])
class_means_z <- class_means_z[, c("LPA_class", "n", lpa_vars), drop = FALSE]
selected_model <- data.frame(selected_classes = best_k, selection_rule = "minimum conventional BIC among converged 1-7 class EEI models with no empty classes", model_name = model_name, lpa_rows = nrow(lpa_z), excluded_due_to_missing = sum(!complete_idx), variables = paste(lpa_vars, collapse = " | "), stringsAsFactors = FALSE)

write_workbook(
  list(
    fit_stats = fit_stats,
    selected_model = selected_model,
    class_counts = class_counts,
    class_means_raw = class_means_raw,
    class_means_z = class_means_z
  ),
  file.path(output_root, "results/tables/lpa_fit_results.xlsx")
)
saveRDS(list(data = dat, models = lpa_models, fit_stats = fit_stats, selected_model = selected_model, class_counts = class_counts, class_means_raw = class_means_raw, class_means_z = class_means_z, lpa_variables = lpa_vars), file.path(output_root, "data/processed/lpa_results.rds"))
log_msg("LPA finished. Selected classes:", best_k)

burden_vars <- intersect(lpa_vars, names(class_means_z))
class_burden <- class_means_z[, c("LPA_class", "n", burden_vars), drop = FALSE]
class_burden$psychological_burden_z_mean <- rowMeans(class_burden[, burden_vars, drop = FALSE], na.rm = TRUE)
class_burden <- class_burden[order(-class_burden$psychological_burden_z_mean), ]
high_risk_lpa_class <- class_burden$LPA_class[1]
dat$high_risk_class <- ifelse(is.na(dat$LPA_class), NA_integer_, as.integer(dat$LPA_class == high_risk_lpa_class))
model_df <- dat[!is.na(dat$high_risk_class), , drop = FALSE]
y <- as.integer(model_df$high_risk_class)
non_predictor <- c("id", "LPA_class", "LPA_probability", "LPA_included", "high_risk_class", "binary_prediction_split", "binary_prediction_probability", "binary_prediction_class_youden")
candidate_all <- setdiff(names(model_df), non_predictor)
candidate_vars <- candidate_all[vapply(candidate_all, strict_allowed_raw, logical(1))]
primary <- fit_lasso_pipeline(model_df, y, candidate_vars)
performance <- primary$performance
performance$model <- "strict_lasso_logistic_lambda_1se"
performance <- performance[, c("model", setdiff(names(performance), "model"))]

save_data <- dat
nonmissing_rows <- which(!is.na(save_data$high_risk_class))
save_data$binary_prediction_split <- NA_character_
save_data$binary_prediction_split[nonmissing_rows[primary$train_idx]] <- "train"
save_data$binary_prediction_split[nonmissing_rows[primary$test_idx]] <- "test"
save_data$binary_prediction_probability <- NA_real_
save_data$binary_prediction_probability[nonmissing_rows[primary$train_idx]] <- primary$train_pred
save_data$binary_prediction_probability[nonmissing_rows[primary$test_idx]] <- primary$test_pred
save_data$binary_prediction_class_youden <- ifelse(is.na(save_data$binary_prediction_probability), NA_integer_, as.integer(save_data$binary_prediction_probability >= primary$threshold))
saveRDS(save_data, file.path(output_root, "data/processed/prediction_binary_data.rds"))

model_object <- list(
  model = primary$fit,
  model_type = "strict_lasso_logistic_lambda_1se",
  best_threshold_youden = primary$threshold,
  preprocess = primary$preprocess,
  retained_model_columns = primary$retained_model_columns,
  high_risk_lpa_class = high_risk_lpa_class,
  class_burden = class_burden,
  lpa_variables = lpa_vars,
  performance = performance,
  coefficients = primary$coefficients
)
saveRDS(model_object, file.path(output_root, "results/models/stage6_binary_prediction_models.rds"))

outcome_table <- as.data.frame(table(save_data$high_risk_class, useNA = "ifany"), stringsAsFactors = FALSE)
names(outcome_table) <- c("high_risk_class", "n")
outcome_table$pct_total <- outcome_table$n / nrow(save_data)

write_workbook(
  list(
    selected_high_risk = data.frame(high_risk_lpa_class = high_risk_lpa_class, selection_rule = "maximum mean standardized LPA symptom burden"),
    class_burden = class_burden,
    outcome_table = outcome_table,
    performance = performance,
    top_lasso_coefficients = head(primary$coefficients, 100),
    preprocessing_exclusions = primary$preprocess$excluded
  ),
  file.path(output_root, "results/tables/stage6_binary_prediction_results.xlsx")
)

test_roc <- pROC::roc(primary$y_test, primary$test_pred, quiet = TRUE, levels = c(0, 1), direction = "<")
roc_df <- data.frame(fpr = 1 - rev(test_roc$specificities), tpr = rev(test_roc$sensitivities))
p_roc <- ggplot2::ggplot(roc_df, ggplot2::aes(fpr, tpr)) +
  ggplot2::geom_line(color = "#2563EB", linewidth = 1) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  ggplot2::coord_equal() +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::labs(title = "Strict Binary Prediction ROC Curve", x = "1 - specificity", y = "Sensitivity")
ggplot2::ggsave(file.path(output_root, "results/figures/binary_prediction/binary_prediction_roc_curve.png"), p_roc, width = 6, height = 5.5, dpi = 300)

if (nrow(primary$coefficients) > 0) {
  top_coef <- head(primary$coefficients, 25)
  top_coef$term <- factor(top_coef$term, levels = rev(top_coef$term))
  p_coef <- ggplot2::ggplot(top_coef, ggplot2::aes(term, abs_coefficient, fill = coefficient > 0)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::labs(title = "Top LASSO Coefficients", x = NULL, y = "Absolute coefficient")
  ggplot2::ggsave(file.path(output_root, "results/figures/binary_prediction/binary_prediction_lasso_top_predictors.png"), p_coef, width = 8, height = 7, dpi = 300)
}
log_msg("Primary strict model finished. Test AUC:", round(performance$auc[performance$split == "test"], 4))

boot_n <- as.integer(get0("BASTION_BOOTSTRAP_N", ifnotfound = 1000, envir = .GlobalEnv))
auc_ci <- as.numeric(pROC::ci.auc(test_roc, conf.level = 0.95))
metric_ci <- rbind(
  data.frame(metric = "auc", estimate = as.numeric(pROC::auc(test_roc)), ci_lower = auc_ci[1], ci_upper = auc_ci[3], ci_method = "DeLong 95% CI", stringsAsFactors = FALSE),
  bootstrap_metric_ci(primary$y_test, primary$test_pred, primary$threshold, n_boot = boot_n)
)
lp <- qlogis(pmin(pmax(primary$test_pred, 1e-6), 1 - 1e-6))
cal_slope_model <- stats::glm(primary$y_test ~ lp, family = stats::binomial())
cal_offset_model <- stats::glm(primary$y_test ~ offset(lp), family = stats::binomial())
calibration_metrics <- data.frame(
  metric = c("calibration_intercept_with_slope", "calibration_slope", "calibration_in_the_large_intercept_offset_slope_1", "observed_events", "expected_events", "observed_expected_ratio", "brier"),
  value = c(unname(stats::coef(cal_slope_model)[1]), unname(stats::coef(cal_slope_model)[2]), unname(stats::coef(cal_offset_model)[1]), sum(primary$y_test == 1), sum(primary$test_pred), sum(primary$y_test == 1) / sum(primary$test_pred), mean((primary$test_pred - primary$y_test)^2)),
  stringsAsFactors = FALSE
)
dca_thresholds <- seq(0.05, 0.50, by = 0.01)
prev <- mean(primary$y_test == 1)
dca <- do.call(rbind, lapply(dca_thresholds, function(t) {
  pred <- primary$test_pred >= t
  n <- length(primary$y_test)
  data.frame(
    threshold_probability = t,
    model_net_benefit = sum(pred & primary$y_test == 1) / n - sum(pred & primary$y_test == 0) / n * t / (1 - t),
    treat_all_net_benefit = prev - (1 - prev) * t / (1 - t),
    treat_none_net_benefit = 0,
    stringsAsFactors = FALSE
  )
}))
write_workbook(
  list(
    model_metric_ci = metric_ci,
    calibration_metrics = calibration_metrics,
    confusion_matrix = classification_metrics(primary$y_test, primary$test_pred, primary$threshold),
    decision_curve = dca
  ),
  file.path(output_root, "results/tables/stage9_prediction_reporting_extras.xlsx")
)

run_sensitivity <- isTRUE(get0("BASTION_RUN_SENSITIVITY", ifnotfound = FALSE, envir = .GlobalEnv))
if (run_sensitivity) {
  log_msg("Sensitivity models started. This can take a long time.")
  sensitivity_rows <- list()
  sensitivity_rows[["primary"]] <- cbind(analysis_label = "primary early-identification model", performance[performance$split == "test", ])
  top2_classes <- class_burden$LPA_class[seq_len(min(2, nrow(class_burden)))]
  alt_y <- ifelse(is.na(model_df$LPA_class), NA_integer_, as.integer(model_df$LPA_class %in% top2_classes))
  ok <- !is.na(alt_y)
  if (length(unique(alt_y[ok])) == 2) {
    fit_alt <- fit_lasso_pipeline(model_df[ok, , drop = FALSE], alt_y[ok], candidate_vars)
    sensitivity_rows[["top2"]] <- cbind(analysis_label = "alternative high-risk definition: top 2 LPA burden classes", fit_alt$performance[fit_alt$performance$split == "test", ])
  }
  if ("LPA_probability" %in% names(model_df)) {
    ok_hc <- !is.na(model_df$LPA_probability) & model_df$LPA_probability >= 0.70
    if (sum(ok_hc) > 100 && length(unique(y[ok_hc])) == 2) {
      fit_hc <- fit_lasso_pipeline(model_df[ok_hc, , drop = FALSE], y[ok_hc], candidate_vars)
      sensitivity_rows[["high_conf"]] <- cbind(analysis_label = "high-confidence LPA sample: posterior probability >= 0.70", fit_hc$performance[fit_hc$performance$split == "test", ])
    }
  }
  enhanced_vars <- unique(c(candidate_vars, existing(model_df, c("gad7_total", "phq9_total", "psqi_total", "ess_total", "pss_total", "mbi_ee_total", "mbi_dp_total", "mbi_low_pa_total"))))
  fit_enh <- fit_lasso_pipeline(model_df, y, enhanced_vars)
  sensitivity_rows[["enhanced"]] <- cbind(analysis_label = "enhanced screening model with symptom scores", fit_enh$performance[fit_enh$performance$split == "test", ])
  sensitivity_perf <- do.call(rbind, sensitivity_rows)
} else {
  sensitivity_perf <- data.frame(
    analysis_label = "sensitivity models not run",
    note = "Set BASTION_RUN_SENSITIVITY <- TRUE before source() to run additional sensitivity models.",
    stringsAsFactors = FALSE
  )
  log_msg("Sensitivity models skipped by default. Set BASTION_RUN_SENSITIVITY <- TRUE to run them.")
}
write_workbook(
  list(sensitivity_performance = sensitivity_perf),
  file.path(output_root, "results/tables/stage8_sensitivity_subgroup_analysis.xlsx")
)

summary_table <- data.frame(
  item = c("output_root", "clean_n", "lpa_complete_n", "selected_lpa_classes", "high_risk_lpa_class", "high_risk_n", "test_auc", "test_pr_auc", "test_brier"),
  value = c(
    output_root,
    nrow(clean_data),
    sum(complete_idx),
    best_k,
    high_risk_lpa_class,
    sum(save_data$high_risk_class == 1, na.rm = TRUE),
    performance$auc[performance$split == "test"],
    performance$pr_auc_average_precision[performance$split == "test"],
    performance$brier[performance$split == "test"]
  ),
  stringsAsFactors = FALSE
)
write_workbook(list(summary = summary_table), file.path(output_root, "results/tables/bastion_pipeline_summary.xlsx"))
log_msg("Bastion pipeline finished.")
print(summary_table)
