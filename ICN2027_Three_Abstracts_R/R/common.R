# Shared functions for the three ICN 2027 abstract analyses.
# Reproducibility: seed 42; R >= 4.3 recommended.

options(stringsAsFactors = FALSE, width = 180)
set.seed(42)

required_packages <- c("sandwich", "lmtest", "ggplot2")

project_root <- function() {
  candidate <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    has_project <- length(list.files(candidate, pattern = "[.]Rproj$", full.names = TRUE)) > 0L
    if (has_project && file.exists(file.path(candidate, "R", "common.R"))) return(candidate)
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break
    candidate <- parent
  }
  stop(
    "Project root not found. Open ICN2027_Three_Abstracts_R.Rproj in RStudio before running a script.",
    call. = FALSE
  )
}

check_packages <- function(packages = required_packages) {
  available <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  if (!all(available)) {
    stop(
      "Missing R package(s): ", paste(names(available)[!available], collapse = ", "),
      ". Run 00_install_packages.R once, then rerun.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_project_config <- function(root = project_root()) {
  config <- new.env(parent = emptyenv())
  config$DATA_FILE <- file.path("data", "scored_data.rds")
  config$OUTPUT_DIR <- "results"
  config$STRICT_REPRODUCTION <- TRUE
  config$EXPECTED_N_ABSTRACT_1 <- 60836L
  config$EXPECTED_N_ABSTRACT_2 <- 8540L
  config$EXPECTED_N_ABSTRACT_3 <- 60836L
  config$EXPECTED_FULL_N <- 60838L

  config_path <- file.path(root, "config.R")
  if (file.exists(config_path)) sys.source(config_path, envir = config)

  env_data <- Sys.getenv("ICN_DATA_FILE", unset = "")
  env_output <- Sys.getenv("ICN_OUTPUT_DIR", unset = "")
  if (nzchar(env_data)) config$DATA_FILE <- env_data
  if (nzchar(env_output)) config$OUTPUT_DIR <- env_output

  as_absolute <- function(path) {
    if (!grepl("^([A-Za-z]:[/\\\\]|/)", path)) path <- file.path(root, path)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  list(
    root = root,
    data_file = as_absolute(config$DATA_FILE),
    output_dir = as_absolute(config$OUTPUT_DIR),
    strict_reproduction = isTRUE(config$STRICT_REPRODUCTION),
    expected_n_1 = as.integer(config$EXPECTED_N_ABSTRACT_1),
    expected_n_2 = as.integer(config$EXPECTED_N_ABSTRACT_2),
    expected_n_3 = as.integer(config$EXPECTED_N_ABSTRACT_3),
    expected_full_n = as.integer(config$EXPECTED_FULL_N)
  )
}

read_analysis_data <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Data file not found: ", path,
      ". Copy config.example.R to config.R and set DATA_FILE to the real data file.",
      call. = FALSE
    )
  }
  extension <- tolower(tools::file_ext(path))
  data <- switch(
    extension,
    rds = readRDS(path),
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    stop("Supported input formats are .rds and .csv. Received: ", extension, call. = FALSE)
  )
  if (!is.data.frame(data)) stop("The input file must contain a data.frame.", call. = FALSE)
  data
}

prepare_output_dir <- function(config, analysis_name) {
  path <- file.path(config$output_dir, analysis_name)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

as_numeric_safe <- function(x) suppressWarnings(as.numeric(as.character(x)))

assert_columns <- function(data, columns) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop("Required variable(s) missing: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# Restricted cubic spline basis equivalent to Hmisc::rcspline.eval(..., inclx = TRUE,
# norm = 2), implemented locally so the analysis runs on R 4.1 without rms/Hmisc.
rcs_basis <- function(x, knots) {
  knots <- sort(unique(as.numeric(knots)))
  knot_count <- length(knots)
  if (knot_count < 3L) stop("At least three distinct spline knots are required.", call. = FALSE)
  first_knot <- knots[[1L]]
  last_knot <- knots[[knot_count]]
  penultimate_knot <- knots[[knot_count - 1L]]
  scale_factor <- (last_knot - first_knot)^(2 / 3)
  nonlinear <- matrix(NA_real_, nrow = length(x), ncol = knot_count - 2L)
  for (index in seq_len(knot_count - 2L)) {
    nonlinear[, index] <-
      pmax((x - knots[[index]]) / scale_factor, 0)^3 +
      ((penultimate_knot - knots[[index]]) *
         pmax((x - last_knot) / scale_factor, 0)^3 -
       (last_knot - knots[[index]]) *
         pmax((x - penultimate_knot) / scale_factor, 0)^3) /
      (last_knot - penultimate_knot)
  }
  cbind(linear = x, nonlinear)
}

assert_expected_n <- function(observed, expected, label, strict = TRUE) {
  if (strict && !identical(as.integer(observed), as.integer(expected))) {
    stop(label, " did not reproduce: observed ", observed, ", expected ", expected, ".", call. = FALSE)
  }
  invisible(TRUE)
}

assert_near <- function(observed, expected, tolerance, label, strict = TRUE) {
  if (strict && (length(observed) != length(expected) || any(!is.finite(observed)) ||
                 any(abs(observed - expected) > tolerance))) {
    stop(label, " did not reproduce within tolerance ", tolerance, ".", call. = FALSE)
  }
  invisible(TRUE)
}

write_csv_utf8 <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path)
}

write_text_utf8 <- function(lines, path) {
  writeLines(enc2utf8(lines), con = path, useBytes = TRUE)
  invisible(path)
}

score_recalculation_audit <- function(stored, recalculated, score_name, tolerance = 1e-8) {
  comparable <- is.finite(stored) & is.finite(recalculated)
  differences <- abs(stored[comparable] - recalculated[comparable])
  data.frame(
    score = score_name,
    comparable_n = sum(comparable),
    mismatch_n = sum(differences > tolerance),
    maximum_absolute_difference = if (length(differences)) max(differences) else NA_real_,
    stringsAsFactors = FALSE
  )
}

robust_coefficient_table <- function(model, vcov_matrix) {
  test <- lmtest::coeftest(model, vcov. = vcov_matrix)
  critical_value <- stats::qnorm(0.975)
  data.frame(
    term = rownames(test),
    estimate = unname(test[, 1L]),
    robust_se = unname(test[, 2L]),
    ci_lower = unname(test[, 1L] - critical_value * test[, 2L]),
    ci_upper = unname(test[, 1L] + critical_value * test[, 2L]),
    statistic = unname(test[, 3L]),
    p_value = unname(test[, 4L]),
    row.names = NULL,
    check.names = FALSE
  )
}

robust_linear_hypothesis <- function(model, coefficient_names, vcov_matrix, label) {
  if (!length(coefficient_names)) stop("No coefficients supplied for: ", label, call. = FALSE)
  coefficient_indices <- match(coefficient_names, names(stats::coef(model)))
  if (anyNA(coefficient_indices)) {
    stop("Unknown coefficient(s) in hypothesis: ",
         paste(coefficient_names[is.na(coefficient_indices)], collapse = ", "), call. = FALSE)
  }
  estimates <- stats::coef(model)[coefficient_indices]
  covariance <- vcov_matrix[coefficient_indices, coefficient_indices, drop = FALSE]
  restriction_df <- length(coefficient_indices)
  if (qr(covariance)$rank < restriction_df) {
    stop("Hypothesis covariance matrix is rank deficient: ", label, call. = FALSE)
  }
  wald_chisq <- as.numeric(crossprod(estimates, solve(covariance, estimates)))
  f_statistic <- wald_chisq / restriction_df
  denominator_df <- stats::df.residual(model)
  data.frame(
    test = label,
    numerator_df = restriction_df,
    denominator_df = denominator_df,
    statistic = f_statistic,
    p_value = stats::pf(f_statistic, restriction_df, denominator_df, lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
}

contrast_differences <- function(model, vcov_matrix, newdata, exposure, reference_value,
                                 reference_data = NULL) {
  terms_without_response <- stats::delete.response(stats::terms(model))
  design <- stats::model.matrix(terms_without_response, newdata, contrasts.arg = model$contrasts)
  if (is.null(reference_data)) {
    reference_data <- newdata
    reference_data[[exposure]] <- reference_value
  }
  reference_design <- stats::model.matrix(
    terms_without_response, reference_data, contrasts.arg = model$contrasts
  )
  contrast_matrix <- design - reference_design
  estimate <- as.vector(contrast_matrix %*% stats::coef(model))
  standard_error <- sqrt(rowSums((contrast_matrix %*% vcov_matrix) * contrast_matrix))
  critical_value <- stats::qnorm(0.975)
  data.frame(
    exposure_value = newdata[[exposure]],
    reference_value = reference_value,
    estimate = estimate,
    robust_se = standard_error,
    ci_lower = estimate - critical_value * standard_error,
    ci_upper = estimate + critical_value * standard_error,
    stringsAsFactors = FALSE
  )
}

robust_predictions <- function(model, vcov_matrix, newdata, exposure) {
  terms_without_response <- stats::delete.response(stats::terms(model))
  design <- stats::model.matrix(terms_without_response, newdata, contrasts.arg = model$contrasts)
  estimate <- as.vector(design %*% stats::coef(model))
  standard_error <- sqrt(rowSums((design %*% vcov_matrix) * design))
  critical <- stats::qnorm(0.975)
  data.frame(
    exposure_value = newdata[[exposure]],
    predicted_score = estimate,
    robust_se = standard_error,
    ci_lower = estimate - critical * standard_error,
    ci_upper = estimate + critical * standard_error,
    stringsAsFactors = FALSE
  )
}

cronbach_alpha <- function(item_data) {
  item_data <- item_data[stats::complete.cases(item_data), , drop = FALSE]
  k <- ncol(item_data)
  if (nrow(item_data) < 2L || k < 2L) return(NA_real_)
  item_variances <- vapply(item_data, stats::var, numeric(1L))
  total_variance <- stats::var(rowSums(item_data, na.rm = FALSE))
  if (!is.finite(total_variance) || total_variance <= 0) return(NA_real_)
  k / (k - 1) * (1 - sum(item_variances) / total_variance)
}

reliability_rows <- function(item_data, scope) {
  x <- item_data[stats::complete.cases(item_data), , drop = FALSE]
  overall_alpha <- cronbach_alpha(x)
  do.call(rbind, lapply(seq_len(ncol(x)), function(index) {
    other_total <- rowSums(x[, -index, drop = FALSE], na.rm = FALSE)
    data.frame(
      scope = scope,
      n_complete = nrow(x),
      cronbach_alpha = overall_alpha,
      item = names(x)[index],
      corrected_item_total_correlation = stats::cor(x[[index]], other_total),
      alpha_if_deleted = cronbach_alpha(x[, -index, drop = FALSE]),
      stringsAsFactors = FALSE
    )
  }))
}

score_descriptive_row <- function(score, scope) {
  valid <- score[is.finite(score)]
  if (!length(valid)) stop("No valid scores for: ", scope, call. = FALSE)
  quartiles <- stats::quantile(valid, c(0.25, 0.75), names = FALSE)
  data.frame(
    scope = scope,
    total_n = length(score), valid_n = length(valid), missing_n = sum(!is.finite(score)),
    mean = mean(valid), sd = stats::sd(valid), median = stats::median(valid),
    q1 = quartiles[[1L]], q3 = quartiles[[2L]], min = min(valid), max = max(valid),
    stringsAsFactors = FALSE
  )
}

vif_table <- function(model) {
  design <- stats::model.matrix(model)
  design <- design[, colnames(design) != "(Intercept)", drop = FALSE]
  if (!ncol(design)) return(data.frame(term = character(), VIF = numeric()))
  values <- vapply(seq_len(ncol(design)), function(index) {
    outcome <- design[, index]
    if (!is.finite(stats::var(outcome)) || stats::var(outcome) == 0) return(NA_real_)
    predictors <- design[, -index, drop = FALSE]
    fit <- stats::lm.fit(cbind(`(Intercept)` = 1, predictors), outcome)
    residual_sum <- sum(fit$residuals^2)
    total_sum <- sum((outcome - mean(outcome))^2)
    r_squared <- 1 - residual_sum / total_sum
    if (!is.finite(r_squared) || r_squared >= 1) return(Inf)
    1 / (1 - r_squared)
  }, numeric(1L))
  data.frame(
    term = colnames(design),
    VIF = values,
    note = "Coefficient-level VIF; multi-level factors are shown by contrast column.",
    stringsAsFactors = FALSE
  )
}

model_diagnostic_statistics <- function(model) {
  residuals_standardized <- as.numeric(scale(stats::residuals(model)))
  ks_result <- suppressWarnings(stats::ks.test(residuals_standardized, "pnorm"))
  bp_result <- lmtest::bptest(model)
  cooks <- stats::cooks.distance(model)
  data.frame(
    n = stats::nobs(model),
    r_squared = summary(model)$r.squared,
    adjusted_r_squared = summary(model)$adj.r.squared,
    residual_ks_statistic = unname(ks_result$statistic),
    residual_ks_p_value = unname(ks_result$p.value),
    breusch_pagan_statistic = unname(bp_result$statistic),
    breusch_pagan_df = unname(bp_result$parameter),
    breusch_pagan_p_value = unname(bp_result$p.value),
    maximum_cooks_distance = max(cooks, na.rm = TRUE),
    observations_above_4_over_n = sum(cooks > 4 / stats::nobs(model), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

draw_lm_diagnostics <- function(model) {
  old_parameters <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_parameters), add = TRUE)
  graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 1, 0))
  graphics::plot(model, which = c(1, 2, 3, 5), id.n = 0, pch = 16, cex = 0.35)
}

save_lm_diagnostics <- function(model, output_dir, file_stem) {
  grDevices::png(file.path(output_dir, paste0(file_stem, ".png")),
                 width = 2400, height = 1800, res = 300, bg = "white")
  draw_lm_diagnostics(model)
  grDevices::dev.off()
  grDevices::pdf(file.path(output_dir, paste0(file_stem, ".pdf")),
                 width = 8, height = 6, useDingbats = FALSE)
  draw_lm_diagnostics(model)
  grDevices::dev.off()
  invisible(TRUE)
}

save_ggplot_pair <- function(plot_object, output_dir, file_stem, width = 7, height = 5) {
  ggplot2::ggsave(file.path(output_dir, paste0(file_stem, ".png")), plot_object,
                  width = width, height = height, units = "in", dpi = 300, bg = "white")
  ggplot2::ggsave(file.path(output_dir, paste0(file_stem, ".pdf")), plot_object,
                  width = width, height = height, units = "in", device = "pdf", bg = "white")
  invisible(TRUE)
}

most_common_level <- function(x) {
  counts <- sort(table(x, useNA = "no"), decreasing = TRUE)
  if (!length(counts)) stop("Factor has no observed levels.", call. = FALSE)
  names(counts)[[1L]]
}

format_p_value <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("<0.001")
  sprintf("%.3f", p_value)
}

write_analysis_manifest <- function(config, output_dir, analysis_script, analysis_n) {
  input_info <- file.info(config$data_file)
  lines <- c(
    paste0("Run completed: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Analysis script: ", analysis_script),
    paste0("Analysis N: ", analysis_n),
    paste0("Strict reproduction checks: ", config$strict_reproduction),
    paste0("Input filename: ", basename(config$data_file)),
    paste0("Input bytes: ", input_info$size),
    paste0("Input MD5: ", unname(tools::md5sum(config$data_file))),
    paste0("R version: ", R.version.string),
    paste0("Package versions: ", paste(
      paste0(required_packages, "=", vapply(required_packages, function(x) {
        as.character(utils::packageVersion(x))
      }, character(1L))),
      collapse = "; "
    ))
  )
  write_text_utf8(lines, file.path(output_dir, "analysis_manifest.txt"))
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  invisible(TRUE)
}
