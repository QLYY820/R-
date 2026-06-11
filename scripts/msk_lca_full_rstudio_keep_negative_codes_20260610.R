# Complete RStudio script for the nurse multisite musculoskeletal symptoms LCA
# study. This version keeps -1/-2/-3/-4 as valid coded values by default and
# does not automatically convert them to NA. It is designed for the
# bastion/RStudio environment where the full nurse dataset has already been
# loaded as a data.frame named `data`.
#
# Usage in RStudio:
#   1. Load the full dataset as `data`.
#   2. Optional: set output_dir, SURVEY_YEAR, or LCA_N_STARTS_* before sourcing.
#   3. source("msk_lca_full_rstudio_bastion_20260602.R")
#
# If `submittime` is present, the script derives each nurse's survey year from
# that timestamp. SURVEY_YEAR is only a fallback when no survey-time column is
# available.
#
# If `data` is not available, set INPUT_DATA_PATH to a CSV or RDS file before
# sourcing this script. Excel reading is intentionally not included so the script
# can run with base R only.

if (!exists("SURVEY_YEAR")) SURVEY_YEAR <- 2025
if (!exists("LCA_MAX_CLASSES")) LCA_MAX_CLASSES <- 6
if (!exists("LCA_N_STARTS_SMALL")) LCA_N_STARTS_SMALL <- 25
if (!exists("LCA_N_STARTS_LARGE")) LCA_N_STARTS_LARGE <- 18
if (!exists("LCA_MAX_ITER")) LCA_MAX_ITER <- 600
if (!exists("LCA_TOL")) LCA_TOL <- 1e-7
if (!exists("RANDOM_SEED")) RANDOM_SEED <- 20260601
if (!exists("MAKE_FIGURES")) MAKE_FIGURES <- TRUE
if (!exists("OUTPUT_ROOT")) OUTPUT_ROOT <- file.path(getwd(), "msk_lca_rstudio_outputs")
if (!exists("NEGATIVE_CODES_AS_MISSING")) NEGATIVE_CODES_AS_MISSING <- FALSE
if (!exists("CHECKPOINT_LCA_MODELS")) CHECKPOINT_LCA_MODELS <- TRUE
if (!exists("RESUME_LCA_CHECKPOINTS")) RESUME_LCA_CHECKPOINTS <- TRUE

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
if (!exists("output_dir")) {
  output_dir <- file.path(OUTPUT_ROOT, paste0("msk_lca_", timestamp))
}
figure_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

BASE_MISSING_TOKENS <- c("", "NA", "N/A", "na", "null", "NULL")
NEGATIVE_MISSING_TOKENS <- c("-1", "-2", "-3", "-4")
MISSING_TOKENS <- if (NEGATIVE_CODES_AS_MISSING) {
  c(BASE_MISSING_TOKENS, NEGATIVE_MISSING_TOKENS)
} else {
  BASE_MISSING_TOKENS
}

BODY_PARTS <- c(
  E_q24_1_1 = "Neck",
  E_q24_2_1 = "Shoulder",
  E_q24_3_1 = "Upper_back",
  E_q24_4_1 = "Elbow",
  E_q24_5_1 = "Wrist_hand",
  E_q24_6_1 = "Low_back",
  E_q24_7_1 = "Hip_thigh",
  E_q24_8_1 = "Knee",
  E_q24_9_1 = "Ankle_foot"
)
LCA_COLS <- unname(BODY_PARTS)

HAZARD_ITEMS <- c(
  "1" = "sharp_injury_score",
  "6" = "acute_lowback_score",
  "11" = "ionizing_radiation_score",
  "12" = "nonionizing_radiation_score",
  "13" = "noise_score",
  "15" = "chemical_disinfectant_score",
  "19" = "msk_ergonomic_score",
  "20" = "repetitive_strain_score",
  "21" = "prolonged_standing_score",
  "26" = "blood_body_fluid_score"
)

RAW_VARS_BASE <- c(
  "id", "A_year", "work_y", "A_q2", "A_q5", "A_q6", "A_q9", "A_q11",
  "A_q13", "A_q15", "A_q16", "A_q65", "A_q67", "A_zonggongzuo",
  "C_q1", "C_q2", "C_q42", "C_SWD",
  "D_PSQI_ALL", "D_PSQI_yichang", "D_shishui_ESS",
  "D_shengchanlishousun_all", "D_yali_all", "D_yiyu_all", "D_jiaolv_all",
  "G_pifa_all", "F_zhiyejuandai_all", "F_shehuizhichi_all",
  "F_fuxingxingwei_all",
  "E_q3", "E_q20",
  paste0("E_q10_", 1:10), paste0("E_q11_", 1:10),
  "E_q12", "E_q13", "E_q14", "E_q15", "E_q16", "E_q17", "E_q18", "E_q19",
  unlist(lapply(names(HAZARD_ITEMS), function(i) paste0("E_q23_", i, "_", 1:2))),
  names(BODY_PARTS)
)

SCALE_ITEM_COLS <- c(
  paste0("D_q19_", 1:8),
  paste0("D_q21_", 1:6),
  paste0("D_q22_", 1:7),
  paste0("D_q23_", 1:9),
  paste0("D_q24_", 1:10),
  paste0("G_q3_", 1:14),
  paste0("F_q3_", 1:22),
  paste0("F_q4_", 1:12),
  paste0("F_q1_", 1:22)
)

read_input_data <- function() {
  if (exists("data", envir = .GlobalEnv) && is.data.frame(get("data", envir = .GlobalEnv))) {
    message("Using data.frame `data` from the RStudio/global environment.")
    return(get("data", envir = .GlobalEnv))
  }
  if (exists("INPUT_DATA_PATH", envir = .GlobalEnv)) {
    path <- get("INPUT_DATA_PATH", envir = .GlobalEnv)
    if (grepl("\\.rds$", path, ignore.case = TRUE)) {
      return(readRDS(path))
    }
    if (grepl("\\.csv$", path, ignore.case = TRUE)) {
      return(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"))
    }
  }
  stop("No input data found. Load the dataset as a data.frame named `data`, or set INPUT_DATA_PATH to a CSV/RDS file.")
}

write_utf8_csv <- function(x, filename) {
  path <- file.path(output_dir, filename)
  if (is.null(x)) {
    x <- data.frame(note = "No rows were produced for this table.", stringsAsFactors = FALSE)
  }
  write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  message("Saved: ", normalizePath(path, winslash = "/", mustWork = FALSE))
  path
}

write_text <- function(lines, filename) {
  path <- file.path(output_dir, filename)
  writeLines(lines, path, useBytes = TRUE)
  path
}

log_msg <- function(...) {
  message(...)
  try(flush.console(), silent = TRUE)
}

empty_table <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)[0, , drop = FALSE]
}

safe_rbind <- function(rows, template) {
  if (length(rows) == 0) return(template)
  do.call(rbind, rows)
}

safe_step <- function(label, expr, fallback = NULL) {
  message("Starting: ", label)
  out <- tryCatch(
    expr,
    error = function(e) {
      message("WARNING: ", label, " failed: ", conditionMessage(e))
      fallback
    }
  )
  message("Finished: ", label)
  out
}

clean_missing <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    if (NEGATIVE_CODES_AS_MISSING) x[x %in% c(-1, -2, -3, -4)] <- NA_real_
    return(x)
  }
  if (is.character(x)) {
    x <- trimws(x)
    x[x %in% MISSING_TOKENS] <- NA_character_
  }
  x
}

to_num <- function(x) {
  x <- clean_missing(x)
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

parse_year_vector <- function(x) {
  out <- rep(NA_real_, length(x))

  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.numeric(format(x, "%Y")))
  }

  if (is.factor(x)) x <- as.character(x)

  if (is.numeric(x)) {
    # Excel date serials are usually around 44,000 for years 2021-2025.
    is_excel_date <- !is.na(x) & x >= 30000 & x <= 60000
    if (any(is_excel_date)) {
      dt <- as.POSIXct(x[is_excel_date] * 86400,
                       origin = "1899-12-30",
                       tz = "Asia/Shanghai")
      out[is_excel_date] <- as.numeric(format(dt, "%Y"))
    }
    is_year <- !is.na(x) & x >= 1900 & x <= 2035 & abs(x - round(x)) < 1e-6
    out[is_year] <- round(x[is_year])
    return(out)
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% MISSING_TOKENS] <- NA_character_
  formats <- c("%Y/%m/%d %H:%M:%S", "%Y-%m-%d %H:%M:%S",
               "%Y/%m/%d", "%Y-%m-%d")
  for (fmt in formats) {
    need <- is.na(out) & !is.na(x_chr)
    if (!any(need)) break
    parsed <- as.POSIXct(x_chr[need], format = fmt, tz = "Asia/Shanghai")
    ok <- !is.na(parsed)
    if (any(ok)) {
      idx <- which(need)[ok]
      out[idx] <- as.numeric(format(parsed[ok], "%Y"))
    }
  }

  need <- is.na(out) & !is.na(x_chr)
  if (any(need)) {
    m <- regexpr("(20[0-3][0-9]|19[0-9]{2})", x_chr[need], perl = TRUE)
    ok <- m > 0
    if (any(ok)) {
      vals <- regmatches(x_chr[need], m)
      idx <- which(need)[ok]
      out[idx] <- as.numeric(vals[ok])
    }
  }
  out
}

derive_survey_year <- function(df) {
  time_candidates <- c("submittime", "submittime.x", "submittime.y", "A_year_sub")
  for (col in time_candidates) {
    if (col %in% names(df)) {
      y <- parse_year_vector(df[[col]])
      if (sum(!is.na(y)) > 0) {
        log_msg(sprintf("Survey year derived from `%s`.", col))
        log_msg("Survey year distribution:")
        print(table(y, useNA = "ifany"))
        return(y)
      }
    }
  }
  if (length(SURVEY_YEAR) == 1) {
    log_msg(sprintf("No survey-time column found; using fixed SURVEY_YEAR = %s.", SURVEY_YEAR))
    return(rep(SURVEY_YEAR, nrow(df)))
  }
  if (length(SURVEY_YEAR) == nrow(df)) {
    log_msg("Using vector SURVEY_YEAR supplied in the R session.")
    return(as.numeric(SURVEY_YEAR))
  }
  stop("Cannot derive survey year. Add `submittime` to data or set SURVEY_YEAR to a single year/vector.")
}

is_binary01 <- function(x) {
  vals <- sort(unique(x[is.finite(x)]))
  length(vals) > 0 && all(vals %in% c(0, 1))
}

nmq_to_binary <- function(x) {
  x0 <- clean_missing(x)
  if (is.factor(x0)) x0 <- as.character(x0)
  out <- rep(NA_real_, length(x0))

  if (is.numeric(x0)) {
    vals <- sort(unique(x0[is.finite(x0)]))
    if (length(vals) > 0 && all(vals %in% c(0, 1))) {
      out <- as.numeric(x0)
    } else {
      out[x0 == 1] <- 0
      out[x0 == 2] <- 1
    }
    return(out)
  }

  num <- suppressWarnings(as.numeric(x0))
  vals <- sort(unique(num[is.finite(num)]))
  if (length(vals) > 0 && all(vals %in% c(0, 1))) {
    out[is.finite(num)] <- num[is.finite(num)]
  } else {
    out[num == 1] <- 0
    out[num == 2] <- 1
  }

  yes_tokens <- c("yes", "Yes", "YES", "Y", "y", "true", "TRUE", "shi", "\u662f")
  no_tokens <- c("no", "No", "NO", "N", "n", "false", "FALSE", "fou", "\u5426")
  out[x0 %in% yes_tokens] <- 1
  out[x0 %in% no_tokens] <- 0
  out
}

require_columns <- function(df, cols, context = "analysis") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Missing %s columns: %s", context, paste(missing, collapse = ", ")))
  }
}

row_mean <- function(df, cols) {
  available <- intersect(cols, names(df))
  if (length(available) == 0) return(rep(NA_real_, nrow(df)))
  mat <- as.matrix(df[, available, drop = FALSE])
  mat <- apply(mat, 2, to_num)
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1)
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

prepare_analysis_data <- function(df_raw) {
  require_columns(df_raw, RAW_VARS_BASE, "required raw")
  df <- df_raw
  original_n <- nrow(df)

  for (nm in names(df)) {
    df[[nm]] <- clean_missing(df[[nm]])
  }

  numeric_cols <- setdiff(intersect(RAW_VARS_BASE, names(df)), c("id", names(BODY_PARTS)))
  for (col in numeric_cols) {
    df[[col]] <- to_num(df[[col]])
  }

  df$survey_year <- derive_survey_year(df)
  df$birth_year <- df$A_year
  df$work_start_year <- df$work_y
  df$age <- df$survey_year - df$birth_year
  df$work_years <- df$survey_year - df$work_start_year
  df$bmi <- df$A_q16 / (df$A_q15 / 100)^2

  for (raw_col in names(BODY_PARTS)) {
    df[[BODY_PARTS[[raw_col]]]] <- nmq_to_binary(df[[raw_col]])
  }

  df$sex <- df$A_q2
  df$education <- df$A_q5
  df$marital <- df$A_q6
  df$department <- df$A_q9
  df$title <- df$A_q11
  df$income <- df$A_q13
  df$work_physical_activity <- df$A_zonggongzuo

  df$night_shift <- ifelse(df$C_q1 %in% c(2, 3, 4) | df$C_q2 %in% c(1, 2, 3), 1, 0)
  df$night_shift[is.na(df$C_q1) & is.na(df$C_q2)] <- NA_real_
  df$over40h_any <- ifelse(is.na(df$C_q42), NA_real_, ifelse(df$C_q42 > 1, 1, 0))
  df$heavy_work_any <- ifelse(is.na(df$A_q65), NA_real_, ifelse(df$A_q65 == 2, 1, 0))
  df$moderate_work_any <- ifelse(is.na(df$A_q67), NA_real_, ifelse(df$A_q67 == 2, 1, 0))
  df$occupational_exposure_any <- ifelse(is.na(df$E_q3), NA_real_, ifelse(df$E_q3 == 1, 1, 0))
  df$blood_body_fluid_any <- ifelse(is.na(df$E_q20), NA_real_, ifelse(df$E_q20 == 1, 1, 0))

  df$disinfectant_weekly_mean <- row_mean(df, paste0("E_q10_", 1:10))
  df$disinfectant_daily_mean <- row_mean(df, paste0("E_q11_", 1:10))

  for (item in names(HAZARD_ITEMS)) {
    prob <- to_num(df[[paste0("E_q23_", item, "_1")]])
    severity <- to_num(df[[paste0("E_q23_", item, "_2")]])
    df[[HAZARD_ITEMS[[item]]]] <- prob * severity
  }

  df$anesthetic_gas_weekly <- df$E_q12
  df$anesthetic_gas_daily <- df$E_q13
  df$antineoplastic_weekly <- df$E_q14
  df$antineoplastic_daily <- df$E_q15
  df$antiviral_weekly <- df$E_q16
  df$antiviral_daily <- df$E_q17
  df$ionizing_radiation_weekly <- df$E_q18
  df$ionizing_radiation_daily <- df$E_q19

  df$psqi_total <- df$D_PSQI_ALL
  df$psqi_poor_sleep <- df$D_PSQI_yichang
  df$ess_total <- df$D_shishui_ESS
  df$swsd_score <- df$C_SWD
  df$sps6_total <- df$D_shengchanlishousun_all
  df$pss_total <- df$D_yali_all
  df$phq9_total <- df$D_yiyu_all
  df$phq9_depression_risk <- ifelse(is.na(df$D_yiyu_all), NA_real_, ifelse(df$D_yiyu_all >= 10, 1, 0))
  df$gad7_total <- df$D_jiaolv_all
  df$fatigue_total <- df$G_pifa_all
  df$mbi_total <- df$F_zhiyejuandai_all
  df$social_support <- df$F_shehuizhichi_all
  df$negative_behavior <- df$F_fuxingxingwei_all

  analysis_cols <- c(
    "id", "survey_year", "age", "work_years", "bmi", "sex", "education", "marital",
    "department", "title", "income", "night_shift", "over40h_any",
    "heavy_work_any", "moderate_work_any", "work_physical_activity",
    "occupational_exposure_any", "blood_body_fluid_any",
    "disinfectant_weekly_mean", "disinfectant_daily_mean",
    "anesthetic_gas_weekly", "anesthetic_gas_daily",
    "antineoplastic_weekly", "antineoplastic_daily", "antiviral_weekly",
    "antiviral_daily", "ionizing_radiation_weekly", "ionizing_radiation_daily",
    "psqi_total", "psqi_poor_sleep", "ess_total", "swsd_score", "sps6_total",
    "pss_total", "phq9_total", "phq9_depression_risk", "gad7_total",
    "fatigue_total", "mbi_total", "social_support", "negative_behavior",
    "sharp_injury_score", "acute_lowback_score", "ionizing_radiation_score",
    "nonionizing_radiation_score", "noise_score", "chemical_disinfectant_score",
    "msk_ergonomic_score", "repetitive_strain_score",
    "prolonged_standing_score", "blood_body_fluid_score", LCA_COLS
  )

  subset <- df[, analysis_cols, drop = FALSE]

  # Core complete-case criteria follow the manuscript analysis variables.
  # Daily exposure-frequency variables are retained for supplemental models but
  # are not used as exclusion criteria because "not applicable" is common among
  # nurses without that exposure.
  core_complete_cols <- c(
    "survey_year", "age", "work_years", "bmi", "sex", "department", "title",
    "night_shift", "over40h_any", "heavy_work_any", "moderate_work_any",
    "work_physical_activity", "occupational_exposure_any", "blood_body_fluid_any",
    "disinfectant_weekly_mean", "anesthetic_gas_weekly", "antineoplastic_weekly",
    "antiviral_weekly", "ionizing_radiation_weekly", "psqi_total",
    "ess_total", "swsd_score", "sps6_total", "pss_total", "phq9_total",
    "fatigue_total", LCA_COLS
  )

  missing_key <- !complete.cases(subset[, core_complete_cols, drop = FALSE])
  work_lt_1y <- subset$work_years < 1
  work_lt_1y[is.na(work_lt_1y)] <- FALSE
  age_work_conflict <- (
    subset$age < 18 | subset$age > 70 |
      subset$work_years < 0 | subset$work_years > 55 |
      (subset$age - subset$work_years) < 18 |
      subset$bmi < 12 | subset$bmi > 60
  )
  age_work_conflict[is.na(age_work_conflict)] <- FALSE

  keep <- !(missing_key | work_lt_1y | age_work_conflict)
  clean <- subset[keep, , drop = FALSE]
  raw_clean <- df[keep, , drop = FALSE]

  exclusions <- data.frame(
    item = c(
      "Raw N",
      "Missing any key analysis variable",
      "Work tenure <1 year",
      "Age/work tenure/BMI logical conflict",
      "Final analysis N"
    ),
    n = c(
      original_n,
      sum(missing_key),
      sum(!missing_key & work_lt_1y),
      sum(!missing_key & !work_lt_1y & age_work_conflict),
      nrow(clean)
    ),
    stringsAsFactors = FALSE
  )

  list(clean = clean, raw_clean = raw_clean, exclusions = exclusions)
}

logsumexp_rows <- function(mat) {
  m <- apply(mat, 1, max)
  m + log(rowSums(exp(mat - m)))
}

fit_lca <- function(X, k, n_starts = 20, max_iter = 600, tol = 1e-7, seed = 20260601) {
  set.seed(seed + k)
  n <- nrow(X)
  p <- ncol(X)
  best <- NULL
  X <- as.matrix(X)

  for (start in seq_len(n_starts)) {
    pi_k <- as.numeric(stats::rgamma(k, shape = 1, rate = 1))
    pi_k <- pi_k / sum(pi_k)
    theta <- matrix(stats::runif(k * p, min = 0.08, max = 0.92), nrow = k, ncol = p)
    last_ll <- -Inf

    for (iter in seq_len(max_iter)) {
      log_prob <- matrix(log(pi_k + 1e-15), nrow = n, ncol = k, byrow = TRUE)
      log_prob <- log_prob +
        X %*% t(log(theta + 1e-12)) +
        (1 - X) %*% t(log(1 - theta + 1e-12))
      ll_i <- logsumexp_rows(log_prob)
      ll <- sum(ll_i)
      resp <- exp(log_prob - ll_i)
      nk <- colSums(resp) + 1e-12
      pi_k <- nk / n
      theta <- t(resp) %*% X
      theta <- sweep(theta, 1, nk, "/")
      theta <- pmin(pmax(theta, 1e-5), 1 - 1e-5)
      if (abs(ll - last_ll) < tol) break
      last_ll <- ll
    }

    if (is.null(best) || ll > best$ll) {
      entropy <- -sum(resp * log(resp + 1e-15))
      rel_entropy <- if (k > 1) 1 - entropy / (n * log(k)) else 1
      params <- (k - 1) + k * p
      best <- list(
        k = k,
        ll = ll,
        aic = -2 * ll + 2 * params,
        bic = -2 * ll + params * log(n),
        abic = -2 * ll + params * log((n + 2) / 24),
        entropy = rel_entropy,
        pi = pi_k,
        theta = theta,
        resp = resp,
        iter = iter
      )
    }
    if (start == 1 || start == n_starts || start %% 3 == 0) {
      log_msg(sprintf(
        "  class %d start %d/%d done; current best LogLik = %.3f",
        k, start, n_starts, best$ll
      ))
    }
  }
  best
}

class_labels <- function(theta) {
  k <- nrow(theta)
  if (k == 6) {
    return(c(
      "Low-symptom",
      "Neck + low back",
      "Neck-shoulder-back",
      "Neck-shoulder-upper limb",
      "Low-back-lower limb",
      "Multisite high-risk"
    ))
  }
  paste0("Class ", seq_len(k))
}

run_lca <- function(clean) {
  X <- clean[, LCA_COLS, drop = FALSE]
  X <- as.matrix(X)
  X <- apply(X, 2, to_num)
  if (is.null(dim(X))) X <- matrix(X, ncol = length(LCA_COLS))
  if (any(is.na(X))) stop("LCA columns contain missing values after cleaning.")

  fits <- vector("list", LCA_MAX_CLASSES)
  checkpoint_dir <- file.path(output_dir, "lca_checkpoints")
  if (CHECKPOINT_LCA_MODELS) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }
  fit_row <- function(fit) {
    data.frame(
      classes = fit$k,
      LogLik = fit$ll,
      AIC = fit$aic,
      BIC = fit$bic,
      aBIC = fit$abic,
      Entropy = fit$entropy,
      min_class_proportion = min(fit$pi),
      stringsAsFactors = FALSE
    )
  }
  is_valid_checkpoint <- function(fit, k) {
    is.list(fit) &&
      identical(as.integer(fit$k), as.integer(k)) &&
      !is.null(fit$resp) && nrow(fit$resp) == nrow(X) &&
      !is.null(fit$theta) && ncol(fit$theta) == ncol(X)
  }
  fit_progress <- data.frame()
  for (k in seq_len(LCA_MAX_CLASSES)) {
    starts <- if (k <= 4) LCA_N_STARTS_SMALL else LCA_N_STARTS_LARGE
    checkpoint_path <- file.path(checkpoint_dir, sprintf("lca_model_%02d_classes.rds", k))
    if (CHECKPOINT_LCA_MODELS && RESUME_LCA_CHECKPOINTS && file.exists(checkpoint_path)) {
      loaded <- tryCatch(readRDS(checkpoint_path), error = function(e) NULL)
      if (is_valid_checkpoint(loaded, k)) {
        fits[[k]] <- loaded
        log_msg(sprintf("Using saved LCA checkpoint for %d class(es).", k))
      }
    }
    if (is.null(fits[[k]])) {
      log_msg(sprintf("Fitting LCA model with %d class(es), starts=%d", k, starts))
      fits[[k]] <- fit_lca(X, k, starts, LCA_MAX_ITER, LCA_TOL, RANDOM_SEED)
    }
    one_fit <- fit_row(fits[[k]])
    fit_progress <- rbind(fit_progress, one_fit)
    if (CHECKPOINT_LCA_MODELS) {
      saveRDS(fits[[k]], file.path(checkpoint_dir, sprintf("lca_model_%02d_classes.rds", k)))
      write.csv(fit_progress, file.path(checkpoint_dir, "lca_fit_progress.csv"),
                row.names = FALSE, fileEncoding = "UTF-8")
      log_msg(sprintf("Saved LCA checkpoint for %d class(es).", k))
    }
    gc()
  }

  fit_table <- fit_progress

  best <- fits[[which.min(fit_table$BIC)]]
  order_idx <- order(rowMeans(best$theta))
  theta <- best$theta[order_idx, , drop = FALSE]
  pi_k <- best$pi[order_idx]
  resp <- best$resp[, order_idx, drop = FALSE]
  labels <- class_labels(theta)

  class_index <- max.col(resp, ties.method = "first")
  clean$latent_class <- factor(labels[class_index], levels = labels)
  clean$latent_class_prob <- apply(resp, 1, max)

  prob_table <- data.frame(
    latent_class = labels,
    model_estimated_proportion = pi_k,
    theta,
    stringsAsFactors = FALSE
  )
  names(prob_table)[3:ncol(prob_table)] <- LCA_COLS

  class_distribution <- data.frame(
    latent_class = labels,
    n = as.integer(table(clean$latent_class)[labels]),
    stringsAsFactors = FALSE
  )
  class_distribution$percent <- class_distribution$n / nrow(clean) * 100

  list(
    clean = clean,
    fit_table = fit_table,
    prob_table = prob_table,
    class_distribution = class_distribution,
    labels = labels
  )
}

continuous_by_class <- function(df, vars) {
  rows <- list()
  idx <- 1
  classes <- levels(df$latent_class)
  for (v in intersect(vars, names(df))) {
    x <- to_num(df[[v]])
    for (cl in classes) {
      z <- x[df$latent_class == cl]
      z <- z[is.finite(z)]
      rows[[idx]] <- data.frame(
        variable = v,
        latent_class = cl,
        n = length(z),
        mean = ifelse(length(z) > 0, mean(z), NA_real_),
        sd = ifelse(length(z) > 1, sd(z), NA_real_),
        median = ifelse(length(z) > 0, median(z), NA_real_),
        q1 = ifelse(length(z) > 0, as.numeric(quantile(z, 0.25)), NA_real_),
        q3 = ifelse(length(z) > 0, as.numeric(quantile(z, 0.75)), NA_real_),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  safe_rbind(rows, empty_table(
    variable = character(),
    latent_class = character(),
    n = integer(),
    mean = numeric(),
    sd = numeric(),
    median = numeric(),
    q1 = numeric(),
    q3 = numeric()
  ))
}

categorical_by_class <- function(df, vars) {
  rows <- list()
  idx <- 1
  classes <- levels(df$latent_class)
  for (v in intersect(vars, names(df))) {
    for (cl in classes) {
      sub <- df[df$latent_class == cl, v, drop = TRUE]
      sub <- sub[!is.na(sub) & sub != ""]
      n_total <- length(sub)
      if (n_total == 0) next
      tab <- sort(table(sub), decreasing = TRUE)
      for (lev in names(tab)) {
        rows[[idx]] <- data.frame(
          variable = v,
          latent_class = cl,
          level = lev,
          n = as.integer(tab[lev]),
          percent = as.integer(tab[lev]) / n_total * 100,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }
  safe_rbind(rows, empty_table(
    variable = character(),
    latent_class = character(),
    level = character(),
    n = integer(),
    percent = numeric()
  ))
}

overall_site_prevalence <- function(df) {
  data.frame(
    site = LCA_COLS,
    n_symptom = colSums(df[, LCA_COLS, drop = FALSE] == 1, na.rm = TRUE),
    n_nonmissing = colSums(!is.na(df[, LCA_COLS, drop = FALSE])),
    prevalence_percent = colMeans(df[, LCA_COLS, drop = FALSE] == 1, na.rm = TRUE) * 100,
    stringsAsFactors = FALSE
  )
}

scale_for_or <- function(x) {
  x <- to_num(x)
  vals <- sort(unique(x[is.finite(x)]))
  if (length(vals) == 2 && all(vals %in% c(0, 1))) return(x)
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

pairwise_logistic <- function(df, ref_class = levels(df$latent_class)[1]) {
  predictors <- c(
    "night_shift", "over40h_any", "heavy_work_any", "moderate_work_any",
    "work_physical_activity", "occupational_exposure_any", "blood_body_fluid_any",
    "disinfectant_weekly_mean", "disinfectant_daily_mean",
    "anesthetic_gas_weekly", "anesthetic_gas_daily",
    "antineoplastic_weekly", "antineoplastic_daily",
    "antiviral_weekly", "antiviral_daily",
    "ionizing_radiation_weekly", "ionizing_radiation_daily",
    "psqi_total", "ess_total", "swsd_score", "sps6_total", "pss_total",
    "phq9_total", "gad7_total", "fatigue_total", "mbi_total",
    "social_support", "negative_behavior", "sharp_injury_score",
    "acute_lowback_score", "ionizing_radiation_score", "nonionizing_radiation_score",
    "noise_score", "chemical_disinfectant_score", "msk_ergonomic_score",
    "repetitive_strain_score", "prolonged_standing_score", "blood_body_fluid_score"
  )
  predictors <- intersect(predictors, names(df))
  classes <- setdiff(levels(df$latent_class), ref_class)
  rows <- list()
  idx <- 1

  for (target in classes) {
    sub0 <- df[df$latent_class %in% c(ref_class, target), , drop = FALSE]
    y <- ifelse(sub0$latent_class == target, 1, 0)
    for (v in predictors) {
      vals <- sort(unique(to_num(sub0[[v]])[is.finite(to_num(sub0[[v]]))]))
      if (length(vals) <= 1) next

      covars <- setdiff(c("age", "work_years", "bmi"), v)
      model_df <- data.frame(
        y = y,
        x = scale_for_or(sub0[[v]]),
        age = to_num(sub0$age),
        work_years = to_num(sub0$work_years),
        bmi = to_num(sub0$bmi)
      )
      keep_cols <- c("y", "x", covars)
      model_df <- model_df[, keep_cols, drop = FALSE]
      model_df <- model_df[complete.cases(model_df), , drop = FALSE]
      if (nrow(model_df) < 100 || length(unique(model_df$y)) < 2) next

      fit <- tryCatch(
        glm(y ~ ., data = model_df, family = binomial()),
        error = function(e) NULL,
        warning = function(w) suppressWarnings(glm(y ~ ., data = model_df, family = binomial()))
      )
      if (is.null(fit)) next
      sm <- summary(fit)$coefficients
      if (!"x" %in% rownames(sm)) next
      beta <- sm["x", "Estimate"]
      se <- sm["x", "Std. Error"]
      p <- sm["x", "Pr(>|z|)"]
      binary <- length(vals) == 2 && all(vals %in% c(0, 1))
      rows[[idx]] <- data.frame(
        comparison = paste(target, "vs", ref_class),
        variable = v,
        n = nrow(model_df),
        OR = exp(beta),
        CI_low = exp(beta - 1.96 * se),
        CI_high = exp(beta + 1.96 * se),
        p_value = p,
        p_display = fmt_p(p),
        note = ifelse(binary,
                      "binary yes vs no; adjusted for age/work years/BMI",
                      "per 1-SD increase; adjusted for age/work years/BMI"),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  safe_rbind(rows, empty_table(
    comparison = character(),
    variable = character(),
    n = integer(),
    OR = numeric(),
    CI_low = numeric(),
    CI_high = numeric(),
    p_value = numeric(),
    p_display = character(),
    note = character()
  ))
}

cronbach_alpha <- function(items) {
  items <- as.data.frame(lapply(items, to_num), check.names = FALSE)
  cc <- complete.cases(items)
  x <- as.matrix(items[cc, , drop = FALSE])
  n <- nrow(x)
  k <- ncol(x)
  if (n < 3 || k < 2) return(list(alpha = NA_real_, n = n, k = k))
  item_var <- apply(x, 2, var)
  total_var <- var(rowSums(x))
  if (!is.finite(total_var) || total_var == 0) return(list(alpha = NA_real_, n = n, k = k))
  list(alpha = (k / (k - 1)) * (1 - sum(item_var) / total_var), n = n, k = k)
}

alpha_interpret <- function(a) {
  ifelse(is.na(a), NA_character_,
         ifelse(a >= 0.90, "excellent",
                ifelse(a >= 0.80, "good",
                       ifelse(a >= 0.70, "acceptable", "questionable/low"))))
}

score_scale_items <- function(raw_clean, clean_with_classes) {
  has_cols <- function(cols) all(cols %in% names(raw_clean))
  get_cols <- function(cols) as.data.frame(lapply(raw_clean[, cols, drop = FALSE], to_num), check.names = FALSE)
  scored <- list()

  cols <- paste0("D_q19_", 1:8)
  if (has_cols(cols)) scored[["ESS"]] <- get_cols(cols) - 1

  cols <- paste0("D_q21_", 1:6)
  if (has_cols(cols)) {
    sps <- get_cols(cols)
    sps[, paste0("D_q21_", 5:6)] <- 6 - sps[, paste0("D_q21_", 5:6)]
    scored[["SPS-6"]] <- sps
  }

  cols <- paste0("D_q22_", 1:7)
  if (has_cols(cols)) scored[["GAD-7"]] <- get_cols(cols) - 1

  cols <- paste0("D_q23_", 1:9)
  if (has_cols(cols)) scored[["PHQ-9"]] <- get_cols(cols) - 1

  cols <- paste0("D_q24_", 1:10)
  if (has_cols(cols)) {
    pss <- get_cols(cols)
    pss[, paste0("D_q24_", c(1, 2, 3, 6, 9, 10))] <-
      pss[, paste0("D_q24_", c(1, 2, 3, 6, 9, 10))] - 1
    pss[, paste0("D_q24_", c(4, 5, 7, 8))] <-
      5 - pss[, paste0("D_q24_", c(4, 5, 7, 8))]
    scored[["PSS-10"]] <- pss
  }

  cols <- paste0("G_q3_", 1:14)
  if (has_cols(cols)) {
    fatigue <- get_cols(cols)
    fatigue_scored <- fatigue
    for (c in paste0("G_q3_", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12))) {
      fatigue_scored[[c]] <- ifelse(is.na(fatigue[[c]]), NA_real_, ifelse(fatigue[[c]] == 1, 1, 0))
    }
    for (c in paste0("G_q3_", c(10, 13, 14))) {
      fatigue_scored[[c]] <- ifelse(is.na(fatigue[[c]]), NA_real_, ifelse(fatigue[[c]] == 2, 1, 0))
    }
    scored[["Fatigue scale total"]] <- fatigue_scored
    scored[["Fatigue scale physical"]] <- fatigue_scored[, paste0("G_q3_", 1:8), drop = FALSE]
    scored[["Fatigue scale mental"]] <- fatigue_scored[, paste0("G_q3_", 9:14), drop = FALSE]
  }

  cols <- paste0("F_q3_", 1:22)
  if (has_cols(cols)) {
    mbi <- get_cols(cols) - 1
    scored[["MBI emotional exhaustion"]] <-
      mbi[, paste0("F_q3_", c(1, 2, 3, 6, 8, 13, 14, 16, 20)), drop = FALSE]
    scored[["MBI depersonalization"]] <-
      mbi[, paste0("F_q3_", c(5, 10, 11, 15, 22)), drop = FALSE]
    scored[["MBI personal accomplishment"]] <-
      mbi[, paste0("F_q3_", c(4, 7, 9, 12, 17, 18, 19, 21)), drop = FALSE]
  }

  cols <- paste0("F_q4_", 1:12)
  if (has_cols(cols)) scored[["Perceived social support"]] <- get_cols(cols)

  cols <- paste0("F_q1_", 1:22)
  if (has_cols(cols)) scored[["Negative acts questionnaire"]] <- get_cols(cols)

  scored[["NMQ 9-site symptoms (KR-20)"]] <- clean_with_classes[, LCA_COLS, drop = FALSE]
  scored
}

compute_alpha_table <- function(raw_clean, clean_with_classes) {
  scored <- score_scale_items(raw_clean, clean_with_classes)
  rows <- lapply(names(scored), function(name) {
    a <- cronbach_alpha(scored[[name]])
    data.frame(
      scale = name,
      items = a$k,
      complete_case_n = a$n,
      cronbach_alpha = a$alpha,
      interpretation = alpha_interpret(a$alpha),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

make_figures <- function(class_distribution, prob_table, fit_table, logit_table) {
  png(file.path(figure_dir, "figure1_class_distribution.png"), width = 1800, height = 1100, res = 180)
  par(mar = c(8, 5, 3, 1))
  bp <- barplot(class_distribution$percent, names.arg = class_distribution$latent_class,
                las = 2, col = "#4C78A8",
                ylim = c(0, max(class_distribution$percent, na.rm = TRUE) * 1.2),
                ylab = "Percentage (%)", main = "Latent class distribution")
  text(bp, class_distribution$percent + 1, labels = sprintf("%.1f%%", class_distribution$percent), cex = 0.8)
  dev.off()

  mat <- as.matrix(prob_table[, LCA_COLS, drop = FALSE])
  rownames(mat) <- prob_table$latent_class
  png(file.path(figure_dir, "figure2_lca_heatmap.png"), width = 2200, height = 1300, res = 180)
  par(mar = c(8, 11, 4, 6))
  image(t(mat[nrow(mat):1, ]), axes = FALSE, col = hcl.colors(100, "YlOrRd"),
        main = "Conditional probabilities of MSK symptoms")
  axis(1, at = seq(0, 1, length.out = ncol(mat)), labels = colnames(mat), las = 2)
  axis(2, at = seq(0, 1, length.out = nrow(mat)), labels = rev(rownames(mat)), las = 2)
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      text((j - 1) / (ncol(mat) - 1), (nrow(mat) - i) / (nrow(mat) - 1),
           sprintf("%.2f", mat[i, j]), cex = 0.65)
    }
  }
  dev.off()

  png(file.path(figure_dir, "figure3_model_fit.png"), width = 1600, height = 1100, res = 180)
  par(mar = c(5, 5, 3, 5))
  plot(fit_table$classes, fit_table$BIC, type = "b", pch = 16, col = "#F58518",
       xlab = "Number of classes", ylab = "Information criterion",
       main = "LCA model fit")
  lines(fit_table$classes, fit_table$AIC, type = "b", pch = 15, col = "#4C78A8")
  legend("topright", legend = c("BIC", "AIC"), pch = c(16, 15),
         col = c("#F58518", "#4C78A8"), bty = "n")
  dev.off()

  high_class <- tail(class_distribution$latent_class, 1)
  ref_class <- class_distribution$latent_class[1]
  forest <- logit_table[logit_table$comparison == paste(high_class, "vs", ref_class), , drop = FALSE]
  keep <- c("night_shift", "over40h_any", "heavy_work_any", "occupational_exposure_any",
            "blood_body_fluid_any", "psqi_total", "ess_total", "swsd_score",
            "sps6_total", "pss_total", "phq9_total", "fatigue_total")
  forest <- forest[match(keep, forest$variable), , drop = FALSE]
  forest <- forest[!is.na(forest$OR), , drop = FALSE]
  labels <- c(
    night_shift = "Night shift", over40h_any = ">40 h/week",
    heavy_work_any = "Heavy physical work",
    occupational_exposure_any = "Occupational exposure",
    blood_body_fluid_any = "Blood/body-fluid exposure",
    psqi_total = "PSQI", ess_total = "ESS", swsd_score = "SWSD",
    sps6_total = "SPS-6", pss_total = "PSS", phq9_total = "PHQ-9",
    fatigue_total = "Fatigue"
  )
  if (nrow(forest) > 0) {
    png(file.path(figure_dir, "figure4_forest_highrisk.png"), width = 1800, height = 1300, res = 180)
    par(mar = c(5, 11, 3, 2))
    y <- seq_len(nrow(forest))
    plot(forest$OR, y, xlim = c(0.8, max(forest$CI_high, na.rm = TRUE) * 1.1),
         ylim = c(0.5, nrow(forest) + 0.5), log = "x", pch = 16,
         yaxt = "n", xlab = "Adjusted OR (log scale)", ylab = "",
         main = paste(high_class, "vs", ref_class))
    segments(forest$CI_low, y, forest$CI_high, y)
    abline(v = 1, lty = 2, col = "gray50")
    axis(2, at = y, labels = labels[forest$variable], las = 2)
    dev.off()
  }
}

run_all <- function() {
  raw_data <- read_input_data()
  message("Raw rows: ", nrow(raw_data))

  prep <- prepare_analysis_data(raw_data)
  clean <- prep$clean
  raw_clean <- prep$raw_clean
  message("Final clean rows: ", nrow(clean))
  write_utf8_csv(prep$exclusions, "exclusions.csv")
  write_utf8_csv(clean, "analysis_clean.csv")

  lca <- run_lca(clean)
  clean_with_classes <- lca$clean
  write_utf8_csv(clean_with_classes, "analysis_clean_with_classes.csv")
  write_utf8_csv(lca$fit_table, "lca_fit.csv")
  write_utf8_csv(lca$prob_table, "class_probabilities.csv")
  write_utf8_csv(lca$class_distribution, "class_distribution.csv")

  cont_vars <- c("age", "work_years", "bmi", "psqi_total", "ess_total", "swsd_score",
                 "sps6_total", "pss_total", "phq9_total", "gad7_total",
                 "fatigue_total", "mbi_total", "social_support", "negative_behavior")
  cat_vars <- c("sex", "education", "marital", "department", "title", "income",
                "night_shift", "over40h_any", "heavy_work_any", "moderate_work_any",
                "occupational_exposure_any", "blood_body_fluid_any",
                "psqi_poor_sleep", "phq9_depression_risk")

  site_prev <- safe_step("overall site prevalence", overall_site_prevalence(clean_with_classes))
  table1_cont <- safe_step("Table 1 continuous variables", continuous_by_class(clean_with_classes, cont_vars))
  table1_cat <- safe_step("Table 1 categorical variables", categorical_by_class(clean_with_classes, cat_vars))
  logit_table <- safe_step("pairwise logistic models", pairwise_logistic(clean_with_classes, lca$labels[1]),
                           fallback = empty_table(
                             comparison = character(), variable = character(), n = integer(),
                             OR = numeric(), CI_low = numeric(), CI_high = numeric(),
                             p_value = numeric(), p_display = character(), note = character()
                           ))
  alpha_table <- safe_step("Cronbach alpha/KR-20", compute_alpha_table(raw_clean, clean_with_classes),
                           fallback = empty_table(
                             scale = character(), items = integer(), complete_case_n = integer(),
                             cronbach_alpha = numeric(), interpretation = character()
                           ))

  output_paths <- c(
    file.path(output_dir, "exclusions.csv"),
    file.path(output_dir, "analysis_clean.csv"),
    file.path(output_dir, "analysis_clean_with_classes.csv"),
    file.path(output_dir, "lca_fit.csv"),
    file.path(output_dir, "class_probabilities.csv"),
    file.path(output_dir, "class_distribution.csv"),
    write_utf8_csv(site_prev, "overall_site_prevalence.csv"),
    write_utf8_csv(table1_cont, "table1_continuous_by_class.csv"),
    write_utf8_csv(table1_cat, "table1_categorical_by_class.csv"),
    write_utf8_csv(logit_table, "pairwise_logistic.csv"),
    write_utf8_csv(alpha_table, "cronbach_alpha.csv")
  )

  if (MAKE_FIGURES) {
    safe_step("figures", make_figures(lca$class_distribution, lca$prob_table, lca$fit_table, logit_table))
  }

  summary_lines <- c(
    "Nurse multisite MSK LCA RStudio analysis",
    paste("Run time:", as.character(Sys.time())),
    paste("Output directory:", normalizePath(output_dir, winslash = "/", mustWork = FALSE)),
    "",
    "Exclusions:",
    paste(prep$exclusions$item, prep$exclusions$n, sep = ": "),
    "",
    "Best LCA model by BIC:",
    paste("Classes:", lca$fit_table$classes[which.min(lca$fit_table$BIC)]),
    paste("BIC:", round(min(lca$fit_table$BIC), 3)),
    "",
    "Class distribution:",
    paste(lca$class_distribution$latent_class,
          paste0(lca$class_distribution$n, " (", sprintf("%.1f", lca$class_distribution$percent), "%)"),
          sep = ": "),
    "",
    "Cronbach alpha / KR-20:",
    if (nrow(alpha_table) == 0) {
      "Alpha table was not produced."
    } else {
      paste(alpha_table$scale, sprintf("%.3f", alpha_table$cronbach_alpha), sep = ": ")
    }
  )
  write_text(summary_lines, "run_summary.txt")

  message("Done. Output directory: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
  invisible(list(
    output_dir = output_dir,
    paths = output_paths,
    exclusions = prep$exclusions,
    fit_table = lca$fit_table,
    class_distribution = lca$class_distribution,
    alpha_table = alpha_table
  ))
}

results <- run_all()
