options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260601)

# Head nurse vs staff nurse occupational-health analysis.
# This script is intentionally standalone and does not overwrite older R scripts.

INSTALL_MISSING <- FALSE
DATA_OBJECT_CANDIDATES <- c("合并数据64114_去重后", "合并数据", "data", "raw_data", "dat")
DEFAULT_CSV <- file.path("analysis_outputs", "late_career_turnover_cleaned", "01_selected_columns.csv")
DEFAULT_XLSX <- "data.xlsx"
DEFAULT_OUT_DIR <- file.path("analysis_outputs", "head_nurse_propensity_health_20260601")

required_packages <- c("data.table", "dplyr", "survey", "broom", "ggplot2", "readxl")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0 && isTRUE(INSTALL_MISSING)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
still_missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(still_missing) > 0) {
  stop(
    "Missing required packages: ", paste(still_missing, collapse = ", "),
    ". Install them or set INSTALL_MISSING <- TRUE if internet access is available.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(survey)
  library(broom)
  library(ggplot2)
  library(readxl)
})

parse_args <- function(args) {
  out <- list()
  i <- 1
  while (i <= length(args)) {
    key <- args[[i]]
    if (startsWith(key, "--") && i < length(args)) {
      out[[substring(key, 3)]] <- args[[i + 1]]
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
csv_path <- args[["csv"]]
csv_env <- Sys.getenv("NURSE_DATA_CSV", unset = "")
if (is.null(csv_path) && nzchar(csv_env)) csv_path <- csv_env
if (is.null(csv_path)) csv_path <- DEFAULT_CSV
xlsx_path <- args[["xlsx"]]
xlsx_env <- Sys.getenv("NURSE_DATA_XLSX", unset = "")
if (is.null(xlsx_path) && nzchar(xlsx_env)) xlsx_path <- xlsx_env
if (is.null(xlsx_path)) xlsx_path <- DEFAULT_XLSX
out_dir <- args[["out-dir"]]
out_env <- Sys.getenv("NURSE_OUTPUT_DIR", unset = "")
if (is.null(out_dir) && nzchar(out_env)) out_dir <- out_env
if (is.null(out_dir)) out_dir <- DEFAULT_OUT_DIR
survey_year <- args[["survey-year"]]
year_env <- Sys.getenv("NURSE_SURVEY_YEAR", unset = "")
if (is.null(survey_year) && nzchar(year_env)) survey_year <- year_env
SURVEY_YEAR <- suppressWarnings(as.integer(ifelse(is.null(survey_year), "2023", survey_year)))
if (!is.finite(SURVEY_YEAR)) SURVEY_YEAR <- 2023L
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

log_lines <- character()
log_msg <- function(...) {
  msg <- paste0(...)
  log_lines <<- c(log_lines, paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", msg))
  message(msg)
}

read_source_data <- function() {
  env_names <- intersect(DATA_OBJECT_CANDIDATES, ls(envir = .GlobalEnv))
  for (nm in env_names) {
    obj <- get(nm, envir = .GlobalEnv)
    if (is.data.frame(obj) || data.table::is.data.table(obj)) {
      log_msg("Using data object from R environment: ", nm)
      return(as.data.frame(obj, check.names = FALSE))
    }
  }

  if (!is.null(csv_path) && file.exists(csv_path)) {
    log_msg("Using selected-column CSV: ", normalizePath(csv_path, winslash = "/", mustWork = FALSE))
    return(as.data.frame(data.table::fread(
      csv_path,
      encoding = "UTF-8",
      check.names = FALSE,
      na.strings = c("", "NA", "N/A", "NULL", "null", "NaN", "nan", "(skip)", "(blank)")
    ), check.names = FALSE))
  }

  if (!is.null(xlsx_path) && file.exists(xlsx_path)) {
    log_msg("Using Excel file via readxl: ", normalizePath(xlsx_path, winslash = "/", mustWork = FALSE))
    return(as.data.frame(readxl::read_excel(xlsx_path, .name_repair = "minimal"), check.names = FALSE))
  }

  stop(
    "No usable data source found. Load one of these objects in R: ",
    paste(DATA_OBJECT_CANDIDATES, collapse = ", "),
    "; or pass --csv path/to/selected_columns.csv; or place data.xlsx in the working directory.",
    call. = FALSE
  )
}

raw <- read_source_data()
n_raw <- nrow(raw)
log_msg("Raw rows: ", format(n_raw, big.mark = ","), "; columns: ", ncol(raw))

get_col <- function(name) {
  if (name %in% names(raw)) raw[[name]] else rep(NA_character_, n_raw)
}

to_num <- function(x, negative_to_na = TRUE) {
  z <- suppressWarnings(as.numeric(trimws(as.character(x))))
  if (negative_to_na) z[z < 0] <- NA_real_
  z
}

to_code_chr <- function(x) {
  z <- trimws(as.character(x))
  z[z %in% c("", "NA", "N/A", "NULL", "null", "NaN", "nan", "(skip)", "(blank)")] <- NA_character_
  z
}

as_code_factor <- function(x, levels_keep = NULL, prefix = "L") {
  z <- to_code_chr(x)
  if (!is.null(levels_keep)) z[!(z %in% as.character(levels_keep))] <- NA_character_
  z <- ifelse(is.na(z), "Missing", paste0(prefix, z))
  factor(z)
}

num_median_impute <- function(x) {
  z <- as.numeric(x)
  med <- suppressWarnings(stats::median(z, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  miss <- is.na(z)
  z[miss] <- med
  list(value = z, missing = as.integer(miss))
}

safe_range <- function(x, min_value, max_value) {
  z <- as.numeric(x)
  z[z < min_value | z > max_value] <- NA_real_
  z
}

first_non_missing <- function(...) {
  xs <- list(...)
  if (length(xs) == 0) return(NULL)
  out <- xs[[1]]
  if (length(xs) > 1) {
    for (i in 2:length(xs)) out <- ifelse(is.na(out), xs[[i]], out)
  }
  out
}

sex_num <- to_num(get_col("A_q2"))
age <- to_num(get_col("A_age"))
birth_year <- to_num(get_col("A_year"))
age <- ifelse(is.na(age) & !is.na(birth_year), SURVEY_YEAR - birth_year, age)

work_years <- to_num(get_col("A_gongzuoshichang"))
work_start_year <- to_num(get_col("work_y"))
work_years <- ifelse(is.na(work_years) & !is.na(work_start_year), SURVEY_YEAR - work_start_year, work_years)

bmi <- to_num(get_col("A_BMI"))
height_cm <- to_num(get_col("A_q15"))
weight_kg <- to_num(get_col("A_q16"))
bmi_from_hw <- weight_kg / (height_cm / 100)^2
bmi <- ifelse(is.na(bmi) & is.finite(bmi_from_hw), bmi_from_hw, bmi)

shift_code <- to_num(get_col("C_q1"))
head_code <- to_num(get_col("A_q12"))
id_chr <- to_code_chr(get_col("id"))

outcome_raw <- list(
  psqi = safe_range(to_num(get_col("D_PSQI_ALL")), 0, 21),
  ess = safe_range(to_num(get_col("D_shishui_ESS")), 0, 24),
  sps6 = safe_range(to_num(get_col("D_shengchanlishousun_all")), 6, 30),
  gad7 = safe_range(to_num(get_col("D_jiaolv_all")), 0, 21),
  phq9 = safe_range(to_num(get_col("D_yiyu_all")), 0, 27),
  pss = safe_range(to_num(get_col("D_yali_all")), 0, 40),
  mbi_ee = safe_range(to_num(get_col("F_qingganshuaijie")), 0, 54),
  mbi_dp = safe_range(to_num(get_col("F_qurengehua")), 0, 30),
  mbi_pa = safe_range(to_num(get_col("F_gerenchengjiugan")), 0, 48),
  turnover_intention = safe_range(to_num(get_col("C_lizhiyiyuan_all")), 6, 24),
  work_family_balance = safe_range(to_num(get_col("C_gzjt_all")), 14, 70)
)

score_range_summary <- data.frame(
  variable = names(outcome_raw),
  n_available_after_range_check = vapply(outcome_raw, function(x) sum(!is.na(x)), integer(1)),
  stringsAsFactors = FALSE
)

dat <- data.frame(
  source_row = seq_len(n_raw),
  id = id_chr,
  head_nurse = ifelse(head_code == 3, 1, ifelse(head_code == 1, 0, NA_real_)),
  admin_position_code = head_code,
  age = age,
  work_years = work_years,
  bmi = bmi,
  sex = as_code_factor(get_col("A_q2"), levels_keep = 1:2, prefix = "sex_"),
  first_education = as_code_factor(get_col("A_q4"), levels_keep = 1:4, prefix = "edu1_"),
  highest_education = as_code_factor(get_col("A_q5"), levels_keep = 1:4, prefix = "edu2_"),
  marital_status = as_code_factor(get_col("A_q6"), levels_keep = 1:6, prefix = "marital_"),
  prior_hospital_count = as_code_factor(get_col("A_q7"), levels_keep = 1:4, prefix = "hosp_"),
  department = as_code_factor(get_col("A_q9"), levels_keep = 1:11, prefix = "dept_"),
  employment_type = as_code_factor(get_col("A_q10"), levels_keep = 1:6, prefix = "emp_"),
  professional_title = as_code_factor(get_col("A_q11"), levels_keep = 1:6, prefix = "title_"),
  sunlight_time = as_code_factor(get_col("A_q14"), levels_keep = 1:3, prefix = "sun_"),
  smoking = as_code_factor(get_col("A_xiyan"), levels_keep = 0:1, prefix = "smoke_"),
  drinking = as_code_factor(get_col("A_yinjiu"), levels_keep = 0:1, prefix = "drink_"),
  pregnant = as_code_factor(get_col("A_q22"), levels_keep = 1:2, prefix = "preg_"),
  shift_pattern = as_code_factor(get_col("C_q1"), levels_keep = 1:4, prefix = "shift_"),
  day_only = ifelse(shift_code == 1, 1, ifelse(shift_code %in% c(2, 3, 4), 0, NA_real_)),
  night_shift = ifelse(shift_code %in% c(2, 3, 4), 1, ifelse(shift_code == 1, 0, NA_real_)),
  psqi = outcome_raw$psqi,
  poor_sleep = ifelse(!is.na(outcome_raw$psqi), as.integer(outcome_raw$psqi >= 8), NA_integer_),
  ess = outcome_raw$ess,
  sps6 = outcome_raw$sps6,
  gad7 = outcome_raw$gad7,
  moderate_anxiety = ifelse(!is.na(outcome_raw$gad7), as.integer(outcome_raw$gad7 >= 10), NA_integer_),
  phq9 = outcome_raw$phq9,
  moderate_depression = ifelse(!is.na(outcome_raw$phq9), as.integer(outcome_raw$phq9 >= 10), NA_integer_),
  pss = outcome_raw$pss,
  mbi_ee = outcome_raw$mbi_ee,
  mbi_dp = outcome_raw$mbi_dp,
  mbi_pa = outcome_raw$mbi_pa,
  turnover_intention = outcome_raw$turnover_intention,
  work_family_balance = outcome_raw$work_family_balance,
  stringsAsFactors = FALSE
)

dat <- dat %>%
  mutate(
    duplicate_id_after_first = ifelse(!is.na(id) & id != "", duplicated(id), FALSE),
    exposure_not_main_contrast = !(admin_position_code %in% c(1, 3)),
    invalid_age = is.na(age) | age < 18 | age > 65,
    invalid_work_years = is.na(work_years) | work_years < 0 | work_years > 50,
    inconsistent_age_work = !is.na(age) & !is.na(work_years) & work_years > pmax(age - 16, 0),
    invalid_bmi = !is.na(bmi) & (bmi < 12 | bmi > 60),
    missing_all_main_outcomes = is.na(psqi) & is.na(pss) & is.na(mbi_ee) &
      is.na(gad7) & is.na(phq9) & is.na(turnover_intention) & is.na(work_family_balance)
  )
dat$bmi[dat$invalid_bmi] <- NA_real_

flow_steps <- data.frame(step = character(), n_excluded_at_step = integer(), n_remaining = integer())
current <- rep(TRUE, nrow(dat))
add_step <- function(label, flag, do_exclude = TRUE) {
  flag <- ifelse(is.na(flag), FALSE, flag)
  excluded <- current & flag
  if (do_exclude) current <<- current & !flag
  flow_steps <<- rbind(
    flow_steps,
    data.frame(
      step = label,
      n_excluded_at_step = ifelse(do_exclude, sum(excluded), 0L),
      n_remaining = sum(current),
      stringsAsFactors = FALSE
    )
  )
}

flow_steps <- rbind(flow_steps, data.frame(step = "Raw records", n_excluded_at_step = 0L, n_remaining = nrow(dat)))
add_step("Duplicated participant id after first occurrence", dat$duplicate_id_after_first)
add_step("Administrative position not A_q12=1 or A_q12=3", dat$exposure_not_main_contrast)
add_step("Age missing or outside 18-65 years", dat$invalid_age)
add_step("Work tenure missing or outside 0-50 years", dat$invalid_work_years)
add_step("Work tenure exceeds age minus 16 years", dat$inconsistent_age_work)
add_step("All main outcomes missing or outside valid score ranges", dat$missing_all_main_outcomes)

analysis_dat <- dat[current, , drop = FALSE]
log_msg("Main analysis rows after cleaning: ", format(nrow(analysis_dat), big.mark = ","))

num_covariates <- c("age", "work_years", "bmi")
for (v in num_covariates) {
  imp <- num_median_impute(analysis_dat[[v]])
  analysis_dat[[paste0(v, "_imp")]] <- imp$value
  analysis_dat[[paste0(v, "_missing")]] <- imp$missing
}

cat_covariates <- c(
  "sex", "first_education", "highest_education", "marital_status",
  "prior_hospital_count", "department", "employment_type",
  "professional_title", "sunlight_time", "smoking", "drinking", "pregnant"
)

candidate_ps_covariates <- c(
  paste0(num_covariates, "_imp"),
  paste0(num_covariates, "_missing"),
  cat_covariates
)

usable_covariates <- candidate_ps_covariates[vapply(candidate_ps_covariates, function(v) {
  x <- analysis_dat[[v]]
  if (is.null(x)) return(FALSE)
  if (is.numeric(x) || is.integer(x)) return(length(unique(x[!is.na(x)])) > 1)
  length(unique(as.character(x[!is.na(x)]))) > 1
}, logical(1))]

ps_formula <- stats::as.formula(paste("head_nurse ~", paste(usable_covariates, collapse = " + ")))

fit_ps <- function(input_dat, label) {
  ps_fit <- stats::glm(ps_formula, data = input_dat, family = stats::binomial())
  ps <- as.numeric(stats::predict(ps_fit, type = "response"))
  ps <- pmin(pmax(ps, 0.001), 0.999)
  weights <- ifelse(input_dat$head_nurse == 1, 1, ps / (1 - ps))
  cap <- as.numeric(stats::quantile(weights, 0.99, na.rm = TRUE, names = FALSE))
  if (!is.finite(cap) || cap <= 0) cap <- max(weights, na.rm = TRUE)
  weights_truncated <- pmin(weights, cap)
  input_dat$ps <- ps
  input_dat$att_weight <- weights_truncated
  input_dat$weight_truncation_p99 <- cap
  attr(input_dat, "ps_label") <- label
  attr(input_dat, "ps_model") <- ps_fit
  input_dat
}

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

wvar <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) <= 1) return(NA_real_)
  mu <- wmean(x[ok], w[ok])
  sum(w[ok] * (x[ok] - mu)^2) / sum(w[ok])
}

smd_cont <- function(x, treat, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  xt <- x[treat == 1]
  xc <- x[treat == 0]
  wt <- w[treat == 1]
  wc <- w[treat == 0]
  mt <- wmean(xt, wt)
  mc <- wmean(xc, wc)
  pooled <- sqrt((wvar(xt, wt) + wvar(xc, wc)) / 2)
  if (!is.finite(pooled) || pooled == 0) return(NA_real_)
  (mt - mc) / pooled
}

smd_factor_max <- function(x, treat, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  levs <- sort(unique(as.character(x[!is.na(x)])))
  if (length(levs) <= 1) return(NA_real_)
  vals <- vapply(levs, function(lv) {
    smd_cont(as.numeric(as.character(x) == lv), treat, w)
  }, numeric(1))
  max(abs(vals), na.rm = TRUE)
}

balance_table <- function(input_dat, covariates) {
  before <- vapply(covariates, function(v) {
    x <- input_dat[[v]]
    if (is.numeric(x) || is.integer(x)) {
      smd_cont(as.numeric(x), input_dat$head_nurse)
    } else {
      smd_factor_max(x, input_dat$head_nurse)
    }
  }, numeric(1))
  after <- vapply(covariates, function(v) {
    x <- input_dat[[v]]
    if (is.numeric(x) || is.integer(x)) {
      smd_cont(as.numeric(x), input_dat$head_nurse, input_dat$att_weight)
    } else {
      smd_factor_max(x, input_dat$head_nurse, input_dat$att_weight)
    }
  }, numeric(1))
  data.frame(
    covariate = covariates,
    smd_before = before,
    smd_after_att_weighting = after,
    abs_smd_before = abs(before),
    abs_smd_after_att_weighting = abs(after),
    stringsAsFactors = FALSE
  )
}

outcome_specs <- data.frame(
  outcome = c(
    "psqi", "poor_sleep", "pss", "mbi_ee", "mbi_dp", "mbi_pa", "gad7",
    "moderate_anxiety", "phq9", "moderate_depression", "turnover_intention",
    "sps6", "work_family_balance", "ess"
  ),
  label = c(
    "PSQI total score", "Poor sleep quality (PSQI >= 8)", "PSS perceived stress",
    "MBI emotional exhaustion", "MBI depersonalization", "MBI personal accomplishment",
    "GAD-7 anxiety score", "Moderate anxiety (GAD-7 >= 10)",
    "PHQ-9 depression score", "Moderate depression (PHQ-9 >= 10)",
    "Turnover-intention score", "SPS-6 productivity loss",
    "Work-family balance score", "ESS daytime sleepiness"
  ),
  type = c(
    "continuous", "binary", "continuous", "continuous", "continuous", "continuous",
    "continuous", "binary", "continuous", "binary", "continuous", "continuous",
    "continuous", "continuous"
  ),
  primary = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

fit_weighted_outcome <- function(input_dat, outcome, outcome_label, type, model_label, adjust_night = FALSE) {
  keep <- !is.na(input_dat[[outcome]]) & !is.na(input_dat$head_nurse) &
    !is.na(input_dat$att_weight) & input_dat$att_weight > 0
  if (adjust_night) keep <- keep & !is.na(input_dat$night_shift)
  dd <- input_dat[keep, , drop = FALSE]
  if (nrow(dd) < 30 || length(unique(dd$head_nurse)) < 2) {
    return(data.frame(
      model = model_label, outcome = outcome, label = outcome_label, type = type,
      n = nrow(dd), n_head_nurse = sum(dd$head_nurse == 1), n_staff = sum(dd$head_nurse == 0),
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
      head_mean_or_prev = NA_real_, staff_mean_or_prev = NA_real_,
      contrast = ifelse(type == "binary", "PR", "Mean difference"),
      note = "Skipped: too few rows or one exposure group absent",
      stringsAsFactors = FALSE
    ))
  }

  rhs <- "head_nurse"
  if (adjust_night) rhs <- paste(rhs, "+ night_shift")
  fm <- stats::as.formula(paste(outcome, "~", rhs))
  des <- survey::svydesign(ids = ~1, weights = ~att_weight, data = dd)

  fit <- tryCatch({
    if (type == "binary") {
      survey::svyglm(fm, design = des, family = quasipoisson(link = "log"))
    } else {
      survey::svyglm(fm, design = des, family = gaussian())
    }
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(data.frame(
      model = model_label, outcome = outcome, label = outcome_label, type = type,
      n = nrow(dd), n_head_nurse = sum(dd$head_nurse == 1), n_staff = sum(dd$head_nurse == 0),
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
      head_mean_or_prev = NA_real_, staff_mean_or_prev = NA_real_,
      contrast = ifelse(type == "binary", "PR", "Mean difference"),
      note = paste("Model failed:", fit$message),
      stringsAsFactors = FALSE
    ))
  }

  co <- summary(fit)$coefficients
  beta <- co["head_nurse", "Estimate"]
  se <- co["head_nurse", "Std. Error"]
  p <- co["head_nurse", ncol(co)]
  if (type == "binary") {
    estimate <- exp(beta)
    low <- exp(beta - 1.96 * se)
    high <- exp(beta + 1.96 * se)
  } else {
    estimate <- beta
    low <- beta - 1.96 * se
    high <- beta + 1.96 * se
  }

  head_mean <- wmean(dd[[outcome]][dd$head_nurse == 1], dd$att_weight[dd$head_nurse == 1])
  staff_mean <- wmean(dd[[outcome]][dd$head_nurse == 0], dd$att_weight[dd$head_nurse == 0])

  data.frame(
    model = model_label,
    outcome = outcome,
    label = outcome_label,
    type = type,
    n = nrow(dd),
    n_head_nurse = sum(dd$head_nurse == 1),
    n_staff = sum(dd$head_nurse == 0),
    estimate = estimate,
    conf_low = low,
    conf_high = high,
    p_value = p,
    head_mean_or_prev = head_mean,
    staff_mean_or_prev = staff_mean,
    contrast = ifelse(type == "binary", "PR", "Mean difference"),
    note = "",
    stringsAsFactors = FALSE
  )
}

analysis_dat <- fit_ps(analysis_dat, "Model 1/2 full main contrast")
balance_m1 <- balance_table(analysis_dat, usable_covariates)

model1_results <- do.call(rbind, lapply(seq_len(nrow(outcome_specs)), function(i) {
  fit_weighted_outcome(
    analysis_dat,
    outcome_specs$outcome[[i]],
    outcome_specs$label[[i]],
    outcome_specs$type[[i]],
    model_label = "Model 1: ATT weighted, no night-shift adjustment",
    adjust_night = FALSE
  )
}))

model2_results <- do.call(rbind, lapply(seq_len(nrow(outcome_specs)), function(i) {
  fit_weighted_outcome(
    analysis_dat,
    outcome_specs$outcome[[i]],
    outcome_specs$label[[i]],
    outcome_specs$type[[i]],
    model_label = "Model 2: ATT weighted, plus night-shift adjustment",
    adjust_night = TRUE
  )
}))

day_dat <- analysis_dat %>% filter(day_only == 1)
day_results <- NULL
balance_m3 <- data.frame()
if (nrow(day_dat) >= 30 && length(unique(day_dat$head_nurse)) == 2) {
  day_dat <- fit_ps(day_dat, "Model 3 day-shift-only")
  balance_m3 <- balance_table(day_dat, usable_covariates)
  day_results <- do.call(rbind, lapply(seq_len(nrow(outcome_specs)), function(i) {
    fit_weighted_outcome(
      day_dat,
      outcome_specs$outcome[[i]],
      outcome_specs$label[[i]],
      outcome_specs$type[[i]],
      model_label = "Model 3: day-shift-only ATT weighted",
      adjust_night = FALSE
    )
  }))
} else {
  day_results <- data.frame(
    model = "Model 3: day-shift-only ATT weighted",
    outcome = outcome_specs$outcome,
    label = outcome_specs$label,
    type = outcome_specs$type,
    n = nrow(day_dat),
    n_head_nurse = sum(day_dat$head_nurse == 1),
    n_staff = sum(day_dat$head_nurse == 0),
    estimate = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    p_value = NA_real_,
    head_mean_or_prev = NA_real_,
    staff_mean_or_prev = NA_real_,
    contrast = ifelse(outcome_specs$type == "binary", "PR", "Mean difference"),
    note = "Skipped: day-shift subset too small or one exposure group absent",
    stringsAsFactors = FALSE
  )
}

all_results <- bind_rows(model1_results, model2_results, day_results) %>%
  group_by(model) %>%
  mutate(
    p_fdr = p.adjust(p_value, method = "BH"),
    primary = outcome %in% outcome_specs$outcome[outcome_specs$primary]
  ) %>%
  ungroup()

baseline_summary <- analysis_dat %>%
  group_by(head_nurse) %>%
  summarise(
    n = n(),
    day_only_n = sum(day_only == 1, na.rm = TRUE),
    day_only_pct = 100 * mean(day_only == 1, na.rm = TRUE),
    night_shift_n = sum(night_shift == 1, na.rm = TRUE),
    night_shift_pct = 100 * mean(night_shift == 1, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    work_years_mean = mean(work_years, na.rm = TRUE),
    work_years_sd = sd(work_years, na.rm = TRUE),
    psqi_mean = mean(psqi, na.rm = TRUE),
    pss_mean = mean(pss, na.rm = TRUE),
    mbi_ee_mean = mean(mbi_ee, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(head_nurse == 1, "Head nurse", "Staff nurse")) %>%
  select(group, everything(), -head_nurse)

ps_summary <- data.frame(
  metric = c(
    "n_analysis", "n_head_nurse", "n_staff", "treated_day_only_pct", "staff_day_only_pct",
    "model1_weight_p99_cap", "max_abs_smd_before", "max_abs_smd_after",
    "model3_n_day_only", "model3_n_head_nurse", "model3_n_staff",
    "model3_max_abs_smd_after"
  ),
  value = c(
    nrow(analysis_dat),
    sum(analysis_dat$head_nurse == 1),
    sum(analysis_dat$head_nurse == 0),
    100 * mean(analysis_dat$day_only[analysis_dat$head_nurse == 1] == 1, na.rm = TRUE),
    100 * mean(analysis_dat$day_only[analysis_dat$head_nurse == 0] == 1, na.rm = TRUE),
    unique(analysis_dat$weight_truncation_p99)[1],
    max(balance_m1$abs_smd_before, na.rm = TRUE),
    max(balance_m1$abs_smd_after_att_weighting, na.rm = TRUE),
    nrow(day_dat),
    sum(day_dat$head_nurse == 1),
    sum(day_dat$head_nurse == 0),
    ifelse(nrow(balance_m3) > 0, max(balance_m3$abs_smd_after_att_weighting, na.rm = TRUE), NA_real_)
  )
)

cleaning_rules_md <- c(
  "# Cleaning basis for head-nurse occupational-health analysis",
  "",
  "1. Main exposure follows the questionnaire definition: A_q12=3 is head nurse and A_q12=1 is staff nurse without an administrative role.",
  "2. A_q12=2,4,5,6,7 and missing administrative-position records are excluded from the main contrast to keep the exposure clinically interpretable.",
  "3. Duplicate participant IDs are excluded after the first record to avoid double-counting the same nurse.",
  paste0("4. Age is taken from A_age when available, otherwise calculated as ", SURVEY_YEAR, " minus A_year; records outside 18-65 years are excluded."),
  paste0("5. Work tenure is taken from A_gongzuoshichang when available, otherwise calculated as ", SURVEY_YEAR, " minus work_y; records outside 0-50 years or exceeding age minus 16 years are excluded."),
  "6. BMI uses A_BMI when available, otherwise height/weight; implausible BMI values outside 12-60 kg/m2 are set to missing but do not exclude the record.",
  "7. Questionnaire scores outside their possible ranges are set to missing: PSQI 0-21, ESS 0-24, SPS-6 6-30, GAD-7 0-21, PHQ-9 0-27, PSS 0-40, MBI-EE 0-54, MBI-DP 0-30, MBI-PA 0-48, turnover intention 6-24, WFB 14-70.",
  "8. Records with all main outcomes missing after score-range checks are excluded; outcome models then use outcome-specific complete cases.",
  "9. Propensity-score covariates are baseline or relatively stable variables: age, work tenure, BMI, sex, education, marital status, prior hospital count, department, employment type, professional title, sunlight, smoking, drinking, and pregnancy status.",
  "10. Night shift C_q1 is not included in the propensity-score model because it may be part of the head-nurse role pathway. It is handled through the three-model strategy: unadjusted for night shift, adjusted for night shift in the outcome model, and restricted to day-shift-only nurses.",
  "11. Missing baseline covariates are retained using median imputation plus missing indicators for numeric variables and an explicit Missing category for categorical variables.",
  "12. The main estimand is ATT, implemented as propensity-score weighting: head nurses weight=1, staff nurses weight=PS/(1-PS), with the 99th percentile used to cap extreme weights."
)

summary_md <- c(
  "# Local trial run summary",
  "",
  paste0("- Raw rows: ", format(n_raw, big.mark = ",")),
  paste0("- Main cleaned analysis rows: ", format(nrow(analysis_dat), big.mark = ",")),
  paste0("- Head nurses: ", format(sum(analysis_dat$head_nurse == 1), big.mark = ",")),
  paste0("- Staff nurses: ", format(sum(analysis_dat$head_nurse == 0), big.mark = ",")),
  sprintf("- Day-shift only: head nurses %.1f%% vs staff nurses %.1f%%",
          100 * mean(analysis_dat$day_only[analysis_dat$head_nurse == 1] == 1, na.rm = TRUE),
          100 * mean(analysis_dat$day_only[analysis_dat$head_nurse == 0] == 1, na.rm = TRUE)),
  sprintf("- Maximum absolute SMD before weighting: %.3f", max(balance_m1$abs_smd_before, na.rm = TRUE)),
  sprintf("- Maximum absolute SMD after ATT weighting: %.3f", max(balance_m1$abs_smd_after_att_weighting, na.rm = TRUE)),
  "",
  "## Main model highlights",
  ""
)

highlight <- all_results %>%
  filter(model == "Model 1: ATT weighted, no night-shift adjustment", outcome %in% c("psqi", "poor_sleep", "pss", "mbi_ee", "gad7", "phq9", "turnover_intention", "work_family_balance")) %>%
  mutate(
    line = ifelse(
      type == "binary",
      sprintf("- %s: PR %.3f (95%% CI %.3f to %.3f), p=%s",
              label, estimate, conf_low, conf_high,
              ifelse(is.na(p_value), "NA", ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)))),
      sprintf("- %s: mean difference %.3f (95%% CI %.3f to %.3f), p=%s",
              label, estimate, conf_low, conf_high,
              ifelse(is.na(p_value), "NA", ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))))
    )
  ) %>%
  pull(line)
summary_md <- c(summary_md, highlight)

data.table::fwrite(flow_steps, file.path(out_dir, "01_cleaning_flow.csv"))
data.table::fwrite(score_range_summary, file.path(out_dir, "02_score_range_summary.csv"))
data.table::fwrite(baseline_summary, file.path(out_dir, "03_baseline_shift_summary.csv"))
data.table::fwrite(ps_summary, file.path(out_dir, "04_propensity_summary.csv"))
data.table::fwrite(balance_m1, file.path(out_dir, "05_balance_model1_att.csv"))
if (nrow(balance_m3) > 0) data.table::fwrite(balance_m3, file.path(out_dir, "06_balance_model3_day_only_att.csv"))
data.table::fwrite(all_results, file.path(out_dir, "07_outcome_models_att.csv"))
writeLines(cleaning_rules_md, file.path(out_dir, "00_cleaning_rules.md"), useBytes = TRUE)
writeLines(summary_md, file.path(out_dir, "analysis_summary.md"), useBytes = TRUE)
writeLines(log_lines, file.path(out_dir, "analysis_run_log.txt"), useBytes = TRUE)

plot_dat <- all_results %>%
  filter(model %in% c(
    "Model 1: ATT weighted, no night-shift adjustment",
    "Model 2: ATT weighted, plus night-shift adjustment",
    "Model 3: day-shift-only ATT weighted"
  )) %>%
  filter(type == "continuous", outcome %in% c("psqi", "pss", "mbi_ee", "gad7", "phq9", "turnover_intention", "work_family_balance")) %>%
  filter(!is.na(estimate))

if (nrow(plot_dat) > 0) {
  p <- ggplot(plot_dat, aes(x = estimate, y = label, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high), position = position_dodge(width = 0.6)) +
    labs(x = "Head nurse minus staff nurse weighted mean difference", y = NULL, color = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(out_dir, "figure_continuous_outcome_differences.png"), p, width = 9, height = 5.5, dpi = 300)
}

log_msg("Saved outputs to: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
