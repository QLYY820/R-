options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260601)

# Head nurse vs staff nurse occupational-health analysis.
# This script is intentionally standalone and does not overwrite older R scripts.

env_install_missing <- toupper(Sys.getenv("HEAD_NURSE_INSTALL_MISSING", unset = "FALSE"))
global_install_missing <- exists("INSTALL_MISSING", envir = .GlobalEnv, inherits = FALSE) &&
  isTRUE(get("INSTALL_MISSING", envir = .GlobalEnv))
INSTALL_MISSING <- global_install_missing || env_install_missing %in% c("TRUE", "T", "1", "YES", "Y")
DATA_OBJECT_CANDIDATES <- c("合并数据64114_去重后", "合并数据", "data", "raw_data", "dat")
DEFAULT_CSV <- file.path("analysis_outputs", "late_career_turnover_cleaned", "01_selected_columns.csv")
DEFAULT_XLSX <- "data.xlsx"
DEFAULT_OUT_DIR <- file.path("analysis_outputs", "head_nurse_propensity_health_20260601")

required_packages <- c("data.table", "dplyr", "survey", "ggplot2")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0 && isTRUE(INSTALL_MISSING)) {
  user_lib <- Sys.getenv("R_LIBS_USER", unset = "")
  if (nzchar(user_lib)) {
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(unique(c(user_lib, .libPaths())))
  }
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
still_missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(still_missing) > 0) {
  stop(
    "Missing required packages: ", paste(still_missing, collapse = ", "),
    ". Run install.packages(c(",
    paste(sprintf('\"%s\"', still_missing), collapse = ", "),
    "), repos = \"https://cloud.r-project.org\") or set Sys.setenv(HEAD_NURSE_INSTALL_MISSING = \"TRUE\") before sourcing the script.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(survey)
  library(ggplot2)
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

safe_z <- function(x) {
  z <- as.numeric(x)
  s <- stats::sd(z, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(z)))
  as.numeric((z - mean(z, na.rm = TRUE)) / s)
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

# Conceptual IJNS-facing risk domains. Positive values indicate a worse
# risk profile. These are not replacements for individual validated scales;
# they summarize the hypothesized risk transition from circadian disruption
# to managerial strain.
analysis_dat$work_family_imbalance <- -analysis_dat$work_family_balance
analysis_dat$circadian_disruption_domain <- rowMeans(
  data.frame(
    psqi_z = safe_z(analysis_dat$psqi),
    ess_z = safe_z(analysis_dat$ess),
    productivity_loss_z = safe_z(analysis_dat$sps6)
  ),
  na.rm = TRUE
)
analysis_dat$circadian_disruption_domain[!is.finite(analysis_dat$circadian_disruption_domain)] <- NA_real_
analysis_dat$management_strain_domain <- rowMeans(
  data.frame(
    pss_z = safe_z(analysis_dat$pss),
    mbi_ee_z = safe_z(analysis_dat$mbi_ee),
    gad7_z = safe_z(analysis_dat$gad7),
    turnover_z = safe_z(analysis_dat$turnover_intention),
    work_family_imbalance_z = safe_z(analysis_dat$work_family_imbalance)
  ),
  na.rm = TRUE
)
analysis_dat$management_strain_domain[!is.finite(analysis_dat$management_strain_domain)] <- NA_real_

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

fmt_mean_sd <- function(mean_value, sd_value, digits = 1) {
  if (!is.finite(mean_value)) return("")
  if (!is.finite(sd_value)) sd_value <- NA_real_
  paste0(sprintf(paste0("%.", digits, "f"), mean_value), " (", sprintf(paste0("%.", digits, "f"), sd_value), ")")
}

fmt_n_pct <- function(n_value, denom_value) {
  if (!is.finite(n_value) || !is.finite(denom_value) || denom_value <= 0) return("")
  sprintf("%s (%.1f)", format(round(n_value), big.mark = ","), 100 * n_value / denom_value)
}

fmt_pct <- function(p_value) {
  if (!is.finite(p_value)) return("")
  sprintf("%.1f", 100 * p_value)
}

fmt_p <- function(p_value) {
  if (!is.finite(p_value)) return("")
  if (p_value < 0.001) return("<0.001")
  sprintf("%.3f", p_value)
}

sort_levels_by_code <- function(x) {
  z <- as.character(x)
  code <- suppressWarnings(as.numeric(sub(".*_([0-9]+)$", "\\1", z)))
  z[order(ifelse(is.na(code), Inf, code), z)]
}

clean_level_label <- function(var, level) {
  lv <- as.character(level)
  if (is.na(lv) || lv == "Missing") return("Missing")
  maps <- list(
    sex = c(sex_1 = "Male", sex_2 = "Female"),
    first_education = c(edu1_1 = "Technical secondary school", edu1_2 = "Junior college", edu1_3 = "Bachelor's degree", edu1_4 = "Master's degree or above"),
    highest_education = c(edu2_1 = "Technical secondary school", edu2_2 = "Junior college", edu2_3 = "Bachelor's degree", edu2_4 = "Master's degree or above"),
    marital_status = c(marital_1 = "Unmarried", marital_2 = "Married", marital_3 = "Divorced", marital_4 = "Widowed", marital_5 = "Remarried", marital_6 = "Other"),
    prior_hospital_count = c(hosp_1 = "1", hosp_2 = "2", hosp_3 = "3", hosp_4 = ">3"),
    department = c(dept_1 = "Internal medicine", dept_2 = "Surgery", dept_3 = "Emergency", dept_4 = "Gynecology", dept_5 = "Obstetrics", dept_6 = "Pediatrics", dept_7 = "Operating room", dept_8 = "ICU", dept_9 = "Outpatient", dept_10 = "Administration", dept_11 = "Other"),
    employment_type = c(emp_1 = "Permanent", emp_2 = "Contract", emp_3 = "Personnel agency", emp_4 = "Filing staff", emp_5 = "Labor dispatch", emp_6 = "Other"),
    professional_title = c(title_1 = "Junior nurse", title_2 = "Senior nurse", title_3 = "Supervisor nurse", title_4 = "Co-chief nurse", title_5 = "Chief nurse", title_6 = "Other"),
    sunlight_time = c(sun_1 = "<1 hour", sun_2 = "1-3 hours", sun_3 = ">3 hours"),
    smoking = c(smoke_0 = "No", smoke_1 = "Yes"),
    drinking = c(drink_0 = "No", drink_1 = "Yes"),
    pregnant = c(preg_1 = "Yes", preg_2 = "No"),
    shift_pattern = c(shift_1 = "Day shift only", shift_2 = "Evening shift only", shift_3 = "Night shift only", shift_4 = "Rotating day/night shifts")
  )
  if (!is.null(maps[[var]]) && lv %in% names(maps[[var]])) return(unname(maps[[var]][[lv]]))
  code <- sub("^[^_]+_", "", lv)
  paste("Code", code)
}

baseline_cont_row <- function(input_dat, var, label, digits = 1) {
  x <- input_dat[[var]]
  treat <- input_dat$head_nurse
  w <- input_dat$att_weight
  head <- treat == 1
  staff <- treat == 0
  data.frame(
    characteristic = label,
    level = "",
    head_unweighted = fmt_mean_sd(mean(x[head], na.rm = TRUE), sd(x[head], na.rm = TRUE), digits),
    staff_unweighted = fmt_mean_sd(mean(x[staff], na.rm = TRUE), sd(x[staff], na.rm = TRUE), digits),
    smd_before = smd_cont(x, treat),
    head_att_weighted = fmt_mean_sd(wmean(x[head], w[head]), sqrt(wvar(x[head], w[head])), digits),
    staff_att_weighted = fmt_mean_sd(wmean(x[staff], w[staff]), sqrt(wvar(x[staff], w[staff])), digits),
    smd_after_att = smd_cont(x, treat, w),
    row_type = "continuous",
    stringsAsFactors = FALSE
  )
}

baseline_factor_rows <- function(input_dat, var, label) {
  x <- as.character(input_dat[[var]])
  x[is.na(x)] <- "Missing"
  levs <- sort_levels_by_code(unique(x))
  if (length(levs) <= 1 && levs[[1]] == "Missing") return(NULL)
  treat <- input_dat$head_nurse
  w <- input_dat$att_weight
  head <- treat == 1
  staff <- treat == 0
  do.call(rbind, lapply(levs, function(lv) {
    ind <- as.numeric(x == lv)
    data.frame(
      characteristic = label,
      level = clean_level_label(var, lv),
      head_unweighted = fmt_n_pct(sum(ind[head] == 1, na.rm = TRUE), sum(head, na.rm = TRUE)),
      staff_unweighted = fmt_n_pct(sum(ind[staff] == 1, na.rm = TRUE), sum(staff, na.rm = TRUE)),
      smd_before = smd_cont(ind, treat),
      head_att_weighted = fmt_pct(wmean(ind[head], w[head])),
      staff_att_weighted = fmt_pct(wmean(ind[staff], w[staff])),
      smd_after_att = smd_cont(ind, treat, w),
      row_type = "categorical",
      stringsAsFactors = FALSE
    )
  }))
}

baseline_binary_row <- function(input_dat, var, label, level_label = "Yes") {
  x <- input_dat[[var]]
  treat <- input_dat$head_nurse
  w <- input_dat$att_weight
  head <- treat == 1
  staff <- treat == 0
  data.frame(
    characteristic = label,
    level = level_label,
    head_unweighted = fmt_n_pct(sum(x[head] == 1, na.rm = TRUE), sum(!is.na(x[head]))),
    staff_unweighted = fmt_n_pct(sum(x[staff] == 1, na.rm = TRUE), sum(!is.na(x[staff]))),
    smd_before = smd_cont(x, treat),
    head_att_weighted = fmt_pct(wmean(x[head], w[head])),
    staff_att_weighted = fmt_pct(wmean(x[staff], w[staff])),
    smd_after_att = smd_cont(x, treat, w),
    row_type = "binary",
    stringsAsFactors = FALSE
  )
}

make_baseline_table1 <- function(input_dat) {
  rows <- list(
    baseline_cont_row(input_dat, "age", "Age, years", 1),
    baseline_cont_row(input_dat, "work_years", "Work tenure, years", 1),
    baseline_cont_row(input_dat, "bmi", "BMI, kg/m2", 1),
    baseline_factor_rows(input_dat, "sex", "Sex"),
    baseline_factor_rows(input_dat, "first_education", "First education"),
    baseline_factor_rows(input_dat, "highest_education", "Highest education"),
    baseline_factor_rows(input_dat, "marital_status", "Marital status"),
    baseline_factor_rows(input_dat, "prior_hospital_count", "Number of prior medical institutions"),
    baseline_factor_rows(input_dat, "department", "Department"),
    baseline_factor_rows(input_dat, "employment_type", "Employment type"),
    baseline_factor_rows(input_dat, "professional_title", "Professional title"),
    baseline_factor_rows(input_dat, "sunlight_time", "Sunlight exposure"),
    baseline_factor_rows(input_dat, "smoking", "Smoking"),
    baseline_factor_rows(input_dat, "drinking", "Drinking"),
    baseline_factor_rows(input_dat, "pregnant", "Pregnancy status"),
    baseline_factor_rows(input_dat, "shift_pattern", "Work schedule in past 6 months"),
    baseline_binary_row(input_dat, "day_only", "Day-shift-only work", "Yes")
  )
  rows <- rows[!vapply(rows, is.null, logical(1))]
  out <- do.call(rbind, rows)
  out$smd_before <- round(out$smd_before, 3)
  out$smd_after_att <- round(out$smd_after_att, 3)
  out
}

test_cont <- function(input_dat, var) {
  dd <- input_dat[!is.na(input_dat[[var]]) & !is.na(input_dat$head_nurse), , drop = FALSE]
  if (nrow(dd) < 3 || length(unique(dd$head_nurse)) < 2) return(c(stat = NA_real_, p = NA_real_))
  tt <- tryCatch(stats::t.test(dd[[var]] ~ dd$head_nurse), error = function(e) NULL)
  if (is.null(tt)) return(c(stat = NA_real_, p = NA_real_))
  c(stat = abs(unname(tt$statistic)), p = tt$p.value)
}

test_factor <- function(input_dat, var) {
  x <- as.character(input_dat[[var]])
  x[is.na(x)] <- "Missing"
  if (length(unique(x)) <= 1) return(c(stat = NA_real_, p = NA_real_))
  tab <- table(x, input_dat$head_nurse)
  ct <- tryCatch(suppressWarnings(stats::chisq.test(tab, correct = FALSE)), error = function(e) NULL)
  if (is.null(ct)) return(c(stat = NA_real_, p = NA_real_))
  c(stat = unname(ct$statistic), p = ct$p.value)
}

baseline_journal_cont_row <- function(input_dat, var, label, digits = 1) {
  x <- input_dat[[var]]
  treat <- input_dat$head_nurse
  w <- input_dat$att_weight
  head <- treat == 1
  staff <- treat == 0
  tst <- test_cont(input_dat, var)
  data.frame(
    row_label = label,
    head_nurse = fmt_mean_sd(mean(x[head], na.rm = TRUE), sd(x[head], na.rm = TRUE), digits),
    staff_nurse = fmt_mean_sd(mean(x[staff], na.rm = TRUE), sd(x[staff], na.rm = TRUE), digits),
    test_statistic = ifelse(is.finite(tst[["stat"]]), sprintf("t=%.3f", tst[["stat"]]), ""),
    p_value = fmt_p(tst[["p"]]),
    smd_before = sprintf("%.3f", smd_cont(x, treat)),
    smd_after_att = sprintf("%.3f", smd_cont(x, treat, w)),
    row_type = "continuous",
    stringsAsFactors = FALSE
  )
}

baseline_journal_factor_rows <- function(input_dat, var, label) {
  x <- as.character(input_dat[[var]])
  x[is.na(x)] <- "Missing"
  levs <- sort_levels_by_code(unique(x))
  if (length(levs) <= 1 && levs[[1]] == "Missing") return(NULL)
  treat <- input_dat$head_nurse
  w <- input_dat$att_weight
  head <- treat == 1
  staff <- treat == 0
  tst <- test_factor(input_dat, var)
  group <- data.frame(
    row_label = label,
    head_nurse = "",
    staff_nurse = "",
    test_statistic = ifelse(is.finite(tst[["stat"]]), sprintf("X2=%.3f", tst[["stat"]]), ""),
    p_value = fmt_p(tst[["p"]]),
    smd_before = sprintf("%.3f", smd_factor_max(x, treat)),
    smd_after_att = sprintf("%.3f", smd_factor_max(x, treat, w)),
    row_type = "group",
    stringsAsFactors = FALSE
  )
  levels <- do.call(rbind, lapply(levs, function(lv) {
    ind <- as.numeric(x == lv)
    data.frame(
      row_label = paste0("  ", clean_level_label(var, lv)),
      head_nurse = fmt_n_pct(sum(ind[head] == 1, na.rm = TRUE), sum(head, na.rm = TRUE)),
      staff_nurse = fmt_n_pct(sum(ind[staff] == 1, na.rm = TRUE), sum(staff, na.rm = TRUE)),
      test_statistic = "",
      p_value = "",
      smd_before = "",
      smd_after_att = "",
      row_type = "level",
      stringsAsFactors = FALSE
    )
  }))
  rbind(group, levels)
}

baseline_journal_binary_rows <- function(input_dat, var, label, level_label = "Yes") {
  x <- input_dat[[var]]
  treat <- input_dat$head_nurse
  w <- input_dat$att_weight
  head <- treat == 1
  staff <- treat == 0
  tmp_dat <- data.frame(tmp = factor(ifelse(x == 1, "Yes", "No")), head_nurse = treat)
  tst <- test_factor(tmp_dat, "tmp")
  rbind(
    data.frame(
      row_label = label,
      head_nurse = "",
      staff_nurse = "",
      test_statistic = ifelse(is.finite(tst[["stat"]]), sprintf("X2=%.3f", tst[["stat"]]), ""),
      p_value = fmt_p(tst[["p"]]),
      smd_before = sprintf("%.3f", abs(smd_cont(x, treat))),
      smd_after_att = sprintf("%.3f", abs(smd_cont(x, treat, w))),
      row_type = "group",
      stringsAsFactors = FALSE
    ),
    data.frame(
      row_label = paste0("  ", level_label),
      head_nurse = fmt_n_pct(sum(x[head] == 1, na.rm = TRUE), sum(!is.na(x[head]))),
      staff_nurse = fmt_n_pct(sum(x[staff] == 1, na.rm = TRUE), sum(!is.na(x[staff]))),
      test_statistic = "",
      p_value = "",
      smd_before = "",
      smd_after_att = "",
      row_type = "level",
      stringsAsFactors = FALSE
    )
  )
}

make_baseline_journal_table1 <- function(input_dat) {
  rows <- list(
    baseline_journal_cont_row(input_dat, "age", "Age, years", 1),
    baseline_journal_cont_row(input_dat, "work_years", "Work tenure, years", 1),
    baseline_journal_cont_row(input_dat, "bmi", "BMI, kg/m2", 1),
    baseline_journal_factor_rows(input_dat, "sex", "Sex"),
    baseline_journal_factor_rows(input_dat, "first_education", "First education"),
    baseline_journal_factor_rows(input_dat, "highest_education", "Highest education"),
    baseline_journal_factor_rows(input_dat, "marital_status", "Marital status"),
    baseline_journal_factor_rows(input_dat, "prior_hospital_count", "Number of prior medical institutions"),
    baseline_journal_factor_rows(input_dat, "department", "Department"),
    baseline_journal_factor_rows(input_dat, "employment_type", "Employment type"),
    baseline_journal_factor_rows(input_dat, "professional_title", "Professional title"),
    baseline_journal_factor_rows(input_dat, "sunlight_time", "Sunlight exposure"),
    baseline_journal_factor_rows(input_dat, "smoking", "Smoking"),
    baseline_journal_factor_rows(input_dat, "drinking", "Drinking"),
    baseline_journal_factor_rows(input_dat, "pregnant", "Pregnancy status"),
    baseline_journal_factor_rows(input_dat, "shift_pattern", "Work schedule in past 6 months"),
    baseline_journal_binary_rows(input_dat, "day_only", "Day-shift-only work", "Yes")
  )
  rows <- rows[!vapply(rows, is.null, logical(1))]
  do.call(rbind, rows)
}

outcome_specs <- data.frame(
  outcome = c(
    "psqi", "poor_sleep", "pss", "mbi_ee", "mbi_dp", "mbi_pa", "gad7",
    "moderate_anxiety", "phq9", "moderate_depression", "turnover_intention",
    "sps6", "work_family_balance", "ess",
    "circadian_disruption_domain", "management_strain_domain"
  ),
  label = c(
    "PSQI total score", "Poor sleep quality (PSQI >= 8)", "PSS perceived stress",
    "MBI emotional exhaustion", "MBI depersonalization", "MBI personal accomplishment",
    "GAD-7 anxiety score", "Moderate anxiety (GAD-7 >= 10)",
    "PHQ-9 depression score", "Moderate depression (PHQ-9 >= 10)",
    "Turnover-intention score", "SPS-6 productivity loss",
    "Work-family balance score", "ESS daytime sleepiness",
    "Circadian-disruption risk domain", "Managerial-strain risk domain"
  ),
  type = c(
    "continuous", "binary", "continuous", "continuous", "continuous", "continuous",
    "continuous", "binary", "continuous", "binary", "continuous", "continuous",
    "continuous", "continuous", "continuous", "continuous"
  ),
  primary = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

fit_weighted_outcome <- function(input_dat, outcome, outcome_label, type, model_label,
                                 adjust_night = FALSE, extra_covariates = NULL) {
  keep <- !is.na(input_dat[[outcome]]) & !is.na(input_dat$head_nurse) &
    !is.na(input_dat$att_weight) & input_dat$att_weight > 0
  if (adjust_night) keep <- keep & !is.na(input_dat$night_shift)
  if (!is.null(extra_covariates) && length(extra_covariates) > 0) {
    for (v in extra_covariates) keep <- keep & !is.na(input_dat[[v]])
  }
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

  rhs_terms <- c("head_nurse")
  if (adjust_night) rhs_terms <- c(rhs_terms, "night_shift")
  if (!is.null(extra_covariates) && length(extra_covariates) > 0) {
    rhs_terms <- c(rhs_terms, extra_covariates)
  }
  fm <- stats::as.formula(paste(outcome, "~", paste(rhs_terms, collapse = " + ")))
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
baseline_table1 <- make_baseline_table1(analysis_dat)
baseline_table1_journal <- make_baseline_journal_table1(analysis_dat)

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

dr_results <- do.call(rbind, lapply(seq_len(nrow(outcome_specs)), function(i) {
  fit_weighted_outcome(
    analysis_dat,
    outcome_specs$outcome[[i]],
    outcome_specs$label[[i]],
    outcome_specs$type[[i]],
    model_label = "Sensitivity 1: ATT weighted plus baseline covariates",
    adjust_night = FALSE,
    extra_covariates = usable_covariates
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
  dr_day_results <- do.call(rbind, lapply(seq_len(nrow(outcome_specs)), function(i) {
    fit_weighted_outcome(
      day_dat,
      outcome_specs$outcome[[i]],
      outcome_specs$label[[i]],
      outcome_specs$type[[i]],
      model_label = "Sensitivity 2: day-shift-only ATT weighted plus baseline covariates",
      adjust_night = FALSE,
      extra_covariates = usable_covariates
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
  dr_day_results <- day_results
  dr_day_results$model <- "Sensitivity 2: day-shift-only ATT weighted plus baseline covariates"
}

all_results <- bind_rows(model1_results, model2_results, day_results, dr_results, dr_day_results) %>%
  group_by(model) %>%
  mutate(
    p_fdr = p.adjust(p_value, method = "BH"),
    primary = outcome %in% outcome_specs$outcome[outcome_specs$primary]
  ) %>%
  ungroup()

get_model_row <- function(outcome, model_name) {
  rows <- all_results[all_results$outcome == outcome & all_results$model == model_name, , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)
  rows[1, , drop = FALSE]
}

risk_transition_summary <- do.call(rbind, lapply(
  c("psqi", "poor_sleep", "pss", "mbi_ee", "gad7", "turnover_intention",
    "work_family_balance", "circadian_disruption_domain", "management_strain_domain"),
  function(outcome) {
    m1 <- get_model_row(outcome, "Model 1: ATT weighted, no night-shift adjustment")
    m2 <- get_model_row(outcome, "Model 2: ATT weighted, plus night-shift adjustment")
    m3 <- get_model_row(outcome, "Model 3: day-shift-only ATT weighted")
    if (is.null(m1) || is.null(m2) || is.null(m3)) return(NULL)
    change_m2 <- ifelse(m1$type == "binary", log(m2$estimate) - log(m1$estimate), m2$estimate - m1$estimate)
    change_m3 <- ifelse(m1$type == "binary", log(m3$estimate) - log(m1$estimate), m3$estimate - m1$estimate)
    interp <- "Role association changes after accounting for night-shift structure"
    if (outcome %in% c("psqi", "poor_sleep", "circadian_disruption_domain")) {
      interp <- "Apparent sleep/circadian advantage attenuates after night-shift accounting"
    }
    if (outcome %in% c("pss", "mbi_ee", "gad7", "turnover_intention", "management_strain_domain")) {
      interp <- "Managerial-strain signal emerges or strengthens after night-shift accounting"
    }
    if (outcome == "work_family_balance") {
      interp <- "Work-family advantage reverses toward poorer balance after night-shift accounting"
    }
    data.frame(
      outcome = outcome,
      label = m1$label,
      type = m1$type,
      model1_estimate = m1$estimate,
      model1_p = m1$p_value,
      model2_estimate = m2$estimate,
      model2_p = m2$p_value,
      model3_estimate = m3$estimate,
      model3_p = m3$p_value,
      model2_minus_model1 = change_m2,
      model3_minus_model1 = change_m3,
      interpretation = interp,
      stringsAsFactors = FALSE
    )
  }
))

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

propensity_diagnostics <- analysis_dat %>%
  transmute(
    sample = "Full sample",
    group = ifelse(head_nurse == 1, "Head nurse", "Staff nurse"),
    head_nurse = head_nurse,
    propensity_score = ps,
    att_weight = att_weight,
    day_only = day_only,
    night_shift = night_shift
  )

if (nrow(day_dat) > 0 && all(c("ps", "att_weight") %in% names(day_dat))) {
  propensity_diagnostics <- bind_rows(
    propensity_diagnostics,
    day_dat %>%
      transmute(
        sample = "Day-shift-only sample",
        group = ifelse(head_nurse == 1, "Head nurse", "Staff nurse"),
        head_nurse = head_nurse,
        propensity_score = ps,
        att_weight = att_weight,
        day_only = day_only,
        night_shift = night_shift
      )
  )
}

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
  "9. Candidate propensity-score covariates are baseline or relatively stable variables: age, work tenure, BMI, sex, education, marital status, prior hospital count when available, department, employment type, professional title, sunlight, smoking, drinking, and pregnancy status. Variables with no observed variation or no usable information are omitted automatically.",
  "10. Night shift C_q1 is not included in the propensity-score model because it may be part of the head-nurse role pathway. It is handled through the three-model strategy: unadjusted for night shift, adjusted for night shift in the outcome model, and restricted to day-shift-only nurses.",
  "11. Missing baseline covariates are retained using median imputation plus missing indicators for numeric variables and an explicit Missing category for categorical variables.",
  "12. The main estimand is ATT, implemented as propensity-score weighting: head nurses weight=1, staff nurses weight=PS/(1-PS), with the 99th percentile used to cap extreme weights.",
  "13. For IJNS-facing interpretation, two exploratory standardized risk domains are created: a circadian-disruption domain from PSQI, ESS, and SPS-6; and a managerial-strain domain from PSS, MBI-EE, GAD-7, turnover intention, and reversed WFB.",
  "14. Sensitivity analyses fit weighted outcome models with additional baseline covariate adjustment after ATT weighting, providing a doubly robust style check of the main associations."
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
data.table::fwrite(baseline_table1, file.path(out_dir, "03b_baseline_table1_att.csv"))
data.table::fwrite(baseline_table1_journal, file.path(out_dir, "03c_baseline_table1_journal.csv"))
data.table::fwrite(ps_summary, file.path(out_dir, "04_propensity_summary.csv"))
data.table::fwrite(balance_m1, file.path(out_dir, "05_balance_model1_att.csv"))
if (nrow(balance_m3) > 0) data.table::fwrite(balance_m3, file.path(out_dir, "06_balance_model3_day_only_att.csv"))
data.table::fwrite(all_results, file.path(out_dir, "07_outcome_models_att.csv"))
data.table::fwrite(risk_transition_summary, file.path(out_dir, "08_risk_transition_summary.csv"))
data.table::fwrite(propensity_diagnostics, file.path(out_dir, "09_propensity_diagnostics.csv"))
writeLines(cleaning_rules_md, file.path(out_dir, "00_cleaning_rules.md"), useBytes = TRUE)
writeLines(summary_md, file.path(out_dir, "analysis_summary.md"), useBytes = TRUE)
writeLines(log_lines, file.path(out_dir, "analysis_run_log.txt"), useBytes = TRUE)

make_love_plot <- function(balance_df, title, out_file) {
  if (is.null(balance_df) || nrow(balance_df) == 0) return(invisible(NULL))
  b <- balance_df %>%
    mutate(
      covariate_label = gsub("_", " ", covariate),
      covariate_label = gsub(" imp$", "", covariate_label),
      covariate_label = gsub(" missing$", " missing", covariate_label),
      max_abs = pmax(abs_smd_before, abs_smd_after_att_weighting, na.rm = TRUE)
    ) %>%
    arrange(max_abs)
  levels_cov <- b$covariate_label
  love_dat <- bind_rows(
    b %>% transmute(covariate_label, status = "Before weighting", abs_smd = abs_smd_before),
    b %>% transmute(covariate_label, status = "After ATT weighting", abs_smd = abs_smd_after_att_weighting)
  ) %>%
    mutate(covariate_label = factor(covariate_label, levels = levels_cov))
  max_x <- max(love_dat$abs_smd, na.rm = TRUE)
  p_love <- ggplot(love_dat, aes(x = abs_smd, y = covariate_label, color = status, shape = status)) +
    geom_vline(xintercept = 0.10, linetype = "dashed", color = "grey55") +
    geom_point(size = 2.2, alpha = 0.9) +
    scale_x_continuous(limits = c(0, max(0.2, max_x * 1.08)), expand = expansion(mult = c(0, 0.02))) +
    labs(title = title, x = "Absolute standardized mean difference", y = NULL, color = NULL, shape = NULL) +
    theme_minimal(base_size = 10.5) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold")
    )
  ggsave(file.path(out_dir, out_file), p_love, width = 7.2, height = 6.2, dpi = 300)
}

make_love_plot(balance_m1, "Covariate balance before and after ATT weighting: full sample", "figure_love_plot_model1.png")
if (nrow(balance_m3) > 0) {
  make_love_plot(balance_m3, "Covariate balance before and after ATT weighting: day-shift-only sample", "figure_love_plot_model3_day_only.png")
}

ps_plot_dat <- analysis_dat %>%
  filter(!is.na(ps), !is.na(att_weight)) %>%
  transmute(
    group = ifelse(head_nurse == 1, "Head nurse", "Staff nurse"),
    propensity_score = ps,
    att_weight = att_weight
  )
if (nrow(ps_plot_dat) > 0) {
  weight_cap <- quantile(ps_plot_dat$att_weight, probs = 0.99, na.rm = TRUE)
  ps_long <- bind_rows(
    ps_plot_dat %>% transmute(group, panel = "Propensity score overlap", value = propensity_score),
    ps_plot_dat %>% transmute(group, panel = "ATT weight distribution", value = pmin(att_weight, weight_cap))
  )
  p_ps <- ggplot(ps_long, aes(x = value, color = group, fill = group)) +
    geom_density(alpha = 0.20, linewidth = 0.8, adjust = 1.05) +
    facet_wrap(~panel, scales = "free_x", ncol = 1) +
    labs(x = NULL, y = "Density", color = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  ggsave(file.path(out_dir, "figure_propensity_overlap_weights.png"), p_ps, width = 7.2, height = 6.2, dpi = 300)
}

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

domain_plot <- all_results %>%
  filter(model %in% c(
    "Model 1: ATT weighted, no night-shift adjustment",
    "Model 2: ATT weighted, plus night-shift adjustment",
    "Model 3: day-shift-only ATT weighted"
  )) %>%
  filter(outcome %in% c("circadian_disruption_domain", "management_strain_domain")) %>%
  filter(!is.na(estimate))

if (nrow(domain_plot) > 0) {
  p_domain <- ggplot(domain_plot, aes(x = estimate, y = label, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high), position = position_dodge(width = 0.45)) +
    labs(x = "Head nurse minus staff nurse standardized domain difference", y = NULL, color = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(out_dir, "figure_risk_domain_transition.png"), p_domain, width = 8.5, height = 3.8, dpi = 300)
}

wrap_box_label <- function(x, width = 25) {
  paste(strwrap(x, width = width), collapse = "\n")
}

make_conceptual_figure <- function(out_file) {
  nodes <- data.frame(
    id = c("role", "schedule", "management", "circadian", "strain", "strategy"),
    x = c(0.7, 2.9, 2.9, 5.1, 5.1, 3.9),
    y = c(0, 1.15, -1.15, 1.15, -1.15, -2.35),
    w = c(1.45, 1.75, 1.85, 1.95, 1.95, 2.45),
    h = c(0.70, 0.82, 0.82, 0.92, 0.92, 0.70),
    fill = c("#E8F2FF", "#E9F7EF", "#FFF3E0", "#DDEBFF", "#FFE6D2", "#F1F5F9"),
    border = c("#2F6FB0", "#2E7D55", "#B66A00", "#2F6FB0", "#B66A00", "#475569"),
    label = c(
      "Head-nurse role transition",
      "Reduced night-shift exposure",
      "Increased managerial responsibility",
      "Lower circadian-disruption signal: PSQI, ESS, SPS-6",
      "Higher managerial-strain signal: MBI-EE, GAD-7, turnover intention, WFB",
      "Three-model strategy separates schedule advantage from role-related strain"
    ),
    stringsAsFactors = FALSE
  )

  p_fig <- ggplot() +
    annotate("segment", x = 1.45, y = 0.10, xend = 2.05, yend = 1.05,
             arrow = grid::arrow(length = grid::unit(0.13, "inches")), linewidth = 0.6, color = "#475569") +
    annotate("segment", x = 1.45, y = -0.10, xend = 2.05, yend = -1.05,
             arrow = grid::arrow(length = grid::unit(0.13, "inches")), linewidth = 0.6, color = "#475569") +
    annotate("segment", x = 3.80, y = 1.15, xend = 4.05, yend = 1.15,
             arrow = grid::arrow(length = grid::unit(0.13, "inches")), linewidth = 0.6, color = "#2F6FB0") +
    annotate("segment", x = 3.85, y = -1.15, xend = 4.05, yend = -1.15,
             arrow = grid::arrow(length = grid::unit(0.13, "inches")), linewidth = 0.6, color = "#B66A00") +
    annotate("segment", x = 5.00, y = 0.65, xend = 4.35, yend = -2.00,
             arrow = grid::arrow(length = grid::unit(0.12, "inches")), linewidth = 0.45, color = "#64748B") +
    annotate("segment", x = 5.00, y = -1.65, xend = 4.35, yend = -2.05,
             arrow = grid::arrow(length = grid::unit(0.12, "inches")), linewidth = 0.45, color = "#64748B") +
    coord_cartesian(xlim = c(-0.2, 6.35), ylim = c(-2.85, 1.85), clip = "off") +
    theme_void()

  for (i in seq_len(nrow(nodes))) {
    p_fig <- p_fig +
      annotate("rect",
               xmin = nodes$x[[i]] - nodes$w[[i]] / 2,
               xmax = nodes$x[[i]] + nodes$w[[i]] / 2,
               ymin = nodes$y[[i]] - nodes$h[[i]] / 2,
               ymax = nodes$y[[i]] + nodes$h[[i]] / 2,
               fill = nodes$fill[[i]],
               color = nodes$border[[i]],
               linewidth = 0.75) +
      annotate("text",
               x = nodes$x[[i]],
               y = nodes$y[[i]],
               label = wrap_box_label(nodes$label[[i]], width = ifelse(nodes$id[[i]] == "strategy", 34, 24)),
               size = 3.2,
               lineheight = 0.92,
               color = "#111827")
  }

  p_fig <- p_fig +
    annotate("text", x = 3.0, y = 1.70, label = "Schedule pathway", size = 3.3, fontface = "bold", color = "#2E7D55") +
    annotate("text", x = 3.0, y = -1.72, label = "Managerial pathway", size = 3.3, fontface = "bold", color = "#B66A00")

  ggsave(out_file, p_fig, width = 8.5, height = 4.6, dpi = 300, bg = "white")
}

conceptual_figure_path <- file.path(out_dir, "figure_conceptual_risk_transition.png")
make_conceptual_figure(conceptual_figure_path)

table_figure_plan <- data.frame(
  item = c(
    "Table 1. Baseline characteristics of head nurses and staff nurses",
    "Table 2. Weighted associations with key occupational-health outcomes",
    "Table 3. Risk-transition summary across the three models",
    "Figure 1. Conceptual risk-transition framework",
    "Figure 2. Risk-domain transition across models",
    "Supplementary Table S1. Cleaning flow",
    "Supplementary Table S2. Score range and missingness audit",
    "Supplementary Table S3. Detailed baseline and balance diagnostics",
    "Supplementary Table S4. Propensity-score summary and covariate balance",
    "Supplementary Table S5. Full weighted outcome models and sensitivity analyses",
    "Supplementary Figure S1. Full-sample Love plot",
    "Supplementary Figure S2. Propensity-score overlap and ATT weight distribution",
    "Supplementary Figure S3. Day-shift-only Love plot",
    "Supplementary Figure S4. Full continuous-outcome forest plot"
  ),
  placement = c(
    "Main text", "Main text", "Main text", "Main text", "Main text",
    "Supplement", "Supplement", "Supplement", "Supplement", "Supplement",
    "Supplement", "Supplement", "Supplement", "Supplement"
  ),
  source_file = c(
    "03c_baseline_table1_journal.csv",
    "07_outcome_models_att.csv",
    "08_risk_transition_summary.csv",
    "figure_conceptual_risk_transition.png",
    "figure_risk_domain_transition.png",
    "01_cleaning_flow.csv",
    "02_score_range_summary.csv",
    "03b_baseline_table1_att.csv",
    "04_propensity_summary.csv; 05_balance_model1_att.csv; 06_balance_model3_day_only_att.csv",
    "07_outcome_models_att.csv",
    "figure_love_plot_model1.png",
    "figure_propensity_overlap_weights.png",
    "figure_love_plot_model3_day_only.png",
    "figure_continuous_outcome_differences.png"
  ),
  rationale = c(
    "Establishes comparability and shows ATT balance; expected as Table 1 in nursing cohort papers.",
    "Presents the core estimands for the primary manuscript argument.",
    "Condenses the role-risk transition from circadian disruption to managerial strain.",
    "Makes the novelty and three-model logic visible to reviewers.",
    "Shows the profile-level risk-transition pattern more clearly than text alone.",
    "Documents reproducible exclusions without crowding the Results section.",
    "Shows score-validity checks and outcome-specific missingness.",
    "Keeps the full balance table available while the main text remains readable.",
    "Supports propensity-score transparency and overlap assessment.",
    "Preserves every modeled outcome, including sensitivity analyses and FDR values.",
    "Provides standard visual balance diagnostics for the full sample.",
    "Shows common support and whether ATT weights are dominated by extremes.",
    "Documents balance for the day-shift-only sensitivity analysis.",
    "Allows readers to inspect all continuous outcomes beyond the key table."
  ),
  stringsAsFactors = FALSE
)
data.table::fwrite(table_figure_plan, file.path(out_dir, "10_table_figure_placement_plan.csv"))

docx_fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), as.numeric(x)))
}

docx_fmt_p <- function(x) {
  ifelse(is.na(x), "", ifelse(as.numeric(x) < 0.001, "<0.001", sprintf("%.3f", as.numeric(x))))
}

docx_model_short <- c(
  "Model 1: ATT weighted, no night-shift adjustment" = "M1: total role association",
  "Model 2: ATT weighted, plus night-shift adjustment" = "M2: plus night shift",
  "Model 3: day-shift-only ATT weighted" = "M3: day-shift only",
  "Sensitivity 1: ATT weighted plus baseline covariates" = "S1: ATT plus baseline covariates",
  "Sensitivity 2: day-shift-only ATT weighted plus baseline covariates" = "S2: day shift plus baseline covariates"
)

docx_outcome_short <- c(
  psqi = "PSQI total",
  poor_sleep = "Poor sleep",
  pss = "PSS",
  mbi_ee = "MBI-EE",
  mbi_dp = "MBI-DP",
  mbi_pa = "MBI-PA",
  gad7 = "GAD-7",
  moderate_anxiety = "Moderate anxiety",
  phq9 = "PHQ-9",
  moderate_depression = "Moderate depression",
  turnover_intention = "Turnover intention",
  sps6 = "SPS-6",
  work_family_balance = "WFB",
  ess = "ESS",
  circadian_disruption_domain = "Circadian-disruption domain",
  management_strain_domain = "Managerial-strain domain"
)

docx_effect <- function(type, estimate) {
  ifelse(type == "binary", paste0("PR ", docx_fmt_num(estimate, 2)), paste0("MD ", docx_fmt_num(estimate, 2)))
}

docx_make_ft <- function(dat, font_size = 8, first_col_left = TRUE) {
  dat <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
  dat[] <- lapply(dat, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  ft <- flextable::flextable(dat)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::fontsize(ft, size = font_size + 0.5, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, align = "center", part = "body")
  if (first_col_left) ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft <- flextable::padding(ft, padding = 3, part = "all")
  ft <- flextable::set_table_properties(ft, layout = "autofit")
  flextable::autofit(ft)
}

docx_add_table <- function(doc, dat, caption, note = NULL, font_size = 8, first_col_left = TRUE) {
  doc <- officer::body_add_par(doc, caption, style = "Normal")
  ft <- docx_make_ft(dat, font_size = font_size, first_col_left = first_col_left)
  doc <- flextable::body_add_flextable(doc, value = ft)
  if (!is.null(note) && nzchar(note)) {
    doc <- officer::body_add_par(doc, paste0("Note. ", note), style = "Normal")
  }
  officer::body_add_par(doc, "", style = "Normal")
}

docx_add_image <- function(doc, image_path, caption, width = 6.5, height = 3.8) {
  if (!file.exists(image_path)) return(doc)
  doc <- officer::body_add_par(doc, caption, style = "Normal")
  doc <- officer::body_add_img(doc, src = image_path, width = width, height = height)
  officer::body_add_par(doc, "", style = "Normal")
}

build_key_results_table <- function() {
  keep_outcomes <- c("psqi", "poor_sleep", "pss", "mbi_ee", "gad7", "phq9", "turnover_intention", "work_family_balance")
  keep_models <- c(
    "Model 1: ATT weighted, no night-shift adjustment",
    "Model 2: ATT weighted, plus night-shift adjustment",
    "Model 3: day-shift-only ATT weighted"
  )
  all_results %>%
    filter(outcome %in% keep_outcomes, model %in% keep_models) %>%
    mutate(
      Outcome = unname(docx_outcome_short[outcome]),
      Model = unname(docx_model_short[model]),
      Effect = docx_effect(type, estimate),
      `95% CI` = paste0(docx_fmt_num(conf_low, 2), " to ", docx_fmt_num(conf_high, 2)),
      P = docx_fmt_p(p_value)
    ) %>%
    select(Outcome, Model, Effect, `95% CI`, P)
}

build_transition_table <- function() {
  keep <- c(
    "circadian_disruption_domain", "management_strain_domain", "psqi",
    "mbi_ee", "gad7", "turnover_intention", "work_family_balance"
  )
  signal <- c(
    "Apparent sleep/circadian advantage attenuates after night-shift accounting" = "Sleep/circadian advantage attenuates",
    "Managerial-strain signal emerges or strengthens after night-shift accounting" = "Managerial-strain signal emerges",
    "Work-family advantage reverses toward poorer balance after night-shift accounting" = "Work-family advantage reverses"
  )
  risk_transition_summary %>%
    filter(outcome %in% keep) %>%
    mutate(
      Outcome = unname(docx_outcome_short[outcome]),
      M1 = docx_fmt_num(model1_estimate, 2),
      M2 = docx_fmt_num(model2_estimate, 2),
      M3 = docx_fmt_num(model3_estimate, 2),
      Signal = ifelse(interpretation %in% names(signal), unname(signal[interpretation]), interpretation)
    ) %>%
    select(Outcome, M1, M2, M3, Signal)
}

build_main_docx <- function() {
  main_docx <- file.path(out_dir, "head_nurse_propensity_health_IJNS_trial_draft_R.docx")
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "From Circadian Disruption to Managerial Strain", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "Occupational-Health Risk Transition Among Head Nurses and Staff Nurses: A Large Propensity-Weighted Cross-Sectional Survey",
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    "Trial note: this draft is generated entirely in R from the local trial dataset and should be replaced with the final bastion/RStudio Server results before submission.",
    style = "Normal"
  )

  doc <- officer::body_add_par(doc, "Abstract", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "Background: Moving from staff nurse to head nurse commonly reduces night-shift exposure but increases managerial responsibility. This study examined whether head-nurse status represents broad occupational-health protection or a transition from circadian disruption to managerial strain.",
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    "Methods: Head nurses (A_q12=3) were compared with staff nurses without administrative roles (A_q12=1). The average treatment effect on the treated was estimated with propensity-score weighting. Night shift was handled through three models: total role association, adjustment for night shift in the outcome model, and restriction to day-shift-only nurses. Additional weighted baseline-adjusted models were used as sensitivity analyses.",
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    sprintf(
      "Results: The cleaned trial sample included %s nurses, including %s head nurses and %s staff nurses. Day-shift-only work was much more common among head nurses than staff nurses (%.1f%% vs %.1f%%). Weighting reduced the maximum absolute SMD from %.3f to %.3f.",
      format(nrow(analysis_dat), big.mark = ","),
      format(sum(analysis_dat$head_nurse == 1), big.mark = ","),
      format(sum(analysis_dat$head_nurse == 0), big.mark = ","),
      100 * mean(analysis_dat$day_only[analysis_dat$head_nurse == 1] == 1, na.rm = TRUE),
      100 * mean(analysis_dat$day_only[analysis_dat$head_nurse == 0] == 1, na.rm = TRUE),
      max(balance_m1$abs_smd_before, na.rm = TRUE),
      max(balance_m1$abs_smd_after_att_weighting, na.rm = TRUE)
    ),
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    "Conclusions: Reduced night-shift exposure should not be interpreted as broad protection. The trial findings support a risk-transition framing in which head nurses show attenuated circadian-disruption advantage after night-shift accounting but clearer managerial-strain signals, especially emotional exhaustion, anxiety, and turnover intention.",
    style = "Normal"
  )

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Introduction", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "Nursing work combines high emotional demands, time pressure, and irregular schedules. Head nurses often leave night-shift rotations as they enter managerial roles, but they also absorb administrative workload, accountability, conflict management, and boundary pressure. A simple healthier-versus-less-healthy comparison may therefore miss the central question: whether the dominant occupational-health risk changes in form.",
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    "This manuscript frames head-nurse status as a role-related risk transition. The circadian-disruption side is represented by sleep quality, daytime sleepiness, and productivity loss; the managerial-strain side is represented by stress, emotional exhaustion, anxiety, turnover intention, and poorer work-family balance.",
    style = "Normal"
  )
  doc <- docx_add_image(
    doc,
    conceptual_figure_path,
    "Figure 1. Conceptual risk-transition framework for head-nurse occupational health.",
    width = 6.7,
    height = 3.6
  )

  doc <- officer::body_add_par(doc, "Methods", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "The primary exposure contrasted head nurses with staff nurses who had no administrative role. Records with other administrative positions, implausible age or tenure values, tenure inconsistent with age, duplicate participant IDs, or no usable main outcome after score-range checks were excluded. Candidate propensity-score covariates were baseline or relatively stable demographic and work-related characteristics. Work schedule was not included in the propensity-score model because night shift was treated as a role-related pathway variable.",
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    "Continuous outcomes were modeled with weighted linear regression and binary outcomes with weighted log-link quasi-Poisson regression. Effects are presented as mean differences or prevalence ratios with 95% confidence intervals.",
    style = "Normal"
  )

  doc <- officer::body_add_par(doc, "Results", style = "heading 1")
  doc <- officer::body_add_par(doc, "Baseline Characteristics and Balance", style = "heading 2")
  baseline_docx <- baseline_table1_journal %>%
    transmute(
      Characteristic = row_label,
      `Head nurse` = head_nurse,
      `Staff nurse` = staff_nurse,
      `Test statistic` = test_statistic,
      P = p_value,
      `SMD before` = smd_before,
      `SMD after ATT` = smd_after_att
    )
  doc <- docx_add_table(
    doc,
    baseline_docx,
    "Table 1. Baseline characteristics of head nurses and staff nurses.",
    note = "Values are mean (SD) or n (%). ATT, average treatment effect on the treated; SMD, standardized mean difference; X2, chi-square statistic. Work schedule is descriptive and was handled as a role-related pathway variable.",
    font_size = 7.2
  )

  doc <- officer::body_add_par(doc, "Occupational-Health Outcomes", style = "heading 2")
  doc <- officer::body_add_par(
    doc,
    "In the total role model, head nurses appeared to have better sleep-related outcomes. After night-shift adjustment and in the day-shift-only comparison, the sleep advantage weakened and managerial-strain outcomes became more apparent.",
    style = "Normal"
  )
  doc <- docx_add_table(
    doc,
    build_key_results_table(),
    "Table 2. Weighted associations between head-nurse status and key occupational-health outcomes.",
    note = "M1 preserves the real-world schedule difference; M2 adjusts the outcome model for night shift; M3 restricts to day-shift-only nurses and re-estimates propensity scores.",
    font_size = 8
  )

  doc <- officer::body_add_par(doc, "Risk-Domain Transition", style = "heading 2")
  doc <- docx_add_table(
    doc,
    build_transition_table(),
    "Table 3. Risk-transition summary across total, night-shift-adjusted, and day-shift-only models.",
    note = "For continuous outcomes, estimates are mean differences. For binary outcomes, estimates are prevalence ratios.",
    font_size = 8
  )
  doc <- docx_add_image(
    doc,
    file.path(out_dir, "figure_risk_domain_transition.png"),
    "Figure 2. Risk-domain transition across total, night-shift-adjusted, and day-shift-only models.",
    width = 6.7,
    height = 3.0
  )

  doc <- officer::body_add_par(doc, "Discussion", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "The main trial-run signal is not that head nurses are simply healthier or less healthy than staff nurses. Rather, the role appears to shift the occupational-health profile. The schedule pathway yields an apparent sleep and circadian advantage, while the managerial pathway reveals higher strain after night shift is accounted for analytically or removed by design.",
    style = "Normal"
  )
  doc <- officer::body_add_par(
    doc,
    "For nursing management and workforce policy, absence of night shift should not be used as a proxy for low occupational risk. Head-nurse support should address administrative workload, leadership support, emotional labor, decision latitude, work-family boundaries, and early turnover-risk signals.",
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "Strengths and Limitations", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "Strengths include a large sample, explicit cleaning rules, ATT weighting, standard balance diagnostics, and a three-model strategy that separates schedule structure from role-related strain. Limitations include the cross-sectional design, residual confounding, local trial-data status, and lack of direct measures of span of control, staffing ratios, unit culture, and managerial workload.",
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "Conclusions", style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "This R-generated pilot manuscript supports a risk-transition interpretation: head nurses have reduced night-shift exposure, but after accounting for schedule differences they show clearer managerial-strain risks. Final claims should be updated after the bastion/RStudio Server analysis.",
    style = "Normal"
  )

  print(doc, target = main_docx)
  log_msg("Saved R-generated main manuscript: ", normalizePath(main_docx, winslash = "/", mustWork = FALSE))
  main_docx
}

build_supplement_docx <- function() {
  supp_docx <- file.path(out_dir, "head_nurse_propensity_health_supplementary_material_trial_R.docx")
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Supplementary Material", style = "heading 1")
  doc <- officer::body_add_par(doc, "Head-nurse occupational-health risk-transition analysis, local trial run generated entirely in R.", style = "Normal")

  doc <- officer::body_add_par(doc, "Table and Figure Placement", style = "heading 1")
  doc <- docx_add_table(doc, table_figure_plan, "Supplementary Table S0. Recommended table and figure placement.", font_size = 7.2)

  doc <- officer::body_add_par(doc, "Cleaning and Score Audit", style = "heading 1")
  doc <- docx_add_table(doc, flow_steps, "Supplementary Table S1. Cleaning flow.", font_size = 8)
  doc <- docx_add_table(doc, score_range_summary, "Supplementary Table S2. Score range and missingness audit.", font_size = 8)

  doc <- officer::body_add_par(doc, "Propensity Score and Balance Diagnostics", style = "heading 1")
  doc <- docx_add_table(doc, ps_summary, "Supplementary Table S3. Propensity-score summary.", font_size = 8)
  doc <- docx_add_table(doc, balance_m1, "Supplementary Table S4. Full-sample covariate balance before and after ATT weighting.", font_size = 7.5)
  if (nrow(balance_m3) > 0) {
    doc <- docx_add_table(doc, balance_m3, "Supplementary Table S5. Day-shift-only covariate balance before and after ATT weighting.", font_size = 7.5)
  }
  doc <- docx_add_image(doc, file.path(out_dir, "figure_love_plot_model1.png"), "Supplementary Figure S1. Full-sample Love plot.", width = 6.3, height = 5.4)
  doc <- docx_add_image(doc, file.path(out_dir, "figure_propensity_overlap_weights.png"), "Supplementary Figure S2. Propensity-score overlap and ATT weight distribution.", width = 6.3, height = 5.4)
  doc <- docx_add_image(doc, file.path(out_dir, "figure_love_plot_model3_day_only.png"), "Supplementary Figure S3. Day-shift-only Love plot.", width = 6.3, height = 5.4)

  doc <- officer::body_add_par(doc, "Outcome Models", style = "heading 1")
  full_outcomes <- all_results %>%
    mutate(
      Model = unname(docx_model_short[model]),
      Outcome = label,
      Effect = docx_effect(type, estimate),
      `95% CI` = paste0(docx_fmt_num(conf_low, 2), " to ", docx_fmt_num(conf_high, 2)),
      P = docx_fmt_p(p_value),
      FDR = docx_fmt_p(p_fdr),
      `Head mean/prevalence` = docx_fmt_num(head_mean_or_prev, 3),
      `Staff mean/prevalence` = docx_fmt_num(staff_mean_or_prev, 3)
    ) %>%
    select(Model, Outcome, type, n, n_head_nurse, n_staff, contrast, Effect, `95% CI`, P, FDR, `Head mean/prevalence`, `Staff mean/prevalence`, note)
  doc <- docx_add_table(doc, full_outcomes, "Supplementary Table S6. Full weighted outcome models and sensitivity analyses.", font_size = 6.5)
  doc <- docx_add_table(doc, risk_transition_summary, "Supplementary Table S7. Full risk-transition summary.", font_size = 7.2)
  doc <- docx_add_image(doc, file.path(out_dir, "figure_continuous_outcome_differences.png"), "Supplementary Figure S4. Full continuous-outcome forest plot.", width = 6.7, height = 4.2)

  print(doc, target = supp_docx)
  log_msg("Saved R-generated supplementary material: ", normalizePath(supp_docx, winslash = "/", mustWork = FALSE))
  supp_docx
}

convert_docx_to_pdf <- function(docx_path, tag) {
  soffice <- Sys.which("soffice")
  if (!nzchar(soffice)) {
    log_msg("LibreOffice soffice not found; skipped PDF render for ", basename(docx_path))
    return(NA_character_)
  }
  render_dir <- file.path(out_dir, paste0("rendered_R_", tag))
  dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)
  cli_path <- function(path) {
    path <- normalizePath(path, winslash = "\\", mustWork = TRUE)
    if (.Platform$OS.type == "windows") {
      return(utils::shortPathName(path))
    }
    path
  }
  args <- c(
    "--headless",
    "--convert-to", "pdf",
    "--outdir", cli_path(render_dir),
    cli_path(docx_path)
  )
  res <- tryCatch(system2(soffice, args = args, stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  log_msg("LibreOffice render output for ", basename(docx_path), ": ", paste(res, collapse = " | "))
  pdf_path <- file.path(render_dir, sub("\\.docx$", ".pdf", basename(docx_path), ignore.case = TRUE))
  if (file.exists(pdf_path)) {
    log_msg("Saved PDF render: ", normalizePath(pdf_path, winslash = "/", mustWork = FALSE))
    return(pdf_path)
  }
  NA_character_
}

skip_word_outputs <- toupper(Sys.getenv("HEAD_NURSE_SKIP_WORD", unset = "TRUE")) %in%
  c("TRUE", "T", "1", "YES", "Y")
optional_docx_packages <- c("officer", "flextable")
missing_docx_packages <- optional_docx_packages[
  !vapply(optional_docx_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (skip_word_outputs) {
  main_docx_path <- NA_character_
  supp_docx_path <- NA_character_
  main_pdf_path <- NA_character_
  supp_pdf_path <- NA_character_
  log_msg(
    "Skipped R-generated DOCX/PDF outputs because HEAD_NURSE_SKIP_WORD=TRUE. ",
    "Core CSV/PNG/statistical outputs were saved."
  )
} else if (length(missing_docx_packages) == 0) {
  main_docx_path <- build_main_docx()
  supp_docx_path <- build_supplement_docx()
  main_pdf_path <- convert_docx_to_pdf(main_docx_path, "main_docx")
  supp_pdf_path <- convert_docx_to_pdf(supp_docx_path, "supplement_docx")
} else {
  main_docx_path <- NA_character_
  supp_docx_path <- NA_character_
  main_pdf_path <- NA_character_
  supp_pdf_path <- NA_character_
  log_msg(
    "Skipped R-generated DOCX/PDF outputs because optional packages are missing: ",
    paste(missing_docx_packages, collapse = ", "),
    ". Core CSV/PNG/statistical outputs were still saved."
  )
}

writeLines(log_lines, file.path(out_dir, "analysis_run_log.txt"), useBytes = TRUE)
log_msg("Saved outputs to: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
writeLines(log_lines, file.path(out_dir, "analysis_run_log.txt"), useBytes = TRUE)
