# TARGET male nurses career-development analysis pipeline
# Date: 2026-06-01
#
# Purpose
# -------
# Reproduce the main analyses for the manuscript:
# "Career development and turnover intention among male nurses in China:
# a cross-sectional analysis of baseline data from the TARGET Nurses' Health Cohort"
#
# This script is designed for RStudio. It reads the original baseline data,
# constructs the analytic variables, fits the logistic regression models,
# exports publication-ready tables, and draws a forest plot.
#
# How to run in RStudio
# ---------------------
# Option 1. If the original dataset is already loaded as an object named data:
#   source("male_nurses_TARGET_career_pipeline_20260601.R")
#
# Option 2. If reading from a file:
#   Sys.setenv(TARGET_DATA_PATH = "D:/path/to/original_data.sav")
#   source("male_nurses_TARGET_career_pipeline_20260601.R")
#
# Option 3. Run directly from GitHub after this file is pushed:
#   source("https://raw.githubusercontent.com/QLYY820/R-/refs/heads/codex/shift-sleep-depression-code/github/male_nurses_TARGET_career_pipeline_20260601.R")
#
# Optional environment variables
# ------------------------------
# TARGET_DATA_PATH          Path to original data file. Supports .rds, .RData,
#                           .rda, .sav, .xlsx, .xls, .csv, .tsv.
# TARGET_OUTPUT_DIR         Output folder. If empty, a timestamped folder is
#                           created under ./analysis_outputs.
# TARGET_TURNOVER_CUTOFF    Numeric cutoff for high turnover intention.
#                           If empty, the sample-specific 75th percentile is used.
# TARGET_INSTALL_PACKAGES   Set to 1 only if you want this script to install
#                           missing optional packages automatically.
#
# Notes
# -----
# 1. This script does not save or upload any individual-level data to GitHub.
# 2. It intentionally does not overwrite earlier R code in this repository.
# 3. Variable names follow the original TARGET baseline questionnaire where
#    possible. If your local variable names differ, edit the name lists in
#    the "Variable construction helpers" section.

options(stringsAsFactors = FALSE)

message("Starting TARGET male nurses career-development pipeline...")

# -------------------------------------------------------------------------
# Package helpers
# -------------------------------------------------------------------------

install_if_requested <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(TRUE)
  }
  if (identical(Sys.getenv("TARGET_INSTALL_PACKAGES"), "1")) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  requireNamespace(pkg, quietly = TRUE)
}

need_pkg <- function(pkg, reason = NULL, required = FALSE) {
  ok <- install_if_requested(pkg)
  if (!ok) {
    msg <- paste0("Package '", pkg, "' is not installed")
    if (!is.null(reason)) {
      msg <- paste0(msg, " (", reason, ")")
    }
    if (required) {
      stop(msg, call. = FALSE)
    } else {
      message(msg, ". Related step will be skipped or use base R fallback.")
    }
  }
  ok
}

has_data_table <- need_pkg("data.table", "fast data import/export", required = FALSE)
has_ggplot2 <- need_pkg("ggplot2", "forest plot", required = FALSE)
has_readxl <- requireNamespace("readxl", quietly = TRUE)
has_haven <- requireNamespace("haven", quietly = TRUE)
has_logistf <- requireNamespace("logistf", quietly = TRUE)

# -------------------------------------------------------------------------
# General helpers
# -------------------------------------------------------------------------

dir_create <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_root <- Sys.getenv("TARGET_OUTPUT_DIR")
if (!nzchar(output_root)) {
  output_root <- file.path(getwd(), "analysis_outputs",
                           paste0("male_nurses_TARGET_career_", timestamp))
}
output_root <- dir_create(output_root)
tables_dir <- dir_create(file.path(output_root, "tables_csv"))
figures_dir <- dir_create(file.path(output_root, "figures"))
data_check_dir <- dir_create(file.path(output_root, "data_checks"))
logs_dir <- dir_create(file.path(output_root, "logs"))

log_file <- file.path(logs_dir, "run_log.txt")
log_message <- function(...) {
  text <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ",
                 paste(..., collapse = " "))
  message(text)
  cat(text, "\n", file = log_file, append = TRUE)
}

write_csv <- function(x, path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fwrite(x, path, bom = TRUE, na = "")
  } else {
    utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(path)
}

write_txt <- function(x, path) {
  cat(paste(x, collapse = "\n"), file = path, sep = "\n")
  invisible(path)
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}

fmt_p <- function(p) {
  out <- ifelse(is.na(p), NA_character_,
                ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
  out
}

fmt_or_ci <- function(or, low, high) {
  ifelse(is.na(or) | is.na(low) | is.na(high), NA_character_,
         paste0(fmt_num(or, 2), " (", fmt_num(low, 2), "-", fmt_num(high, 2), ")"))
}

to_num <- function(x, negative_to_na = TRUE) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  y <- suppressWarnings(as.numeric(x))
  if (negative_to_na) {
    y[y < 0] <- NA_real_
  }
  y
}

to_chr <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(x)
  }
  as.character(x)
}

get_col <- function(df, candidates, default = NA) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  rep(default, nrow(df))
}

has_col <- function(df, candidates) {
  any(candidates %in% names(df))
}

first_existing_name <- function(df, candidates) {
  candidates[candidates %in% names(df)][1]
}

mode_value <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) {
    return(NA)
  }
  ux[which.max(tabulate(match(x, ux)))]
}

safe_z <- function(x) {
  x <- to_num(x)
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }
  as.numeric((x - m) / s)
}

alpha_from_items <- function(df, items) {
  items <- items[items %in% names(df)]
  if (length(items) < 2) {
    return(NA_real_)
  }
  x <- as.data.frame(lapply(df[items], to_num))
  keep <- stats::complete.cases(x)
  x <- x[keep, , drop = FALSE]
  if (nrow(x) < 5) {
    return(NA_real_)
  }
  k <- ncol(x)
  item_var <- sum(vapply(x, stats::var, numeric(1), na.rm = TRUE))
  total_var <- stats::var(rowSums(x), na.rm = TRUE)
  if (is.na(total_var) || total_var == 0) {
    return(NA_real_)
  }
  (k / (k - 1)) * (1 - item_var / total_var)
}

# -------------------------------------------------------------------------
# Data loading
# -------------------------------------------------------------------------

read_first_dataframe_from_rdata <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  obj_names <- ls(env)
  if ("data" %in% obj_names && is.data.frame(env$data)) {
    return(env$data)
  }
  dataframes <- obj_names[vapply(obj_names, function(nm) is.data.frame(env[[nm]]), logical(1))]
  if (length(dataframes) == 0) {
    stop("No data.frame object found in RData file: ", path, call. = FALSE)
  }
  sizes <- vapply(dataframes, function(nm) nrow(env[[nm]]) * ncol(env[[nm]]), numeric(1))
  env[[dataframes[which.max(sizes)]]]
}

read_data_file <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  ext <- tolower(tools::file_ext(path))
  log_message("Reading data file:", path)
  if (ext == "rds") {
    return(readRDS(path))
  }
  if (ext %in% c("rdata", "rda")) {
    return(read_first_dataframe_from_rdata(path))
  }
  if (ext == "sav") {
    need_pkg("haven", "SPSS .sav import", required = TRUE)
    return(as.data.frame(haven::read_sav(path)))
  }
  if (ext %in% c("xlsx", "xls")) {
    need_pkg("readxl", "Excel import", required = TRUE)
    return(as.data.frame(readxl::read_excel(path)))
  }
  if (ext == "csv") {
    if (requireNamespace("data.table", quietly = TRUE)) {
      return(as.data.frame(data.table::fread(path, encoding = "UTF-8")))
    }
    return(utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8"))
  }
  if (ext %in% c("tsv", "txt")) {
    if (requireNamespace("data.table", quietly = TRUE)) {
      return(as.data.frame(data.table::fread(path, sep = "\t", encoding = "UTF-8")))
    }
    return(utils::read.delim(path, check.names = FALSE, fileEncoding = "UTF-8"))
  }
  stop("Unsupported file extension: ", ext, call. = FALSE)
}

find_candidate_data_file <- function() {
  env_path <- Sys.getenv("TARGET_DATA_PATH")
  if (nzchar(env_path) && file.exists(env_path)) {
    return(env_path)
  }

  search_dirs <- unique(c(getwd(), file.path("D:", "护士队列", "data")))
  patterns <- c("data.rds", "data.RData", "data.rda", "data.sav",
                "data.xlsx", "data.xls", "data.csv", "data.tsv")
  candidates <- character(0)
  for (d in search_dirs) {
    if (!dir.exists(d)) {
      next
    }
    candidates <- c(candidates, file.path(d, patterns))
    candidates <- c(candidates, list.files(d, pattern = "\\.(rds|RData|rda|sav|xlsx|xls|csv|tsv)$",
                                           full.names = TRUE, ignore.case = TRUE))
  }
  candidates <- unique(candidates[file.exists(candidates)])
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[1]
}

load_target_data <- function() {
  if (exists("data", envir = .GlobalEnv) && is.data.frame(get("data", envir = .GlobalEnv))) {
    log_message("Using data.frame object named 'data' from the global environment.")
    return(get("data", envir = .GlobalEnv))
  }
  path <- find_candidate_data_file()
  if (is.na(path)) {
    stop(
      paste(
        "No input data found.",
        "Load the original baseline dataset as an object named data, or set:",
        'Sys.setenv(TARGET_DATA_PATH = "D:/path/to/data.sav")',
        sep = "\n"
      ),
      call. = FALSE
    )
  }
  read_data_file(path)
}

raw <- load_target_data()
raw <- as.data.frame(raw)
log_message("Raw data dimensions:", nrow(raw), "rows x", ncol(raw), "columns.")

write_csv(data.frame(variable = names(raw)), file.path(data_check_dir, "raw_variable_names.csv"))

# -------------------------------------------------------------------------
# Variable construction helpers
# -------------------------------------------------------------------------

derive_age <- function(df) {
  age <- to_num(get_col(df, c("A_age", "age", "Age")))
  if (all(is.na(age))) {
    birth_year <- to_num(get_col(df, c("A_year", "birth_year", "year_birth")))
    age <- 2023 - birth_year
  }
  age
}

derive_work_years <- function(df) {
  wy <- to_num(get_col(df, c("A_gongzuoshichang", "work_years", "work_year", "workyears")))
  if (all(is.na(wy))) {
    start_year <- to_num(get_col(df, c("work_y", "work_start_year", "start_work_year")))
    wy <- 2023 - start_year
  }
  wy
}

derive_education <- function(df) {
  edu_code <- to_num(get_col(df, c("A_q4", "education", "edu")))
  out <- rep(NA_character_, nrow(df))
  out[!is.na(edu_code) & edu_code <= 2] <- "College_or_less"
  out[!is.na(edu_code) & edu_code == 3] <- "Bachelor"
  out[!is.na(edu_code) & edu_code >= 4] <- "Master_or_higher"
  factor(out, levels = c("College_or_less", "Bachelor", "Master_or_higher"))
}

derive_marital <- function(df) {
  x <- to_num(get_col(df, c("A_q6", "marital", "marriage")))
  out <- rep(NA_character_, nrow(df))
  out[x == 1] <- "Unmarried"
  out[x == 2] <- "Married"
  out[!is.na(x) & !(x %in% c(1, 2))] <- "Other"
  factor(out, levels = c("Unmarried", "Married", "Other"))
}

derive_department <- function(df) {
  x <- to_num(get_col(df, c("A_q9", "department", "dept")))
  out <- rep(NA_character_, nrow(df))
  out[x == 1] <- "Internal"
  out[x == 2] <- "Surgery"
  out[x == 3] <- "Emergency"
  out[x == 4] <- "Gynecology"
  out[x == 5] <- "Obstetrics"
  out[x == 6] <- "Pediatrics"
  out[x == 7] <- "Operating_room"
  out[x == 8] <- "ICU"
  out[x == 9] <- "Outpatient"
  out[!is.na(x) & !(x %in% 1:9)] <- "Other"
  factor(out, levels = c("Internal", "Surgery", "Emergency", "Gynecology",
                         "Obstetrics", "Pediatrics", "Operating_room", "ICU",
                         "Outpatient", "Other"))
}

derive_employment <- function(df) {
  x <- to_num(get_col(df, c("A_q10", "employment", "employment_type")))
  out <- rep(NA_character_, nrow(df))
  out[x == 1] <- "Permanent"
  out[x == 2] <- "Contract"
  out[x == 3] <- "Personnel_agency"
  out[x == 4] <- "Record_system"
  out[x == 5] <- "Labor_dispatch"
  out[!is.na(x) & !(x %in% 1:5)] <- "Other"
  factor(out, levels = c("Permanent", "Contract", "Personnel_agency",
                         "Record_system", "Labor_dispatch", "Other"))
}

derive_night_cat <- function(df) {
  monthly_name <- first_existing_name(df, c("C_q8", "night_count",
                                            "monthly_night_shifts",
                                            "night_shift_count"))
  monthly <- to_num(get_col(df, c("C_q8", "night_count", "monthly_night_shifts",
                                  "night_shift_count")))
  any_night <- to_num(get_col(df, c("C_q1", "night_shift", "night_work")))
  out <- rep(NA_character_, nrow(df))

  # Common TARGET coding for C_q8: 1 = <=4, 2 = 5-9, 3 or above = >=10.
  # If a true numeric night-shift-count column is supplied, categorize by count.
  if (!is.na(monthly_name) && monthly_name %in% c("monthly_night_shifts",
                                                  "night_shift_count")) {
    out[!is.na(monthly) & monthly == 0] <- "0_no_night"
    out[!is.na(monthly) & monthly > 0 & monthly <= 4] <- "1_<=4"
    out[!is.na(monthly) & monthly >= 5 & monthly <= 9] <- "2_5-9"
    out[!is.na(monthly) & monthly >= 10] <- "3_>=10"
  } else {
    out[!is.na(monthly) & monthly == 1] <- "1_<=4"
    out[!is.na(monthly) & monthly == 2] <- "2_5-9"
    out[!is.na(monthly) & monthly >= 3] <- "3_>=10"
  }

  # For nurses reporting no night-shift work, monthly night shifts are
  # structurally not applicable and are coded as the reference category.
  out[is.na(out) & !is.na(any_night)] <- "0_no_night"
  factor(out, levels = c("0_no_night", "1_<=4", "2_5-9", "3_>=10"))
}

derive_binary_outcomes <- function(df) {
  title_code <- to_num(get_col(df, c("A_q11", "professional_title", "title")))
  admin_code <- to_num(get_col(df, c("A_q12", "administrative_position", "admin_position")))
  income_code <- to_num(get_col(df, c("A_q13", "monthly_income", "income")))
  data.frame(
    senior_title = ifelse(is.na(title_code), NA_integer_,
                          ifelse(title_code %in% c(3, 4, 5), 1L, 0L)),
    admin_position = ifelse(is.na(admin_code), NA_integer_,
                            ifelse(admin_code %in% 2:6, 1L, 0L)),
    high_income = ifelse(is.na(income_code), NA_integer_,
                         ifelse(income_code >= 4, 1L, 0L))
  )
}

derive_turnover <- function(df, male_only = TRUE) {
  item_names <- c("C_q47_1", "C_q47_2", "C_q47_3", "C_q48_1", "C_q48_2", "C_q49_1")
  if (has_col(df, c("C_lizhiyiyuan_all", "turnover_total", "turnover_intention_score"))) {
    score <- to_num(get_col(df, c("C_lizhiyiyuan_all", "turnover_total",
                                  "turnover_intention_score")))
  } else {
    missing_items <- setdiff(item_names, names(df))
    if (length(missing_items) > 0) {
      warning("Turnover items missing: ", paste(missing_items, collapse = ", "))
    }
    items <- as.data.frame(lapply(df[intersect(item_names, names(df))], to_num))
    score <- if (ncol(items) == 0) {
      rep(NA_real_, nrow(df))
    } else {
      rowSums(items, na.rm = FALSE)
    }
  }

  env_cutoff <- Sys.getenv("TARGET_TURNOVER_CUTOFF")
  if (nzchar(env_cutoff)) {
    cutoff <- suppressWarnings(as.numeric(env_cutoff))
    if (is.na(cutoff)) {
      stop("TARGET_TURNOVER_CUTOFF must be numeric.", call. = FALSE)
    }
  } else {
    cutoff <- as.numeric(stats::quantile(score, 0.75, na.rm = TRUE, type = 1))
  }

  data.frame(
    turnover_score = score,
    high_turnover = ifelse(is.na(score), NA_integer_, ifelse(score >= cutoff, 1L, 0L)),
    turnover_avg3 = ifelse(is.na(score), NA_integer_, ifelse(score / 6 >= 3, 1L, 0L)),
    turnover_cutoff = cutoff
  )
}

derive_scale_scores <- function(df) {
  data.frame(
    PSQI = to_num(get_col(df, c("D_PSQI_ALL", "PSQI", "psqi_total"))),
    anxiety = to_num(get_col(df, c("D_jiaolv_all", "GAD7", "anxiety", "gad7_total"))),
    depression = to_num(get_col(df, c("D_yiyu_all", "PHQ9", "depression", "phq9_total"))),
    negative_behavior = to_num(get_col(df, c("F_fuxingxingwei_all", "negative_behavior",
                                             "negative_workplace_behavior", "NAQ_R"))),
    burnout = to_num(get_col(df, c("F_zhiyejuandai_all", "burnout", "MBI_total"))),
    social_support = to_num(get_col(df, c("F_shehuizhichi_all", "social_support",
                                          "PSSS_total"))),
    fatigue = to_num(get_col(df, c("G_pifa_all", "fatigue", "CFS_total")))
  )
}

build_analysis_data <- function(df) {
  sex <- to_num(get_col(df, c("A_q2", "sex", "gender")))
  age <- derive_age(df)
  work_years <- derive_work_years(df)
  binaries <- derive_binary_outcomes(df)
  turnover <- derive_turnover(df)
  scales <- derive_scale_scores(df)

  dat <- data.frame(
    id = get_col(df, c("id", "ID", "participant_id", "A_id"), default = NA),
    sex = sex,
    male = ifelse(is.na(sex), NA_integer_, ifelse(sex == 1, 1L, 0L)),
    age = age,
    work_years = work_years,
    BMI = to_num(get_col(df, c("BMI", "bmi", "A_BMI"))),
    age_group = cut(age, breaks = c(-Inf, 29, 39, 49, Inf),
                    labels = c("<30", "30-39", "40-49", ">=50"), right = TRUE),
    work_years_group = cut(work_years, breaks = c(-Inf, 5, 10, 20, Inf),
                           labels = c("<=5", "6-10", "11-20", ">20"), right = TRUE),
    edu = derive_education(df),
    marital = derive_marital(df),
    dept = derive_department(df),
    employment = derive_employment(df),
    night_cat = derive_night_cat(df),
    binaries,
    turnover,
    scales,
    stringsAsFactors = FALSE
  )

  dat$night_work <- ifelse(is.na(dat$night_cat), NA_integer_,
                           ifelse(dat$night_cat == "0_no_night", 0L, 1L))

  dat$z_PSQI <- safe_z(dat$PSQI)
  dat$z_anxiety <- safe_z(dat$anxiety)
  dat$z_depression <- safe_z(dat$depression)
  dat$z_negative_behavior <- safe_z(dat$negative_behavior)
  dat$z_burnout <- safe_z(dat$burnout)
  dat$z_social_support <- safe_z(dat$social_support)
  dat$z_fatigue <- safe_z(dat$fatigue)

  dat
}

analysis_all <- build_analysis_data(raw)
male_index <- !is.na(analysis_all$male) & analysis_all$male == 1

# Define high turnover intention using the male-nurse analytic sample. This
# reproduces the manuscript definition based on the 75th percentile in the
# study sample, unless TARGET_TURNOVER_CUTOFF is explicitly supplied.
env_cutoff <- Sys.getenv("TARGET_TURNOVER_CUTOFF")
if (nzchar(env_cutoff)) {
  turnover_cutoff <- suppressWarnings(as.numeric(env_cutoff))
  if (is.na(turnover_cutoff)) {
    stop("TARGET_TURNOVER_CUTOFF must be numeric.", call. = FALSE)
  }
} else {
  turnover_cutoff <- as.numeric(stats::quantile(analysis_all$turnover_score[male_index],
                                                0.75, na.rm = TRUE, type = 1))
}
analysis_all$turnover_cutoff <- turnover_cutoff
analysis_all$high_turnover <- ifelse(is.na(analysis_all$turnover_score), NA_integer_,
                                     ifelse(analysis_all$turnover_score >=
                                              turnover_cutoff, 1L, 0L))
analysis_all$turnover_avg3 <- ifelse(is.na(analysis_all$turnover_score), NA_integer_,
                                     ifelse(analysis_all$turnover_score / 6 >= 3,
                                            1L, 0L))

male <- analysis_all[male_index, , drop = FALSE]
log_message("Analytic male nurse sample:", nrow(male), "rows.")

write_csv(
  data.frame(
    item = c("Raw rows", "Rows with complete sex", "Male nurses"),
    n = c(nrow(raw), sum(!is.na(analysis_all$sex)), nrow(male))
  ),
  file.path(data_check_dir, "sample_counts.csv")
)

write_csv(
  data.frame(
    variable = names(analysis_all),
    missing_male = vapply(male, function(x) sum(is.na(x)), integer(1)),
    missing_male_pct = round(vapply(male, function(x) mean(is.na(x)) * 100, numeric(1)), 2)
  ),
  file.path(data_check_dir, "missingness_male.csv")
)

# -------------------------------------------------------------------------
# Descriptive tables
# -------------------------------------------------------------------------

n_pct <- function(x, level = NULL, denom = length(x)) {
  if (is.null(level)) {
    n <- sum(!is.na(x))
  } else {
    n <- sum(x == level, na.rm = TRUE)
  }
  pct <- ifelse(denom > 0, 100 * n / denom, NA_real_)
  paste0(n, " (", fmt_num(pct, 1), ")")
}

mean_sd <- function(x) {
  paste0(fmt_num(mean(x, na.rm = TRUE), 2), " +/- ", fmt_num(stats::sd(x, na.rm = TRUE), 2))
}

median_iqr <- function(x) {
  qs <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 2)
  paste0(fmt_num(qs[[2]], 0), " (", fmt_num(qs[[1]], 0), "-", fmt_num(qs[[3]], 0), ")")
}

blank_repeated_variables <- function(tab) {
  if (!("Variable" %in% names(tab))) {
    return(tab)
  }
  out <- tab
  dup <- duplicated(out$Variable)
  out$Variable[dup] <- ""
  out
}

make_table1 <- function(dat) {
  rows <- list(
    data.frame(Variable = "Age, years", Category = "Mean +/- SD", Result = mean_sd(dat$age)),
    data.frame(Variable = "Age, years", Category = "Median (IQR)", Result = median_iqr(dat$age)),
    data.frame(Variable = "Work years", Category = "Mean +/- SD", Result = mean_sd(dat$work_years)),
    data.frame(Variable = "Work years", Category = "Median (IQR)", Result = median_iqr(dat$work_years)),
    data.frame(Variable = "BMI", Category = "Mean +/- SD", Result = mean_sd(dat$BMI))
  )
  add_levels <- function(var, label) {
    x <- dat[[var]]
    levs <- levels(x)
    if (is.null(levs)) {
      levs <- sort(unique(x[!is.na(x)]))
    }
    do.call(rbind, lapply(levs, function(lv) {
      data.frame(Variable = label, Category = as.character(lv), Result = n_pct(x, lv, nrow(dat)))
    }))
  }
  rows <- c(rows, list(
    add_levels("age_group", "Age group"),
    add_levels("edu", "Highest education"),
    add_levels("marital", "Marital status"),
    add_levels("dept", "Department"),
    add_levels("employment", "Employment type"),
    add_levels("night_cat", "Monthly night shifts")
  ))
  tab <- do.call(rbind, rows)
  row.names(tab) <- NULL
  tab
}

make_table2 <- function(dat) {
  cutoff <- mode_value(dat$turnover_cutoff)
  data.frame(
    Indicator = c(
      "Bachelor degree or higher",
      "Intermediate or higher professional title",
      "Administrative position",
      "Monthly income >=9001 RMB",
      paste0("High turnover intention, score >=", cutoff),
      "Turnover intention item average >=3",
      "Turnover intention score"
    ),
    Result = c(
      n_pct(dat$edu %in% c("Bachelor", "Master_or_higher"), TRUE, nrow(dat)),
      n_pct(dat$senior_title, 1, nrow(dat)),
      n_pct(dat$admin_position, 1, nrow(dat)),
      n_pct(dat$high_income, 1, nrow(dat)),
      n_pct(dat$high_turnover, 1, nrow(dat)),
      n_pct(dat$turnover_avg3, 1, nrow(dat)),
      median_iqr(dat$turnover_score)
    ),
    stringsAsFactors = FALSE
  )
}

table1 <- make_table1(male)
table1_display <- blank_repeated_variables(table1)
table2 <- make_table2(male)

write_csv(table1, file.path(tables_dir, "Table1_male_characteristics_full_labels.csv"))
write_csv(table1_display, file.path(tables_dir, "Table1_male_characteristics_blank_repeated_labels.csv"))
write_csv(table2, file.path(tables_dir, "Table2_career_turnover_indicators.csv"))

# -------------------------------------------------------------------------
# Male vs female comparison
# -------------------------------------------------------------------------

p_for_binary <- function(x, sex) {
  keep <- !is.na(x) & !is.na(sex)
  tab <- table(sex[keep], x[keep])
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(NA_real_)
  }
  if (any(tab < 5)) {
    return(stats::fisher.test(tab)$p.value)
  }
  stats::chisq.test(tab)$p.value
}

compare_binary <- function(dat, var, label, event = 1) {
  male_idx <- !is.na(dat$male) & dat$male == 1
  female_idx <- !is.na(dat$male) & dat$male == 0
  male_x <- dat[[var]][male_idx]
  female_x <- dat[[var]][female_idx]
  data.frame(
    Indicator = label,
    Male = n_pct(male_x, event, sum(male_idx)),
    Female = n_pct(female_x, event, sum(female_idx)),
    P_value = fmt_p(p_for_binary(dat[[var]] == event, dat$male)),
    stringsAsFactors = FALSE
  )
}

s1 <- do.call(rbind, list(
  compare_binary(transform(analysis_all,
                           edu_high = edu %in% c("Bachelor", "Master_or_higher")),
                 "edu_high", "Bachelor degree or higher", TRUE),
  compare_binary(analysis_all, "senior_title",
                 "Intermediate or higher professional title", 1),
  compare_binary(analysis_all, "admin_position", "Administrative position", 1),
  compare_binary(analysis_all, "high_income", "Monthly income >=9001 RMB", 1),
  compare_binary(analysis_all, "high_turnover", "High turnover intention", 1)
))
write_csv(s1, file.path(tables_dir, "S1_male_female_career_comparison.csv"))

# -------------------------------------------------------------------------
# Logistic regression models
# -------------------------------------------------------------------------

z_terms <- c("z_PSQI", "z_anxiety", "z_depression", "z_negative_behavior",
             "z_burnout", "z_social_support", "z_fatigue")
base_covars <- c("edu", "marital", "work_years_group", "dept", "employment",
                 "night_cat", z_terms)
age_covars <- c("edu", "marital", "age_group", "dept", "employment",
                "night_cat", z_terms)

model_formula <- function(outcome, covars) {
  stats::as.formula(paste(outcome, "~", paste(covars, collapse = " + ")))
}

label_map <- c(
  "(Intercept)" = "Intercept",
  "eduBachelor" = "Education: bachelor vs college or less",
  "eduMaster_or_higher" = "Education: master or higher vs college or less",
  "maritalMarried" = "Marital status: married vs unmarried",
  "maritalOther" = "Marital status: other vs unmarried",
  "work_years_group6-10" = "Work years: 6-10 vs <=5",
  "work_years_group11-20" = "Work years: 11-20 vs <=5",
  "work_years_group>20" = "Work years: >20 vs <=5",
  "age_group30-39" = "Age group: 30-39 vs <30",
  "age_group40-49" = "Age group: 40-49 vs <30",
  "age_group>=50" = "Age group: >=50 vs <30",
  "deptSurgery" = "Department: surgery vs internal medicine",
  "deptEmergency" = "Department: emergency vs internal medicine",
  "deptGynecology" = "Department: gynecology vs internal medicine",
  "deptObstetrics" = "Department: obstetrics vs internal medicine",
  "deptPediatrics" = "Department: pediatrics vs internal medicine",
  "deptOperating_room" = "Department: operating room vs internal medicine",
  "deptICU" = "Department: ICU vs internal medicine",
  "deptOutpatient" = "Department: outpatient vs internal medicine",
  "deptOther" = "Department: other vs internal medicine",
  "employmentContract" = "Employment type: contract vs permanent",
  "employmentPersonnel_agency" = "Employment type: personnel agency vs permanent",
  "employmentRecord_system" = "Employment type: record-system vs permanent",
  "employmentLabor_dispatch" = "Employment type: labor dispatch vs permanent",
  "employmentOther" = "Employment type: other vs permanent",
  "night_cat1_<=4" = "Monthly night shifts: <=4 vs none",
  "night_cat2_5-9" = "Monthly night shifts: 5-9 vs none",
  "night_cat3_>=10" = "Monthly night shifts: >=10 vs none",
  "z_PSQI" = "PSQI, per SD increase",
  "z_anxiety" = "Anxiety, per SD increase",
  "z_depression" = "Depression, per SD increase",
  "z_negative_behavior" = "Negative workplace behaviors, per SD increase",
  "z_burnout" = "Burnout, per SD increase",
  "z_social_support" = "Social support, per SD increase",
  "z_fatigue" = "Fatigue, per SD increase",
  "senior_title" = "Intermediate or higher professional title: yes vs no",
  "admin_position" = "Administrative position: yes vs no",
  "high_income" = "Monthly income >=9001 RMB: yes vs no"
)

term_label <- function(term) {
  out <- unname(label_map[term])
  ifelse(is.na(out), term, out)
}

fit_glm_or <- function(formula, dat, model_name) {
  mf <- stats::model.frame(formula, data = dat, na.action = stats::na.omit)
  fit <- stats::glm(formula, data = mf, family = stats::binomial())
  sm <- summary(fit)$coefficients
  terms <- rownames(sm)
  beta <- sm[, "Estimate"]
  se <- sm[, "Std. Error"]
  out <- data.frame(
    Model = model_name,
    N = stats::nobs(fit),
    Variable = term_label(terms),
    Term = terms,
    OR = exp(beta),
    CI_low = exp(beta - 1.96 * se),
    CI_high = exp(beta + 1.96 * se),
    OR_95CI = fmt_or_ci(exp(beta), exp(beta - 1.96 * se), exp(beta + 1.96 * se)),
    P_value = sm[, "Pr(>|z|)"],
    P = fmt_p(sm[, "Pr(>|z|)"]),
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL
  list(fit = fit, table = out[out$Term != "(Intercept)", , drop = FALSE],
       data = mf)
}

models <- list()
models$S2_high_turnover <- fit_glm_or(
  model_formula("high_turnover",
                c(base_covars, "senior_title", "admin_position", "high_income")),
  male,
  "High turnover intention"
)
models$S3_title <- fit_glm_or(
  model_formula("senior_title", base_covars),
  male,
  "Intermediate or higher professional title"
)
models$S4_admin <- fit_glm_or(
  model_formula("admin_position", base_covars),
  male,
  "Administrative position"
)
models$S5_income <- fit_glm_or(
  model_formula("high_income", c(base_covars, "senior_title", "admin_position")),
  male,
  "Monthly income >=9001 RMB"
)
models$S6_age_sensitivity <- fit_glm_or(
  model_formula("high_turnover",
                c(age_covars, "senior_title", "admin_position", "high_income")),
  male,
  "High turnover intention, age-group sensitivity"
)
models$S7_turnover_avg3 <- fit_glm_or(
  model_formula("turnover_avg3",
                c(base_covars, "senior_title", "admin_position", "high_income")),
  male,
  "Turnover intention item average >=3"
)

write_csv(models$S2_high_turnover$table, file.path(tables_dir, "S2_full_model_high_turnover.csv"))
write_csv(models$S3_title$table, file.path(tables_dir, "S3_full_model_professional_title.csv"))
write_csv(models$S4_admin$table, file.path(tables_dir, "S4_full_model_administrative_position.csv"))
write_csv(models$S5_income$table, file.path(tables_dir, "S5_full_model_high_income.csv"))
write_csv(models$S6_age_sensitivity$table, file.path(tables_dir, "S6_age_group_sensitivity_high_turnover.csv"))
write_csv(models$S7_turnover_avg3$table, file.path(tables_dir, "S7_turnover_average3_sensitivity.csv"))

# Main text tables with prespecified/core variables. Full models are exported above.
main_high_turnover_terms <- c(
  "maritalMarried", "night_cat1_<=4", "night_cat2_5-9", "night_cat3_>=10",
  "z_PSQI", "z_anxiety", "z_depression", "z_negative_behavior",
  "z_burnout", "z_social_support", "z_fatigue", "high_income"
)
main_title_terms <- c(
  "eduBachelor", "eduMaster_or_higher",
  "work_years_group6-10", "work_years_group11-20", "work_years_group>20",
  "employmentContract", "employmentPersonnel_agency", "employmentLabor_dispatch"
)
main_admin_terms <- c(
  "eduBachelor", "eduMaster_or_higher",
  "work_years_group6-10", "work_years_group11-20", "work_years_group>20",
  "night_cat1_<=4", "night_cat2_5-9", "night_cat3_>=10",
  "employmentContract", "employmentPersonnel_agency", "employmentLabor_dispatch"
)
main_income_terms <- c(
  "eduBachelor", "eduMaster_or_higher",
  "work_years_group6-10", "work_years_group11-20", "work_years_group>20",
  "night_cat2_5-9", "night_cat3_>=10",
  "senior_title", "admin_position"
)

subset_terms <- function(tab, terms) {
  out <- tab[match(terms, tab$Term), , drop = FALSE]
  out <- out[!is.na(out$Term), c("Variable", "OR_95CI", "P"), drop = FALSE]
  names(out) <- c("Variable", "OR (95% CI)", "P value")
  out
}

write_csv(subset_terms(models$S2_high_turnover$table, main_high_turnover_terms),
          file.path(tables_dir, "Table3_main_high_turnover_core_variables.csv"))
write_csv(subset_terms(models$S3_title$table, main_title_terms),
          file.path(tables_dir, "Table4_main_professional_title_core_variables.csv"))
write_csv(subset_terms(models$S4_admin$table, main_admin_terms),
          file.path(tables_dir, "Table5_main_administrative_position_core_variables.csv"))
write_csv(subset_terms(models$S5_income$table, main_income_terms),
          file.path(tables_dir, "Table6_main_high_income_core_variables.csv"))

# -------------------------------------------------------------------------
# S8: Scale reliability and standardized predictor means/SD
# -------------------------------------------------------------------------

turnover_items <- c("C_q47_1", "C_q47_2", "C_q47_3", "C_q48_1", "C_q48_2", "C_q49_1")
alpha_tab <- data.frame(
  Scale = c("Turnover intention", "GAD-7", "PHQ-9",
    "Negative Acts Questionnaire-Revised",
    "Maslach Burnout Inventory", "Social support", "Chalder Fatigue Scale"),
  Cronbach_alpha = c(
    alpha_from_items(raw[male_index, , drop = FALSE], turnover_items),
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
  ),
  Note = c(
    "Calculated if all six turnover-intention items are present.",
    "Fill in item names in this script if item-level columns are needed.",
    "Fill in item names in this script if item-level columns are needed.",
    "Fill in item names in this script if item-level columns are needed.",
    "Fill in item names in this script if item-level columns are needed.",
    "Fill in item names in this script if item-level columns are needed.",
    "Fill in item names in this script if item-level columns are needed."
  ),
  stringsAsFactors = FALSE
)
alpha_tab$Cronbach_alpha <- round(alpha_tab$Cronbach_alpha, 3)
write_csv(alpha_tab, file.path(tables_dir, "S8a_scale_reliability_if_items_available.csv"))

scale_names <- c("PSQI", "anxiety", "depression", "negative_behavior",
                 "burnout", "social_support", "fatigue")
s8b <- data.frame(
  Variable = c("PSQI", "Anxiety", "Depression", "Negative workplace behaviors",
               "Burnout", "Social support", "Fatigue"),
  Mean = round(vapply(male[scale_names], mean, numeric(1), na.rm = TRUE), 2),
  SD = round(vapply(male[scale_names], stats::sd, numeric(1), na.rm = TRUE), 2),
  stringsAsFactors = FALSE
)
write_csv(s8b, file.path(tables_dir, "S8b_standardized_variable_mean_sd.csv"))

# -------------------------------------------------------------------------
# S9: Social support direction across adjustment levels
# -------------------------------------------------------------------------

s9_formulas <- list(
  Unadjusted = high_turnover ~ z_social_support,
  Demographic = high_turnover ~ z_social_support + edu + marital + work_years_group,
  Occupational = high_turnover ~ z_social_support + edu + marital + work_years_group +
    dept + employment + night_cat + senior_title + admin_position + high_income,
  Fully_adjusted = model_formula("high_turnover",
                                 c(base_covars, "senior_title", "admin_position",
                                   "high_income"))
)
s9_rows <- lapply(names(s9_formulas), function(nm) {
  fit <- fit_glm_or(s9_formulas[[nm]], male, nm)
  out <- fit$table[fit$table$Term == "z_social_support", , drop = FALSE]
  if (nrow(out) == 0) {
    return(data.frame(Model = nm, N = stats::nobs(fit$fit), OR_95CI = NA, P = NA))
  }
  data.frame(Model = nm, N = out$N[1], OR_95CI = out$OR_95CI[1], P = out$P[1])
})
s9 <- do.call(rbind, s9_rows)
write_csv(s9, file.path(tables_dir, "S9_social_support_direction_check.csv"))

# -------------------------------------------------------------------------
# S10: Sparse event counts
# -------------------------------------------------------------------------

event_count_table <- function(dat, outcome, var, outcome_label, var_label) {
  x <- dat[[var]]
  levs <- if (is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
  do.call(rbind, lapply(levs, function(lv) {
    idx <- !is.na(x) & x == lv
    n <- sum(idx)
    events <- sum(dat[[outcome]][idx] == 1, na.rm = TRUE)
    data.frame(
      Outcome = outcome_label,
      Variable = var_label,
      Category = as.character(lv),
      N = n,
      Events = events,
      Event_percent = round(ifelse(n > 0, 100 * events / n, NA_real_), 1),
      stringsAsFactors = FALSE
    )
  }))
}

s10 <- do.call(rbind, list(
  event_count_table(male, "senior_title", "edu",
                    "Intermediate or higher professional title", "Education"),
  event_count_table(male, "senior_title", "work_years_group",
                    "Intermediate or higher professional title", "Work years"),
  event_count_table(male, "admin_position", "edu",
                    "Administrative position", "Education"),
  event_count_table(male, "admin_position", "work_years_group",
                    "Administrative position", "Work years"),
  event_count_table(male, "high_income", "edu",
                    "Monthly income >=9001 RMB", "Education"),
  event_count_table(male, "high_income", "work_years_group",
                    "Monthly income >=9001 RMB", "Work years")
))
write_csv(s10, file.path(tables_dir, "S10_sparse_category_event_counts.csv"))

# -------------------------------------------------------------------------
# S11 and S12: Administrative-position sensitivity analyses
# -------------------------------------------------------------------------

admin_formula <- model_formula("admin_position", base_covars)

if (requireNamespace("logistf", quietly = TRUE)) {
  mf <- stats::model.frame(admin_formula, data = male, na.action = stats::na.omit)
  firth_fit <- logistf::logistf(admin_formula, data = mf)
  cf <- stats::coef(firth_fit)
  ci <- stats::confint(firth_fit)
  pvals <- firth_fit$prob
  firth_tab <- data.frame(
    Model = "Administrative position, Firth penalized logistic regression",
    N = nrow(mf),
    Variable = term_label(names(cf)),
    Term = names(cf),
    OR = exp(cf),
    CI_low = exp(ci[, 1]),
    CI_high = exp(ci[, 2]),
    OR_95CI = fmt_or_ci(exp(cf), exp(ci[, 1]), exp(ci[, 2])),
    P_value = pvals,
    P = fmt_p(pvals),
    stringsAsFactors = FALSE
  )
  firth_tab <- firth_tab[firth_tab$Term != "(Intercept)", , drop = FALSE]
  write_csv(firth_tab, file.path(tables_dir, "S11_firth_admin_position_model.csv"))
} else {
  write_txt(
    c("The 'logistf' package is not installed.",
      "Install it or set TARGET_INSTALL_PACKAGES=1 to run Firth penalized logistic regression.",
      "The standard administrative-position model is available in S4."),
    file.path(tables_dir, "S11_firth_admin_position_model_NOT_RUN.txt")
  )
}

male$edu_collapsed <- factor(ifelse(male$edu == "Master_or_higher", "Bachelor_or_higher",
                                    ifelse(male$edu == "Bachelor", "Bachelor_or_higher",
                                           ifelse(male$edu == "College_or_less",
                                                  "College_or_less", NA))),
                             levels = c("College_or_less", "Bachelor_or_higher"))
base_covars_collapsed <- c("edu_collapsed", "marital", "work_years_group", "dept",
                           "employment", "night_cat", z_terms)
collapsed_admin <- fit_glm_or(model_formula("admin_position", base_covars_collapsed),
                              male,
                              "Administrative position, collapsed education")
write_csv(collapsed_admin$table, file.path(tables_dir, "S12_admin_position_collapsed_education.csv"))

# -------------------------------------------------------------------------
# Forest plot for high turnover intention
# -------------------------------------------------------------------------

forest_terms <- c(
  "maritalMarried", "night_cat1_<=4", "night_cat2_5-9", "night_cat3_>=10",
  "z_PSQI", "z_anxiety", "z_depression", "z_negative_behavior",
  "z_burnout", "z_social_support", "z_fatigue", "high_income"
)
forest <- models$S2_high_turnover$table[
  models$S2_high_turnover$table$Term %in% forest_terms,
  c("Variable", "Term", "OR", "CI_low", "CI_high", "P"),
  drop = FALSE
]
forest$Variable <- factor(forest$Variable,
                          levels = rev(term_label(forest_terms)))
write_csv(forest, file.path(tables_dir, "Figure_forest_high_turnover_data.csv"))

if (requireNamespace("ggplot2", quietly = TRUE) && nrow(forest) > 0) {
  p <- ggplot2::ggplot(forest, ggplot2::aes(x = OR, y = Variable)) +
    ggplot2::geom_vline(xintercept = 1, linetype = 2, color = "grey45") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = CI_low, xmax = CI_high),
                            height = 0.18, linewidth = 0.5, color = "grey30") +
    ggplot2::geom_point(size = 2.3, color = "black") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Odds ratio (log scale)",
      y = NULL,
      title = "Factors associated with high turnover intention"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(size = 9)
    )
  ggplot2::ggsave(file.path(figures_dir, "forest_high_turnover.png"),
                  p, width = 7.5, height = 4.8, dpi = 600)
  ggplot2::ggsave(file.path(figures_dir, "forest_high_turnover.tiff"),
                  p, width = 7.5, height = 4.8, dpi = 600, compression = "lzw")
} else {
  write_txt("Forest plot was skipped because ggplot2 is not installed or model output is empty.",
            file.path(figures_dir, "forest_plot_NOT_RUN.txt"))
}

# -------------------------------------------------------------------------
# Analysis notes
# -------------------------------------------------------------------------

notes <- c(
  "TARGET male nurses career-development pipeline completed.",
  paste0("Output folder: ", output_root),
  paste0("Raw data dimensions: ", nrow(raw), " rows x ", ncol(raw), " columns"),
  paste0("Male nurses: ", nrow(male)),
  paste0("Turnover-intention cutoff used: ", mode_value(male$turnover_cutoff)),
  "",
  "Key files:",
  "tables_csv/Table1_male_characteristics_blank_repeated_labels.csv",
  "tables_csv/Table2_career_turnover_indicators.csv",
  "tables_csv/S2_full_model_high_turnover.csv",
  "tables_csv/S3_full_model_professional_title.csv",
  "tables_csv/S4_full_model_administrative_position.csv",
  "tables_csv/S5_full_model_high_income.csv",
  "tables_csv/S9_social_support_direction_check.csv",
  "tables_csv/S10_sparse_category_event_counts.csv",
  "figures/forest_high_turnover.png",
  "",
  "Interpretation reminders:",
  "1. This is a cross-sectional analysis; use 'associated with' instead of causal language.",
  "2. Monthly night-shift categories use no night shifts as the reference group.",
  "3. Continuous psychosocial variables are standardized; ORs represent per SD increase.",
  "4. If Firth regression is skipped, install the logistf package and rerun."
)
write_txt(notes, file.path(output_root, "README_analysis_outputs.txt"))

log_message("Pipeline completed. Outputs saved to:", output_root)
message("Done. Output folder: ", output_root)
