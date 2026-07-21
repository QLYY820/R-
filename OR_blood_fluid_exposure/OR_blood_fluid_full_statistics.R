#!/usr/bin/env Rscript

# Complete supplementary statistics for the operating-room nurse study.
# Exposure: ever occupational blood/body-fluid contact (E_q20: 1=yes, 2=no).
# Primary outcome: MBI emotional exhaustion. Cross-sectional associations only.
# The source data and scale dictionary are read-only; this script writes only to output_dir.
#
# Bastion/RStudio execution supports either:
#   1) a data.frame/data.table named `data` in .GlobalEnv; or
#   2) OR_DATA_FILE pointing to CSV, CSV.GZ, RDS, or XLSX; or
#   3) command-line arguments: data_file dictionary_file output_dir.
# Environment variables override the defaults:
#   OR_DATA_OBJECT, OR_DATA_FILE, OR_DICTIONARY_FILE, OR_OUTPUT_DIR,
#   OR_XLSX_SHEET, OR_INSTALL_PACKAGES.

set.seed(42)
args <- commandArgs(trailingOnly = TRUE)

arg_or_env <- function(index, env_name, default = "") {
  env_value <- Sys.getenv(env_name, unset = "")
  if (nzchar(env_value)) return(env_value)
  if (length(args) >= index && nzchar(args[index])) return(args[index])
  default
}

data_object_name <- Sys.getenv("OR_DATA_OBJECT", unset = "data")
input_arg <- arg_or_env(1L, "OR_DATA_FILE")
dictionary_arg <- arg_or_env(2L, "OR_DICTIONARY_FILE")
output_arg <- arg_or_env(3L, "OR_OUTPUT_DIR", file.path(getwd(), "OR_blood_fluid_results"))
xlsx_sheet <- Sys.getenv("OR_XLSX_SHEET", unset = "1")
if (grepl("^[0-9]+$", xlsx_sheet)) xlsx_sheet <- as.integer(xlsx_sheet)

has_data_object <- exists(data_object_name, envir = .GlobalEnv, inherits = FALSE) &&
  inherits(get(data_object_name, envir = .GlobalEnv, inherits = FALSE), c("data.frame", "data.table"))
if (!nzchar(input_arg) && !has_data_object) {
  stop(paste0(
    "No input data found. Load a data.frame named `", data_object_name,
    "` in RStudio, set OR_DATA_FILE, or run: Rscript OR_blood_fluid_full_statistics.R data_file [dictionary_file] [output_dir]"
  ))
}

if (nzchar(input_arg)) input_file <- normalizePath(input_arg, mustWork = TRUE) else input_file <- NA_character_
dictionary_file <- if (nzchar(dictionary_arg) && file.exists(dictionary_arg)) normalizePath(dictionary_arg, mustWork = TRUE) else NA_character_
dir.create(output_arg, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_arg, mustWork = TRUE)

required_packages <- c("data.table", "psych", "sandwich", "lmtest", "survey", "car", "quantreg", "MatchIt", "ggplot2", "readxl", "splines")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) && identical(Sys.getenv("OR_INSTALL_PACKAGES", unset = "0"), "1")) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
  missing_packages <- setdiff(required_packages, rownames(installed.packages()))
}
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "),
       ". Install them first, or set OR_INSTALL_PACKAGES=1 if the bastion host permits CRAN access.")
}

suppressWarnings(suppressPackageStartupMessages({
  library(data.table)
  library(psych)
  library(sandwich)
  library(lmtest)
  library(survey)
  library(car)
  library(quantreg)
  library(MatchIt)
  library(ggplot2)
  library(readxl)
  library(splines)
}))

options(stringsAsFactors = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
safe_div <- function(a, b) ifelse(is.finite(b) & b != 0, a / b, NA_real_)
clamp01 <- function(x) pmin(1, pmax(0, x))
write_csv <- function(x, name) fwrite(as.data.table(x), file.path(output_dir, name), bom = TRUE, na = "")

weighted_mean_safe <- function(x, w) {
  keep <- is.finite(x) & is.finite(w)
  if (!any(keep) || sum(w[keep]) <= 0) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

weighted_var_safe <- function(x, w) {
  keep <- is.finite(x) & is.finite(w)
  if (sum(keep) < 2L || sum(w[keep]) <= 0) return(NA_real_)
  m <- weighted_mean_safe(x[keep], w[keep])
  sum(w[keep] * (x[keep] - m)^2) / sum(w[keep])
}

smd_cont <- function(x, g, w = rep(1, length(x))) {
  m1 <- weighted_mean_safe(x[g == 1], w[g == 1])
  m0 <- weighted_mean_safe(x[g == 0], w[g == 0])
  v1 <- weighted_var_safe(x[g == 1], w[g == 1])
  v0 <- weighted_var_safe(x[g == 0], w[g == 0])
  c(smd = safe_div(m1 - m0, sqrt((v1 + v0) / 2)), vr = safe_div(v1, v0), m0 = m0, m1 = m1,
    sd0 = sqrt(v0), sd1 = sqrt(v1))
}

smd_binary <- function(x, g, w = rep(1, length(x))) {
  p1 <- weighted_mean_safe(x[g == 1], w[g == 1])
  p0 <- weighted_mean_safe(x[g == 0], w[g == 0])
  den <- sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  c(smd = safe_div(p1 - p0, den), p0 = p0, p1 = p1)
}

robust_stats <- function(fit, term, type = "HC3") {
  cc <- coeftest(fit, vcov. = vcovHC(fit, type = type))
  if (!term %in% rownames(cc)) return(c(estimate = NA, se = NA, lower = NA, upper = NA, p = NA))
  est <- unname(cc[term, 1]); se <- unname(cc[term, 2]); p <- unname(cc[term, 4])
  c(estimate = est, se = se, lower = est - 1.96 * se, upper = est + 1.96 * se, p = p)
}

new_model_matrix <- function(fit, newdata) {
  mm <- model.matrix(delete.response(terms(fit)), newdata, contrasts.arg = fit$contrasts, xlev = fit$xlevels)
  beta_names <- names(coef(fit))
  missing_cols <- setdiff(beta_names, colnames(mm))
  if (length(missing_cols)) for (nm in missing_cols) mm <- cbind(mm, setNames(data.frame(rep(0, nrow(mm))), nm))
  mm[, beta_names, drop = FALSE]
}

linear_gcomp <- function(fit, data) {
  d0 <- copy(as.data.table(data)); d1 <- copy(as.data.table(data))
  d0[, exposure := 0L]; d1[, exposure := 1L]
  x0 <- new_model_matrix(fit, d0); x1 <- new_model_matrix(fit, d1)
  b <- coef(fit); v <- vcovHC(fit, type = "HC3")
  g0 <- colMeans(x0); g1 <- colMeans(x1); gd <- g1 - g0
  m0 <- sum(g0 * b); m1 <- sum(g1 * b); dif <- sum(gd * b)
  se0 <- sqrt(drop(t(g0) %*% v %*% g0)); se1 <- sqrt(drop(t(g1) %*% v %*% g1)); sed <- sqrt(drop(t(gd) %*% v %*% gd))
  c(mean_no = m0, mean_no_lower = m0 - 1.96 * se0, mean_no_upper = m0 + 1.96 * se0,
    mean_yes = m1, mean_yes_lower = m1 - 1.96 * se1, mean_yes_upper = m1 + 1.96 * se1,
    difference = dif, diff_lower = dif - 1.96 * sed, diff_upper = dif + 1.96 * sed)
}

poisson_gcomp <- function(fit, data) {
  d0 <- copy(as.data.table(data)); d1 <- copy(as.data.table(data))
  d0[, exposure := 0L]; d1[, exposure := 1L]
  x0 <- new_model_matrix(fit, d0); x1 <- new_model_matrix(fit, d1)
  b <- coef(fit); v <- vcovHC(fit, type = "HC3")
  mu0 <- as.numeric(exp(x0 %*% b)); mu1 <- as.numeric(exp(x1 %*% b))
  p0 <- mean(mu0); p1 <- mean(mu1)
  g0 <- colMeans(x0 * mu0); g1 <- colMeans(x1 * mu1); gd <- g1 - g0
  se0 <- sqrt(drop(t(g0) %*% v %*% g0)); se1 <- sqrt(drop(t(g1) %*% v %*% g1)); sed <- sqrt(drop(t(gd) %*% v %*% gd))
  c(prev_no = p0, prev_no_lower = clamp01(p0 - 1.96 * se0), prev_no_upper = clamp01(p0 + 1.96 * se0),
    prev_yes = p1, prev_yes_lower = clamp01(p1 - 1.96 * se1), prev_yes_upper = clamp01(p1 + 1.96 * se1),
    prevalence_difference = p1 - p0, pd_lower = (p1 - p0) - 1.96 * sed, pd_upper = (p1 - p0) + 1.96 * sed)
}

score_sum <- function(D, vars) {
  z <- as.data.frame(lapply(D[, ..vars], num))
  out <- rowSums(z, na.rm = TRUE)
  out[rowSums(!is.na(z)) < length(vars)] <- NA_real_
  out
}

# Only these source columns are read from XLSX. This reduces memory use and also
# creates a fail-fast schema check for the real bastion-host dataset.
required_source_columns <- unique(c(
  "A_q9", "A_q12", "E_q20", "submittime.x", "A_age", "A_year", "work_y",
  "A_q2", "A_q4", "A_q5", "A_q6", "A_q10", "A_q11", "C_q8", "C_q42",
  "E_q12", "E_q14", "E_q16", "E_q18", paste0("E_q10_", 1:10),
  paste0("D_q19_", 1:8), paste0("D_q22_", 1:7), paste0("D_q23_", 1:9),
  paste0("D_q24_", 1:10), paste0("G_q3_", 1:14), paste0("F_q3_", 1:22),
  "D_shuimianzhiliang_A", "D_rushuishijian_B", "D_shuimianshijian_C",
  "D_shuimianxiaoliv_D", "D_shuimianzhangai_E", "D_cuimianyaowu_F",
  "D_rijiangongnengzhangai_G", "D_shishui_ESS", "D_jiaolv_all",
  "D_yiyu_all", "D_yali_all", "D_PSQI_ALL", "G_qutipifa", "G_naolipifa",
  "F_qingganshuaijie", "F_qurengehua", "F_gerenchengjiugan",
  paste0("E_q24_", 1:9, "_2"), paste0("E_q24_", 1:9, "_4")
))

stream_xlsx_with_python <- function(path, sheet_index = 1L) {
  python_bin <- Sys.getenv("OR_PYTHON", unset = "")
  if (!nzchar(python_bin)) {
    candidates <- Sys.which(c("python", "python3"))
    candidates <- unname(candidates[nzchar(candidates)])
    if (length(candidates)) {
      for (candidate in candidates) {
        probe <- suppressWarnings(tryCatch(
          system2(candidate, "--version", stdout = TRUE, stderr = TRUE),
          error = function(e) structure(character(), status = 1L)
        ))
        probe_status <- attr(probe, "status")
        if (is.null(probe_status) || identical(as.integer(probe_status), 0L)) {
          python_bin <- candidate
          break
        }
      }
    }
  }
  if (!nzchar(python_bin)) {
    stop(paste(
      "Large-XLSX streaming requires Python 3, but python/python3 was not found.",
      "Set OR_PYTHON to the Python executable, load the dataset as an R object named `data`,",
      "or provide CSV/RDS through OR_DATA_FILE."
    ))
  }
  if (!is.numeric(sheet_index) || length(sheet_index) != 1L || is.na(sheet_index)) {
    stop("For large XLSX input, OR_XLSX_SHEET must be a 1-based numeric sheet index.")
  }

  py_file <- tempfile("xlsx_stream_", fileext = ".py")
  columns_file <- tempfile("xlsx_columns_", fileext = ".txt")
  csv_file <- tempfile("xlsx_selected_", fileext = ".csv")
  on.exit(unlink(c(py_file, columns_file, csv_file), force = TRUE), add = TRUE)
  writeLines(required_source_columns, columns_file, useBytes = TRUE)

  # Standard-library-only helper: it never edits the workbook and materializes
  # only the locked analysis columns into a temporary CSV.
  py_code <- c(
    "import csv, re, sys, zipfile, xml.etree.ElementTree as ET",
    "NS = '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'",
    "def colnum(ref):",
    "    m = re.match(r'[A-Za-z]+', ref or '')",
    "    if not m: return None",
    "    n = 0",
    "    for ch in m.group().upper(): n = n * 26 + ord(ch) - 64",
    "    return n",
    "def cell_value(cell, shared):",
    "    typ = cell.attrib.get('t')",
    "    if typ == 'inlineStr': return ''.join(t.text or '' for t in cell.iter(NS + 't'))",
    "    value = cell.find(NS + 'v')",
    "    value = '' if value is None or value.text is None else value.text",
    "    if typ == 's' and value != '': return shared[int(value)]",
    "    return value",
    "src, out, columns_path, sheet_index = sys.argv[1:5]",
    "with open(columns_path, encoding='utf-8-sig') as fh:",
    "    wanted = [line.rstrip('\\r\\n') for line in fh if line.rstrip('\\r\\n')]",
    "with zipfile.ZipFile(src) as z:",
    "    shared = []",
    "    if 'xl/sharedStrings.xml' in z.namelist():",
    "        for event, elem in ET.iterparse(z.open('xl/sharedStrings.xml'), events=('end',)):",
    "            if elem.tag == NS + 'si':",
    "                shared.append(''.join(t.text or '' for t in elem.iter(NS + 't')))",
    "                elem.clear()",
    "    sheet = 'xl/worksheets/sheet%s.xml' % int(sheet_index)",
    "    if sheet not in z.namelist(): raise RuntimeError('Worksheet XML not found: ' + sheet)",
    "    selected_cols, selected_set, writer, fout, rows = None, None, None, None, 0",
    "    try:",
    "        for event, elem in ET.iterparse(z.open(sheet), events=('end',)):",
    "            if elem.tag != NS + 'row': continue",
    "            row_number = int(elem.attrib.get('r', '0'))",
    "            if row_number == 1:",
    "                headers = {}",
    "                for cell in elem.findall(NS + 'c'):",
    "                    idx = colnum(cell.attrib.get('r', ''))",
    "                    if idx is not None: headers[idx] = cell_value(cell, shared)",
    "                reverse = {}",
    "                for idx, name in headers.items(): reverse.setdefault(name, []).append(idx)",
    "                missing = [name for name in wanted if name not in reverse]",
    "                duplicates = [name for name in wanted if len(reverse.get(name, [])) > 1]",
    "                if missing: raise RuntimeError('Required columns missing: ' + ', '.join(missing))",
    "                if duplicates: raise RuntimeError('Duplicate required headers: ' + ', '.join(duplicates))",
    "                selected_cols = [reverse[name][0] for name in wanted]",
    "                selected_set = set(selected_cols)",
    "                fout = open(out, 'w', encoding='utf-8-sig', newline='')",
    "                writer = csv.writer(fout)",
    "                writer.writerow(wanted)",
    "            else:",
    "                if writer is None: raise RuntimeError('Header row was not parsed before data rows')",
    "                values = {}",
    "                for cell in elem.findall(NS + 'c'):",
    "                    idx = colnum(cell.attrib.get('r', ''))",
    "                    if idx in selected_set: values[idx] = cell_value(cell, shared)",
    "                writer.writerow([values.get(idx, '') for idx in selected_cols])",
    "                rows += 1",
    "                if rows % 10000 == 0: print('Extracted rows:', rows, flush=True)",
    "            elem.clear()",
    "    finally:",
    "        if fout is not None: fout.close()",
    "print('XLSX_STREAM_DONE rows=%d columns=%d' % (rows, len(wanted)))"
  )
  writeLines(py_code, py_file, useBytes = TRUE)
  cmd_args <- c(py_file, path, csv_file, columns_file, as.character(as.integer(sheet_index)))
  log <- system2(python_bin, args = shQuote(cmd_args), stdout = TRUE, stderr = TRUE)
  status <- attr(log, "status")
  if (is.null(status)) status <- 0L
  if (length(log)) message(paste(log, collapse = "\n"))
  if (!identical(as.integer(status), 0L) || !file.exists(csv_file)) {
    stop("Python XLSX streaming failed with status ", status, ". See the preceding log.")
  }
  fread(csv_file, na.strings = c("", "NA", "NaN"), showProgress = FALSE)
}

read_source_data <- function() {
  if (!nzchar(input_arg) && has_data_object) {
    message("Input mode: RStudio object `", data_object_name, "` (read-only copy).")
    return(copy(as.data.table(get(data_object_name, envir = .GlobalEnv, inherits = FALSE))))
  }
  ext <- tolower(input_file)
  if (grepl("\\.csv(\\.gz)?$", ext)) {
    message("Input mode: CSV/CSV.GZ (read-only).")
    return(fread(input_file, select = required_source_columns,
                 na.strings = c("", "NA", "NaN"), showProgress = FALSE))
  }
  if (grepl("\\.rds$", ext)) {
    message("Input mode: RDS (read-only).")
    return(copy(as.data.table(readRDS(input_file))))
  }
  if (grepl("\\.xlsx?$", ext)) {
    message("Input mode: large XLSX streamed to locked analysis columns (read-only).")
    return(stream_xlsx_with_python(input_file, xlsx_sheet))
  }
  stop("Unsupported input format. Use a data object, CSV/CSV.GZ, RDS, or XLSX.")
}

# Read-only source load and schema gate.
raw <- read_source_data()
missing_source_columns <- setdiff(required_source_columns, names(raw))
if (length(missing_source_columns)) {
  stop("Required source columns are missing: ", paste(missing_source_columns, collapse = ", "))
}
source_n <- nrow(raw)
expected_source_n <- suppressWarnings(as.integer(Sys.getenv("OR_EXPECTED_ROWS", unset = "64114")))
if (is.finite(expected_source_n) && source_n != expected_source_n) {
  warning("Source row count is ", source_n, "; expected ", expected_source_n,
          ". Analysis will continue without deleting or adding records.")
}
d <- raw[A_q9 == 7 & A_q12 == 1 & E_q20 %in% c(1, 2)]
or_all <- raw[A_q9 == 7 & A_q12 %in% 1:7 & E_q20 %in% c(1, 2)]

derive_vars <- function(x) {
  x <- copy(x)
  x[, exposure := as.integer(E_q20 == 1)]
  x[, exposure_group := factor(exposure, levels = c(0, 1), labels = c("No contact", "Ever contact"))]
  x[, survey_year := as.integer(substr(as.character(submittime.x), 1, 4))]
  x[, age_existing := num(A_age)]
  x[, age_calculated := survey_year - num(A_year)]
  x[, age := fifelse(age_existing >= 18 & age_existing <= 70, age_existing,
                     fifelse(age_calculated >= 18 & age_calculated <= 70, age_calculated, NA_real_))]
  x[, employment_start_year := num(work_y)]
  x[, work_years := survey_year - employment_start_year]
  x[work_years < 0 | work_years > 60, work_years := NA_real_]
  x[, sex := factor(A_q2, levels = c(1, 2), labels = c("Male", "Female"))]
  x[, first_education := factor(A_q4, levels = 1:4, labels = c("Technical secondary", "Junior college", "Bachelor", "Master or above"))]
  x[, highest_education := factor(A_q5, levels = 1:4, labels = c("Technical secondary", "Junior college", "Bachelor", "Master or above"))]
  x[, first_education_collapsed := factor(ifelse(first_education %in% c("Bachelor","Master or above"),"Bachelor or above","Below bachelor"),
                                          levels=c("Below bachelor","Bachelor or above"))]
  x[, highest_education_collapsed := factor(ifelse(highest_education %in% c("Bachelor","Master or above"),"Bachelor or above","Below bachelor"),
                                            levels=c("Below bachelor","Bachelor or above"))]
  x[, marital := factor(A_q6, levels = 1:6, labels = c("Unmarried", "Married", "Divorced", "Widowed", "Remarried", "Other"))]
  x[, employment := factor(A_q10, levels = 1:6, labels = c("Permanent", "Contract", "Personnel agency", "Filed appointment", "Labor dispatch", "Other"))]
  x[, title := factor(A_q11, levels = 1:6, labels = c("Nurse", "Senior nurse", "Supervisor nurse", "Associate chief nurse", "Chief nurse", "Other"))]
  x[, marital_collapsed := factor(ifelse(marital == "Married", "Married", ifelse(marital == "Unmarried", "Unmarried", "Other")),
                                  levels=c("Unmarried","Married","Other"))]
  x[, employment_collapsed := factor(ifelse(employment == "Permanent", "Permanent", ifelse(employment == "Contract", "Contract", "Other")),
                                     levels=c("Permanent","Contract","Other"))]
  x[, title_collapsed := factor(ifelse(title == "Nurse", "Nurse", ifelse(title == "Senior nurse", "Senior nurse",
                              ifelse(title == "Supervisor nurse", "Supervisor nurse", "Associate chief or higher/other"))),
                               levels=c("Nurse","Senior nurse","Supervisor nurse","Associate chief or higher/other"))]
  x[, admin_role := factor(A_q12, levels = 1:7, labels = c("None", "Deputy head nurse", "Head nurse", "Department head nurse", "Deputy nursing director", "Nursing director", "Other"))]
  x[, night_frequency := factor(fifelse(num(C_q8) == -3, 0L, as.integer(num(C_q8))), levels = 0:3,
                                labels = c("Day only", "<=4/month", "5-9/month", ">=10/month"))]
  x[, any_night_shift := factor(ifelse(night_frequency == "Day only", "Day only", "Any night shift"),
                                levels = c("Day only", "Any night shift"))]
  x[, overtime_weeks := num(C_q42) - 1]
  exp_levels <- c("Never", "<1 day/week", "1-3 days/week", "4-7 days/week")
  x[, anesthetic_gas := factor(E_q12, levels = 1:4, labels = exp_levels)]
  x[, antineoplastic_drug := factor(E_q14, levels = 1:4, labels = exp_levels)]
  x[, antiviral_drug := factor(E_q16, levels = 1:4, labels = exp_levels)]
  x[, radiation := factor(E_q18, levels = 1:4, labels = exp_levels)]
  dis_vars <- paste0("E_q10_", 1:10)
  dismat <- as.data.frame(lapply(x[, ..dis_vars], function(z) num(z) - 1))
  x[, disinfectant_index := rowMeans(dismat, na.rm = FALSE)]

  ess_items <- paste0("D_q19_", 1:8); gad_items <- paste0("D_q22_", 1:7)
  phq_items <- paste0("D_q23_", 1:9); pss_items <- paste0("D_q24_", 1:10)
  fat_p <- paste0("G_q3_", 1:8); fat_m <- paste0("G_q3_", 9:14)
  ee_items <- paste0("F_q3_", c(1,2,3,6,8,13,14,16,20))
  dp_items <- paste0("F_q3_", c(5,10,11,15,22))
  pa_items <- paste0("F_q3_", c(4,7,9,12,17,18,19,21))
  psqi_items <- c("D_shuimianzhiliang_A", "D_rushuishijian_B", "D_shuimianshijian_C",
                  "D_shuimianxiaoliv_D", "D_shuimianzhangai_E", "D_cuimianyaowu_F", "D_rijiangongnengzhangai_G")
  x[, ESS_recalc := score_sum(.SD, ess_items)]
  x[, GAD7_recalc := score_sum(.SD, gad_items)]
  x[, PHQ9_recalc := score_sum(.SD, phq_items)]
  pss_scored <- as.data.frame(lapply(x[, ..pss_items], num))
  pss_scored[, c(4,5,7,8)] <- lapply(pss_scored[, c(4,5,7,8), drop=FALSE], function(z) 4-z)
  x[, PSS_recalc := ifelse(rowSums(!is.na(pss_scored)) == 10, rowSums(pss_scored), NA_real_)]
  x[, physical_fatigue_recalc := score_sum(.SD, fat_p)]
  mental_scored <- as.data.frame(lapply(x[, ..fat_m], num))
  mental_scored[, c(2,5,6)] <- lapply(mental_scored[, c(2,5,6), drop=FALSE], function(z) 1-z)
  x[, mental_fatigue_recalc := ifelse(rowSums(!is.na(mental_scored)) == 6, rowSums(mental_scored), NA_real_)]
  x[, MBI_EE_recalc := score_sum(.SD, ee_items)]
  x[, MBI_DP_recalc := score_sum(.SD, dp_items)]
  x[, MBI_PA_recalc := score_sum(.SD, pa_items)]
  x[, PSQI_recalc := score_sum(.SD, psqi_items)]

  x[, ESS := num(D_shishui_ESS)]
  x[, GAD7 := num(D_jiaolv_all)]
  x[, PHQ9 := num(D_yiyu_all)]
  x[, PSS := num(D_yali_all)]
  x[, PSQI := num(D_PSQI_ALL)]
  x[, physical_fatigue := num(G_qutipifa)]
  x[, mental_fatigue := num(G_naolipifa)]
  x[, MBI_EE := num(F_qingganshuaijie)]
  x[, MBI_DP := num(F_qurengehua)]
  x[, MBI_PA := num(F_gerenchengjiugan)]

  nmq7 <- paste0("E_q24_", 1:9, "_4"); nmqlim <- paste0("E_q24_", 1:9, "_2")
  yesno <- function(z) {
    zz <- as.character(z)
    fifelse(zz %in% c("2", "\u662f"), 1, fifelse(zz %in% c("1", "\u5426"), 0, NA_real_))
  }
  m7 <- as.data.frame(lapply(x[, ..nmq7], yesno)); ml <- as.data.frame(lapply(x[, ..nmqlim], yesno))
  x[, NMQ_7d_count := ifelse(rowSums(!is.na(m7)) == 9, rowSums(m7), NA_real_)]
  x[, NMQ_limit_count := ifelse(rowSums(!is.na(ml)) == 9, rowSums(ml), NA_real_)]

  x[, high_EE := as.integer(MBI_EE >= 27)]
  x[, high_DP := as.integer(MBI_DP >= 10)]
  x[, low_PA := as.integer(MBI_PA <= 33)]
  x[, high_burnout_any := as.integer(high_EE == 1 | high_DP == 1 | low_PA == 1)]
  x[, poor_sleep := as.integer(PSQI > 7)]
  x[, excessive_sleepiness := as.integer(ESS >= 11)]
  x[, PHQ9_ge10 := as.integer(PHQ9 >= 10)]
  x[, GAD7_ge10 := as.integer(GAD7 >= 10)]
  x[, any_NMQ_7d := as.integer(NMQ_7d_count > 0)]
  x[, any_NMQ_limit := as.integer(NMQ_limit_count > 0)]
  x[, age_group := cut(age, breaks = c(-Inf, 29, 39, Inf), labels = c("<30", "30-39", ">=40"), right = TRUE)]
  droplevels(x)
}

d <- derive_vars(d)
or_all <- derive_vars(or_all)

# Dictionary-backed variable definitions used by this analysis. The dictionary
# is optional on the bastion host because all scoring rules are locked below;
# when absent, the audit records that the extract was not regenerated.
if (is.na(dictionary_file)) {
  dict_extract <- data.table(
    variable = character(), description = character(), options = character(),
    scale_assignment = character(), scale_name = character(), dimension = character(),
    scoring = character(), interpretation = character()
  )
  message("Scale dictionary not supplied; dictionary extract will be empty.")
} else {
  dict <- as.data.table(read_excel(dictionary_file, sheet = 1, col_types = "text"))
  if (ncol(dict) < 12L) stop("Scale dictionary must contain at least 12 columns.")
  dict_extract <- dict[dict[[3]] %in% unique(c(
    "A_q2","A_q4","A_q5","A_q6","A_q9","A_q10","A_q11","A_q12","C_q8","C_q42",
    "E_q12","E_q14","E_q16","E_q18","E_q20", paste0("D_q19_",1:8), paste0("D_q22_",1:7),
    paste0("D_q23_",1:9), paste0("D_q24_",1:10), paste0("G_q3_",1:14), paste0("F_q3_",1:22),
    paste0("E_q24_",rep(1:9, each=4),"_",rep(1:4,9))
  )), c(3,6:12), with = FALSE]
  setnames(dict_extract, c("variable", "description", "options", "scale_assignment", "scale_name", "dimension", "scoring", "interpretation"))
}
write_csv(dict_extract, "21_dictionary_extract.csv")

# Cohort arithmetic and exclusions.
cohort_check <- data.table(
  metric = c("Source rows", "Expected source rows (audit only)", "Source row count matches expectation",
             "OR nurses with valid exposure including managers", "Ordinary clinical OR nurses with valid exposure",
             "Ever contact", "No contact", "Exposure prevalence percent"),
  value = c(source_n, expected_source_n, as.integer(source_n == expected_source_n), nrow(or_all), nrow(d),
            sum(d$exposure == 1), sum(d$exposure == 0), 100 * mean(d$exposure == 1))
)
write_csv(cohort_check, "22_cohort_arithmetic_check.csv")

demographic_quality <- data.table(
  check=c("Existing age outside 18-70 years","Calculated age outside 18-70 years","Future employment start year relative to module survey year",
          "Valid existing age differs from calculated age","Survey year unavailable"),
  n=c(sum(!(d$age_existing>=18 & d$age_existing<=70) | is.na(d$age_existing)),
      sum(!(d$age_calculated>=18 & d$age_calculated<=70) | is.na(d$age_calculated)),
      sum(d$employment_start_year>d$survey_year,na.rm=TRUE),
      sum(d$age_existing>=18 & d$age_existing<=70 & is.finite(d$age_calculated) & d$age_existing!=d$age_calculated),
      sum(is.na(d$survey_year))),
  handling=c("Use calculated age only when valid; otherwise set missing","Set missing when neither existing nor calculated age is valid",
             "Calculated work experience set missing","Existing age retained per protocol; discrepancy documented","Set derived age/work experience missing")
)
write_csv(demographic_quality,"22b_demographic_data_quality.csv")

# Recalculated totals versus existing totals.
score_pairs <- list(
  ESS = c("ESS", "ESS_recalc"), GAD7 = c("GAD7", "GAD7_recalc"), PHQ9 = c("PHQ9", "PHQ9_recalc"),
  PSS = c("PSS", "PSS_recalc"), PSQI = c("PSQI", "PSQI_recalc"),
  Physical_fatigue = c("physical_fatigue", "physical_fatigue_recalc"),
  Mental_fatigue = c("mental_fatigue", "mental_fatigue_recalc"),
  MBI_EE = c("MBI_EE", "MBI_EE_recalc"), MBI_DP = c("MBI_DP", "MBI_DP_recalc"), MBI_PA = c("MBI_PA", "MBI_PA_recalc")
)
score_compare <- rbindlist(lapply(names(score_pairs), function(nm) {
  p <- score_pairs[[nm]]; a <- d[[p[1]]]; b <- d[[p[2]]]; keep <- is.finite(a) & is.finite(b)
  data.table(scale = nm, existing_variable = p[1], recalculated_variable = p[2], n_compared = sum(keep),
             exact_match_n = sum(a[keep] == b[keep]), exact_match_pct = 100 * mean(a[keep] == b[keep]),
             mean_difference_recalc_minus_existing = mean(b[keep] - a[keep]),
             max_absolute_difference = max(abs(b[keep] - a[keep])), correlation = cor(a[keep], b[keep]))
}))
write_csv(score_compare, "23_score_recalculation_check.csv")

# Scale quality and reliability in the main OR cohort.
nmq7_items <- paste0("E_q24_",1:9,"_4"); nmqlim_items <- paste0("E_q24_",1:9,"_2")
nmq_score_matrix <- function(vars) as.data.frame(lapply(d[, ..vars], function(z) {
  zz <- as.character(z); fifelse(zz %in% c("2", "\u662f"), 1, fifelse(zz %in% c("1", "\u5426"), 0, NA_real_))
}))
psqi_component_items <- c("D_shuimianzhiliang_A","D_rushuishijian_B","D_shuimianshijian_C","D_shuimianxiaoliv_D","D_shuimianzhangai_E","D_cuimianyaowu_F","D_rijiangongnengzhangai_G")
ess_item_names <- paste0("D_q19_",1:8); gad_item_names <- paste0("D_q22_",1:7)
phq_item_names <- paste0("D_q23_",1:9); pss_item_names <- paste0("D_q24_",1:10)
fatigue_physical_items <- paste0("G_q3_",1:8); fatigue_mental_items <- paste0("G_q3_",9:14)
mbi_ee_items <- paste0("F_q3_",c(1,2,3,6,8,13,14,16,20))
mbi_dp_items <- paste0("F_q3_",c(5,10,11,15,22))
mbi_pa_items <- paste0("F_q3_",c(4,7,9,12,17,18,19,21))
pss_scored_items <- as.data.frame(lapply(d[, ..pss_item_names], num))
pss_scored_items[,c(4,5,7,8)] <- lapply(pss_scored_items[,c(4,5,7,8),drop=FALSE],function(z)4-z)
mental_fatigue_scored_items <- as.data.frame(lapply(d[, ..fatigue_mental_items], num))
mental_fatigue_scored_items[,c(2,5,6)] <- lapply(mental_fatigue_scored_items[,c(2,5,6),drop=FALSE],function(z)1-z)
scale_items <- list(
  NMQ_7d_count = nmq_score_matrix(nmq7_items), NMQ_limit_count = nmq_score_matrix(nmqlim_items),
  PSQI = as.data.frame(d[, ..psqi_component_items]),
  ESS = as.data.frame(d[, ..ess_item_names]), GAD7 = as.data.frame(d[, ..gad_item_names]),
  PHQ9 = as.data.frame(d[, ..phq_item_names]), PSS = pss_scored_items,
  Physical_fatigue = as.data.frame(d[, ..fatigue_physical_items]), Mental_fatigue = mental_fatigue_scored_items,
  MBI_EE = as.data.frame(d[, ..mbi_ee_items]), MBI_DP = as.data.frame(d[, ..mbi_dp_items]),
  MBI_PA = as.data.frame(d[, ..mbi_pa_items])
)
reliability <- rbindlist(lapply(names(scale_items), function(nm) {
  x <- as.data.frame(lapply(scale_items[[nm]], num)); complete_n <- sum(complete.cases(x))
  aa <- tryCatch(psych::alpha(x, check.keys = FALSE, warnings = FALSE, delete = FALSE), error = function(e) NULL)
  data.table(scale = nm, items = ncol(x), n_complete_items = complete_n,
             cronbach_alpha_raw = if (is.null(aa)) NA_real_ else aa$total$raw_alpha,
             cronbach_alpha_standardized = if (is.null(aa)) NA_real_ else aa$total$std.alpha,
             note = ifelse(grepl("NMQ", nm), "Alpha is descriptive only because body regions are heterogeneous indicators.",
                           ifelse(nm == "PSQI", "Alpha calculated across the seven PSQI components.", "Dictionary-scored items; no post-hoc item deletion.")))
}))

score_rules <- data.table(
  scale = c("NMQ_7d_count","NMQ_limit_count","PSQI","ESS","Physical_fatigue","Mental_fatigue","PHQ9","GAD7","PSS","MBI_EE","MBI_DP","MBI_PA"),
  variable = c("NMQ_7d_count","NMQ_limit_count","PSQI","ESS","physical_fatigue","mental_fatigue","PHQ9","GAD7","PSS","MBI_EE","MBI_DP","MBI_PA"),
  theoretical_min = c(0,0,0,0,0,0,0,0,0,0,0,0), theoretical_max = c(9,9,21,24,8,6,27,21,40,54,30,48),
  direction = c(rep("Higher = greater burden",11), "Higher = better personal accomplishment")
)
scale_quality <- score_rules[, {
  z <- num(d[[variable]]); valid <- z[is.finite(z)]
  .(n_valid = length(valid), missing_n = sum(!is.finite(z)), missing_pct = 100 * mean(!is.finite(z)),
    mean = mean(valid), sd = sd(valid), median = median(valid), q1 = quantile(valid,.25), q3 = quantile(valid,.75),
    min = min(valid), max = max(valid), skewness = psych::skew(valid), kurtosis = psych::kurtosi(valid),
    floor_pct = 100 * mean(valid == theoretical_min), ceiling_pct = 100 * mean(valid == theoretical_max),
    illegal_n = sum(valid < theoretical_min | valid > theoretical_max))
}, by = .(scale, variable, theoretical_min, theoretical_max, direction)]
scale_quality <- merge(scale_quality, reliability[, .(scale, items, n_complete_items, cronbach_alpha_raw, cronbach_alpha_standardized, note)], by = "scale", all.x = TRUE)
write_csv(scale_quality, "24_scale_quality_reliability.csv")
write_csv(reliability, "25_reliability_detail.csv")

# Unweighted and overlap-weighted Table 1.
table1_cont <- c("age", "work_years", "overtime_weeks", "disinfectant_index")
table1_cat <- c("sex", "first_education", "highest_education", "marital", "employment", "title", "night_frequency",
                "anesthetic_gas", "antineoplastic_drug", "antiviral_drug", "radiation")
labels <- c(age="Age, years", work_years="Work experience, years", overtime_weeks="Weeks >40 h in past month",
            disinfectant_index="Mean disinfectant-contact frequency score", sex="Sex", first_education="First education",
            highest_education="Highest education", marital="Marital status", employment="Employment type", title="Technical title",
            night_frequency="Night-shift frequency", anesthetic_gas="Anesthetic gas contact", antineoplastic_drug="Antineoplastic drug contact",
            antiviral_drug="Antiviral drug contact", radiation="Ionizing-radiation contact")

table1_unweighted <- list(); kk <- 1L
for (v in table1_cont) {
  z <- num(d[[v]]); skew <- psych::skew(z[is.finite(z)]); use_median <- abs(skew) > 1
  x0 <- z[d$exposure == 0 & is.finite(z)]; x1 <- z[d$exposure == 1 & is.finite(z)]
  tst <- if (use_median) wilcox.test(x1, x0, exact = FALSE) else t.test(x1, x0, var.equal = FALSE)
  sm <- smd_cont(z, d$exposure)
  disp <- function(x) if (use_median) sprintf("%.1f (%.1f-%.1f)", median(x), quantile(x,.25), quantile(x,.75)) else sprintf("%.1f (%.1f)", mean(x), sd(x))
  table1_unweighted[[kk]] <- data.table(variable=labels[v], level=ifelse(use_median,"Median (IQR)","Mean (SD)"),
                                         no_contact=disp(x0), ever_contact=disp(x1), p=tst$p.value,
                                         test=ifelse(use_median,"Mann-Whitney U","Welch t test"), smd=sm["smd"], variance_ratio=sm["vr"])
  kk <- kk + 1L
}
for (v in table1_cat) {
  f <- droplevels(d[[v]]); tab <- table(f, d$exposure)
  if (nrow(tab) < 2L || ncol(tab) < 2L) next
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  if (any(chi$expected < 5) && nrow(tab) == 2L) { p <- fisher.test(tab)$p.value; test <- "Fisher exact" }
  else if (any(chi$expected < 5)) { p <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)$p.value; test <- "Fisher exact (Monte Carlo)" }
  else { p <- chi$p.value; test <- "Pearson chi-square" }
  levs <- levels(f)
  if (v == "sex") levs <- "Female"
  for (i in seq_along(levs)) {
    lv <- levs[i]; b <- as.integer(f == lv); sm <- smd_binary(b, d$exposure)
    n0 <- sum(f[d$exposure==0] == lv, na.rm=TRUE); den0 <- sum(!is.na(f[d$exposure==0]))
    n1 <- sum(f[d$exposure==1] == lv, na.rm=TRUE); den1 <- sum(!is.na(f[d$exposure==1]))
    table1_unweighted[[kk]] <- data.table(variable=ifelse(i==1,labels[v],""), level=lv,
                                           no_contact=sprintf("%d (%.1f%%)",n0,100*n0/den0), ever_contact=sprintf("%d (%.1f%%)",n1,100*n1/den1),
                                           p=ifelse(i==1,p,NA_real_), test=ifelse(i==1,test,""), smd=sm["smd"], variance_ratio=NA_real_)
    kk <- kk + 1L
  }
}
table1_unweighted <- rbindlist(table1_unweighted, fill=TRUE)
write_csv(table1_unweighted, "26_table1_unweighted.csv")

# Propensity score includes pre-exposure demographic/professional factors and schedule opportunity variables.
ps_covariates <- c("age","work_years","sex","first_education","highest_education","marital","employment","title","night_frequency","overtime_weeks")
ps_data0 <- droplevels(d[complete.cases(d[, c("exposure", ps_covariates), with=FALSE])])
ps_formula <- as.formula(paste("exposure ~", paste(ps_covariates, collapse=" + ")))
ps_fit0 <- glm(ps_formula, data=ps_data0, family=binomial())
ps_data0[, ps_initial := predict(ps_fit0, type="response")]
common_lower <- max(min(ps_data0[exposure==1,ps_initial]), min(ps_data0[exposure==0,ps_initial]))
common_upper <- min(max(ps_data0[exposure==1,ps_initial]), max(ps_data0[exposure==0,ps_initial]))
ps_data <- droplevels(ps_data0[ps_initial >= common_lower & ps_initial <= common_upper])
ps_fit <- glm(ps_formula, data=ps_data, family=binomial())
ps_data[, ps := predict(ps_fit,type="response")]
ps_data[, ow := ifelse(exposure==1,1-ps,ps)]

table1_weighted <- list(); kk <- 1L
for (v in c("age","work_years","overtime_weeks")) {
  sm <- smd_cont(num(ps_data[[v]]), ps_data$exposure, ps_data$ow)
  table1_weighted[[kk]] <- data.table(variable=labels[v], level="Weighted mean (SD)",
    no_contact=sprintf("%.2f (%.2f)",sm["m0"],sm["sd0"]), ever_contact=sprintf("%.2f (%.2f)",sm["m1"],sm["sd1"]),
    smd=sm["smd"], variance_ratio=sm["vr"]); kk <- kk + 1L
}
for (v in c("sex","first_education","highest_education","marital","employment","title","night_frequency")) {
  f <- droplevels(ps_data[[v]]); levs <- levels(f); if(v=="sex") levs <- "Female"
  for (i in seq_along(levs)) {
    lv <- levs[i]; sm <- smd_binary(as.integer(f==lv),ps_data$exposure,ps_data$ow)
    table1_weighted[[kk]] <- data.table(variable=ifelse(i==1,labels[v],""),level=lv,
      no_contact=sprintf("%.1f%%",100*sm["p0"]),ever_contact=sprintf("%.1f%%",100*sm["p1"]),smd=sm["smd"],variance_ratio=NA_real_); kk <- kk+1L
  }
}
table1_weighted <- rbindlist(table1_weighted,fill=TRUE)
write_csv(table1_weighted,"27_table1_overlap_weighted.csv")

ps_balance_summary <- data.table(
  metric=c("PS complete-case N","Common-support lower","Common-support upper","Discarded ever-contact nurses","Discarded no-contact nurses",
           "Overlap-weight ESS overall","Minimum weight","Maximum weight","Maximum absolute SMD after weighting","All absolute SMD <0.10"),
  value=c(nrow(ps_data0),common_lower,common_upper,sum(ps_data0$exposure==1)-sum(ps_data$exposure==1),sum(ps_data0$exposure==0)-sum(ps_data$exposure==0),
          sum(ps_data$ow)^2/sum(ps_data$ow^2),min(ps_data$ow),max(ps_data$ow),max(abs(table1_weighted$smd),na.rm=TRUE),
          as.numeric(all(abs(table1_weighted$smd)<0.10,na.rm=TRUE)))
)
write_csv(ps_balance_summary,"28_propensity_summary_extended.csv")

# Extended continuous and binary outcomes specified by the original analysis plan.
core_cov <- c("age","work_years","sex","first_education","highest_education","marital","employment","title")
schedule_cov <- c("night_frequency","overtime_weeks")
coexposure_cov <- c("anesthetic_gas","antineoplastic_drug","antiviral_drug","radiation","disinfectant_index")
rhs_core <- paste(c("exposure",core_cov),collapse=" + ")
rhs_schedule <- paste(c("exposure",core_cov,schedule_cov),collapse=" + ")
rhs_full <- paste(c("exposure",core_cov,schedule_cov,coexposure_cov),collapse=" + ")

continuous_map <- c(MBI_emotional_exhaustion="MBI_EE",MBI_depersonalization="MBI_DP",MBI_personal_accomplishment="MBI_PA",
                    Physical_fatigue="physical_fatigue",Mental_fatigue="mental_fatigue",PSQI="PSQI",ESS="ESS",PHQ9="PHQ9",GAD7="GAD7",PSS="PSS",
                    NMQ_7d_region_count="NMQ_7d_count",NMQ_limitation_region_count="NMQ_limit_count")
extended_continuous <- rbindlist(lapply(names(continuous_map),function(label){
  y <- unname(continuous_map[label]); vars <- unique(c(y,"exposure",core_cov,schedule_cov,coexposure_cov)); dd <- droplevels(d[complete.cases(d[,..vars])])
  fit <- lm(as.formula(paste(y,"~",rhs_core)),data=dd); st <- robust_stats(fit,"exposure"); gc <- linear_gcomp(fit,dd); ys <- sd(dd[[y]])
  data.table(outcome=label,n=nrow(dd),exposed_n=sum(dd$exposure==1),reference_n=sum(dd$exposure==0),
             adjusted_mean_no=gc["mean_no"],adjusted_mean_yes=gc["mean_yes"],mean_difference=st["estimate"],ci_lower=st["lower"],ci_upper=st["upper"],
             standardized_difference=st["estimate"]/ys,standardized_ci_lower=st["lower"]/ys,standardized_ci_upper=st["upper"]/ys,p=st["p"],
             adjusted_r_squared=summary(fit)$adj.r.squared)
}),fill=TRUE)
extended_continuous[,p_fdr:=p]
extended_continuous[outcome!="MBI_emotional_exhaustion",p_fdr:=p.adjust(p,method="BH")]
write_csv(extended_continuous,"29_extended_continuous_results.csv")

binary_map <- c(High_emotional_exhaustion="high_EE",High_depersonalization="high_DP",Low_personal_accomplishment="low_PA",
                High_burnout_any_dimension="high_burnout_any",Poor_sleep_PSQI_gt7="poor_sleep",Excessive_daytime_sleepiness_ESS_ge11="excessive_sleepiness",
                PHQ9_ge10="PHQ9_ge10",GAD7_ge10="GAD7_ge10",Any_7d_musculoskeletal_symptom="any_NMQ_7d",Any_activity_limitation="any_NMQ_limit")
extended_binary <- rbindlist(lapply(names(binary_map),function(label){
  y <- unname(binary_map[label]); vars <- unique(c(y,"exposure",core_cov)); dd <- droplevels(d[complete.cases(d[,..vars])])
  fit <- glm(as.formula(paste(y,"~",rhs_core)),data=dd,family=poisson(link="log")); st <- robust_stats(fit,"exposure"); gc <- poisson_gcomp(fit,dd)
  data.table(outcome=label,n=nrow(dd),events=sum(dd[[y]]==1),adjusted_prevalence_no=gc["prev_no"],adjusted_prevalence_yes=gc["prev_yes"],
             prevalence_ratio=exp(st["estimate"]),pr_ci_lower=exp(st["lower"]),pr_ci_upper=exp(st["upper"]),
             prevalence_difference=gc["prevalence_difference"],pd_ci_lower=gc["pd_lower"],pd_ci_upper=gc["pd_upper"],p=st["p"])
}),fill=TRUE)
extended_binary[,p_fdr:=p]
extended_binary[outcome!="High_emotional_exhaustion",p_fdr:=p.adjust(p,method="BH")]
write_csv(extended_binary,"30_extended_binary_results.csv")

# Corrected overlap-weighted outcome analyses (ATO) using validated age/work-experience derivations.
overlap_continuous_extended <- rbindlist(lapply(names(continuous_map),function(label){
  y <- unname(continuous_map[label]); dd <- ps_data[!is.na(get(y))]
  des <- svydesign(ids=~1,weights=~ow,data=dd); fit <- svyglm(as.formula(paste(y,"~exposure")),design=des,family=gaussian())
  cc <- summary(fit)$coefficients; est <- cc["exposure","Estimate"]; se <- cc["exposure","Std. Error"]; p <- cc["exposure",ncol(cc)]
  m0 <- weighted_mean_safe(dd[exposure==0][[y]],dd[exposure==0]$ow); m1 <- weighted_mean_safe(dd[exposure==1][[y]],dd[exposure==1]$ow); ys <- sd(dd[[y]])
  data.table(outcome=label,n=nrow(dd),weighted_mean_no=m0,weighted_mean_yes=m1,mean_difference=est,ci_lower=est-1.96*se,ci_upper=est+1.96*se,
             standardized_difference=est/ys,standardized_ci_lower=(est-1.96*se)/ys,standardized_ci_upper=(est+1.96*se)/ys,p=p)
}),fill=TRUE)
overlap_continuous_extended[,p_fdr:=p]
overlap_continuous_extended[outcome!="MBI_emotional_exhaustion",p_fdr:=p.adjust(p,method="BH")]
write_csv(overlap_continuous_extended,"41_overlap_weighted_continuous_extended.csv")

overlap_binary_extended <- rbindlist(lapply(names(binary_map),function(label){
  y <- unname(binary_map[label]); dd <- ps_data[!is.na(get(y))]
  des <- svydesign(ids=~1,weights=~ow,data=dd); fit <- svyglm(as.formula(paste(y,"~exposure")),design=des,family=quasipoisson(link="log"))
  cc <- summary(fit)$coefficients; est <- cc["exposure","Estimate"]; se <- cc["exposure","Std. Error"]; p <- cc["exposure",ncol(cc)]
  p0 <- weighted_mean_safe(dd[exposure==0][[y]],dd[exposure==0]$ow); p1 <- weighted_mean_safe(dd[exposure==1][[y]],dd[exposure==1]$ow)
  data.table(outcome=label,n=nrow(dd),weighted_prevalence_no=p0,weighted_prevalence_yes=p1,prevalence_ratio=exp(est),
             pr_ci_lower=exp(est-1.96*se),pr_ci_upper=exp(est+1.96*se),prevalence_difference=p1-p0,p=p)
}),fill=TRUE)
overlap_binary_extended[,p_fdr:=p]
overlap_binary_extended[outcome!="High_emotional_exhaustion",p_fdr:=p.adjust(p,method="BH")]
write_csv(overlap_binary_extended,"42_overlap_weighted_binary_extended.csv")

# Corrected 1:1 nearest-neighbour PSM sensitivity. A 1:3 match is infeasible because references are too few.
match_object <- matchit(ps_formula,data=ps_data0,method="nearest",distance="glm",ratio=1,replace=FALSE,caliper=.2,std.caliper=TRUE,estimand="ATT")
matched <- as.data.table(match.data(match_object)); matched_ee <- matched[!is.na(MBI_EE)]
mfit <- lm(MBI_EE~exposure,data=matched_ee,weights=weights); mvc <- vcovCL(mfit,cluster=matched_ee$subclass,type="HC1"); mcc <- coeftest(mfit,vcov.=mvc)
mest <- mcc["exposure",1]; mse <- mcc["exposure",2]; mp <- mcc["exposure",4]
mbfit <- glm(high_EE~exposure,data=matched_ee,weights=weights,family=poisson(link="log")); mbvc <- vcovCL(mbfit,cluster=matched_ee$subclass,type="HC1"); mbcc <- coeftest(mbfit,vcov.=mbvc)
Xmat <- model.matrix(ps_formula,matched)[,-1,drop=FALSE]
match_smd <- sapply(seq_len(ncol(Xmat)),function(j) smd_cont(Xmat[,j],matched$exposure,matched$weights)["smd"])
match_balance <- data.table(covariate=colnames(Xmat),smd_after=as.numeric(match_smd))
write_csv(match_balance,"43_psm_balance_corrected.csv")
matched_p0 <- weighted_mean_safe(matched_ee[exposure==0]$high_EE,matched_ee[exposure==0]$weights)
matched_p1 <- weighted_mean_safe(matched_ee[exposure==1]$high_EE,matched_ee[exposure==1]$weights)
corrected_psm <- rbindlist(list(
  data.table(outcome="MBI_emotional_exhaustion",effect_measure="Matched mean difference",estimate=mest,ci_lower=mest-1.96*mse,ci_upper=mest+1.96*mse,p=mp),
  data.table(outcome="High_emotional_exhaustion",effect_measure="Matched prevalence ratio",estimate=exp(mbcc["exposure",1]),
             ci_lower=exp(mbcc["exposure",1]-1.96*mbcc["exposure",2]),ci_upper=exp(mbcc["exposure",1]+1.96*mbcc["exposure",2]),p=mbcc["exposure",4])
),fill=TRUE)
corrected_psm[,`:=`(matched_exposed=sum(matched$exposure==1),matched_reference=sum(matched$exposure==0),
                    discarded_exposed=sum(ps_data0$exposure==1)-sum(matched$exposure==1),
                    discarded_reference=sum(ps_data0$exposure==0)-sum(matched$exposure==0),
                    max_abs_smd_after=max(abs(match_smd),na.rm=TRUE),ratio="1:1",caliper="0.2 SD of logit propensity score")]
corrected_psm[outcome=="MBI_emotional_exhaustion",standardized_difference:=mest/sd(matched_ee$MBI_EE)]
corrected_psm[outcome=="High_emotional_exhaustion",prevalence_difference:=matched_p1-matched_p0]
write_csv(corrected_psm,"44_corrected_psm_results.csv")


# Full coefficient tables for the primary continuous and binary outcomes.
full_vars <- unique(c("MBI_EE","high_EE","exposure",core_cov,schedule_cov,coexposure_cov))
primary_frame <- droplevels(d[complete.cases(d[,..full_vars])])
coef_table <- function(fit, model_name, exponentiate=FALSE) {
  cc <- coeftest(fit,vcov.=vcovHC(fit,type="HC3")); out <- data.table(term=rownames(cc),estimate=cc[,1],se=cc[,2],p=cc[,4])
  out[,`:=`(ci_lower=estimate-1.96*se,ci_upper=estimate+1.96*se,model=model_name)]
  if(exponentiate) out[,`:=`(effect=exp(estimate),effect_ci_lower=exp(ci_lower),effect_ci_upper=exp(ci_upper),effect_measure="Prevalence ratio")]
  else out[,`:=`(effect=estimate,effect_ci_lower=ci_lower,effect_ci_upper=ci_upper,effect_measure="Mean difference")]
  out[]
}
linear_fits <- list(Core=lm(as.formula(paste("MBI_EE~",rhs_core)),data=primary_frame),
                    Schedule=lm(as.formula(paste("MBI_EE~",rhs_schedule)),data=primary_frame),
                    Full_coexposure=lm(as.formula(paste("MBI_EE~",rhs_full)),data=primary_frame))
primary_linear_coefficients <- rbindlist(Map(function(f,nm)coef_table(f,nm,FALSE),linear_fits,names(linear_fits)),fill=TRUE)
write_csv(primary_linear_coefficients,"31_primary_linear_full_coefficients.csv")
binary_fits <- list(Core=glm(as.formula(paste("high_EE~",rhs_core)),data=primary_frame,family=poisson(link="log")),
                    Full_coexposure=glm(as.formula(paste("high_EE~",rhs_full)),data=primary_frame,family=poisson(link="log")))
primary_binary_coefficients <- rbindlist(Map(function(f,nm)coef_table(f,nm,TRUE),binary_fits,names(binary_fits)),fill=TRUE)
write_csv(primary_binary_coefficients,"32_primary_binary_full_coefficients.csv")

# Same-frame sequential models quantify explanatory attenuation without mediation claims.
seq_rhs <- list(Crude="exposure",Core=rhs_core,Schedule=rhs_schedule,Full_coexposure=rhs_full)
primary_sequence_continuous <- rbindlist(lapply(names(seq_rhs),function(nm){
  fit<-lm(as.formula(paste("MBI_EE~",seq_rhs[[nm]])),data=primary_frame);st<-robust_stats(fit,"exposure");gc<-linear_gcomp(fit,primary_frame)
  data.table(model=nm,n=nrow(primary_frame),adjusted_mean_no=gc["mean_no"],adjusted_mean_yes=gc["mean_yes"],mean_difference=st["estimate"],
             ci_lower=st["lower"],ci_upper=st["upper"],standardized_difference=st["estimate"]/sd(primary_frame$MBI_EE),p=st["p"],
             adjusted_r_squared=summary(fit)$adj.r.squared)
}))
core_diff<-primary_sequence_continuous[model=="Core",mean_difference]
primary_sequence_continuous[,attenuation_from_core_pct:=ifelse(model%in%c("Schedule","Full_coexposure"),100*(core_diff-mean_difference)/core_diff,NA_real_)]
write_csv(primary_sequence_continuous,"45_primary_sequential_continuous_corrected.csv")

primary_sequence_binary <- rbindlist(lapply(names(seq_rhs),function(nm){
  fit<-glm(as.formula(paste("high_EE~",seq_rhs[[nm]])),data=primary_frame,family=poisson(link="log"));st<-robust_stats(fit,"exposure");gc<-poisson_gcomp(fit,primary_frame)
  data.table(model=nm,n=nrow(primary_frame),adjusted_prevalence_no=gc["prev_no"],adjusted_prevalence_yes=gc["prev_yes"],
             prevalence_ratio=exp(st["estimate"]),pr_ci_lower=exp(st["lower"]),pr_ci_upper=exp(st["upper"]),
             prevalence_difference=gc["prevalence_difference"],pd_ci_lower=gc["pd_lower"],pd_ci_upper=gc["pd_upper"],p=st["p"])
}))
core_logpr<-log(primary_sequence_binary[model=="Core",prevalence_ratio])
primary_sequence_binary[,attenuation_log_pr_from_core_pct:=ifelse(model%in%c("Schedule","Full_coexposure"),100*(core_logpr-log(prevalence_ratio))/core_logpr,NA_real_)]
write_csv(primary_sequence_binary,"46_primary_sequential_binary_corrected.csv")

# Full unadjusted health-outcome distributions by exposure group.
outcome_descriptives_extended <- rbindlist(lapply(names(continuous_map),function(label){
  y<-unname(continuous_map[label]);d[,.(n=sum(!is.na(get(y))),missing_n=sum(is.na(get(y))),mean=mean(get(y),na.rm=TRUE),sd=sd(get(y),na.rm=TRUE),
    median=median(get(y),na.rm=TRUE),q1=quantile(get(y),.25,na.rm=TRUE),q3=quantile(get(y),.75,na.rm=TRUE),min=min(get(y),na.rm=TRUE),max=max(get(y),na.rm=TRUE)),by=exposure_group][,outcome:=label]
}),fill=TRUE)
setcolorder(outcome_descriptives_extended,c("outcome","exposure_group",setdiff(names(outcome_descriptives_extended),c("outcome","exposure_group"))))
write_csv(outcome_descriptives_extended,"47_outcome_descriptives_extended.csv")
binary_descriptives_extended <- rbindlist(lapply(names(binary_map),function(label){
  y<-unname(binary_map[label]);d[,.(n=sum(!is.na(get(y))),events=sum(get(y)==1,na.rm=TRUE),prevalence=mean(get(y),na.rm=TRUE)),by=exposure_group][,outcome:=label]
}),fill=TRUE)
setcolorder(binary_descriptives_extended,c("outcome","exposure_group","n","events","prevalence"))
write_csv(binary_descriptives_extended,"48_binary_descriptives_extended.csv")

# Corrected VIF/GVIF and E-value for the primary core model.
vraw<-car::vif(linear_fits$Core)
if(is.matrix(vraw)){
  vif_corrected<-data.table(term=rownames(vraw),GVIF=vraw[,"GVIF"],Df=vraw[,"Df"],GVIF_adjusted=vraw[,"GVIF"]^(1/(2*vraw[,"Df"])))
}else vif_corrected<-data.table(term=names(vraw),VIF=as.numeric(vraw))
write_csv(vif_corrected,"49_vif_corrected.csv")
core_pr<-primary_sequence_binary[model=="Core",prevalence_ratio];core_pr_low<-primary_sequence_binary[model=="Core",pr_ci_lower]
evalue_fun<-function(rr)ifelse(rr>=1,rr+sqrt(rr*(rr-1)),(1/rr)+sqrt((1/rr)*((1/rr)-1)))
evalue_corrected<-data.table(endpoint="High emotional exhaustion",adjusted_PR=core_pr,PR_ci_lower=core_pr_low,evalue_point=evalue_fun(core_pr),
                             evalue_ci_closest_to_null=ifelse(core_pr_low>1,evalue_fun(core_pr_low),1))
write_csv(evalue_corrected,"50_evalue_corrected.csv")

# Spearman correlation matrix and long form for health measures.
corr_vars <- unname(continuous_map)
corr_labels <- names(continuous_map)
cmat <- cor(as.data.frame(d[,..corr_vars]),use="pairwise.complete.obs",method="spearman")
dimnames(cmat) <- list(corr_labels,corr_labels)
corr_matrix <- data.table(outcome=rownames(cmat),as.data.frame(cmat,check.names=FALSE))
write_csv(corr_matrix,"33_spearman_correlation_matrix.csv")
corr_long <- rbindlist(lapply(seq_along(corr_vars),function(i) rbindlist(lapply(seq_along(corr_vars),function(j){
  a<-d[[corr_vars[i]]];b<-d[[corr_vars[j]]];keep<-is.finite(a)&is.finite(b)
  data.table(outcome1=corr_labels[i],outcome2=corr_labels[j],n=sum(keep),rho=if(sum(keep)>2)cor(a[keep],b[keep],method="spearman") else NA_real_)
}))))
write_csv(corr_long,"34_spearman_correlation_long.csv")

# Exploratory subgroup estimates and formal interaction tests for the primary outcome.
subgroup_core_cov <- c("age","work_years","sex","first_education_collapsed","highest_education_collapsed","marital_collapsed","employment_collapsed","title_collapsed")
subgroup_specs <- list(
  Sex=list(var="sex",cov=setdiff(subgroup_core_cov,"sex")),
  Night_shift=list(var="any_night_shift",cov=c(subgroup_core_cov,"overtime_weeks")),
  Age_group=list(var="age_group",cov=setdiff(subgroup_core_cov,"age"))
)
subgroup_rows <- list(); ii <- 1L; interaction_rows <- list(); jj <- 1L
for(nm in names(subgroup_specs)) {
  sp <- subgroup_specs[[nm]]; v <- sp$var; covs <- sp$cov
  vars <- unique(c("MBI_EE","exposure",v,covs)); dd <- droplevels(d[complete.cases(d[,..vars])])
  form_int <- as.formula(paste("MBI_EE ~ exposure *",v,"+",paste(covs,collapse=" + ")))
  fit_int <- lm(form_int,data=dd); inter_terms <- grep(paste0("^exposure:",v),names(coef(fit_int)),value=TRUE)
  if(length(inter_terms)==1L) pint <- robust_stats(fit_int,inter_terms,type="HC1")["p"]
  else if(length(inter_terms)>1L) {
    lh <- car::linearHypothesis(fit_int,inter_terms,vcov.=vcovHC(fit_int,type="HC1"),test="F")
    pint <- lh[nrow(lh),"Pr(>F)"]
  } else pint <- NA_real_
  interaction_rows[[jj]] <- data.table(subgroup=nm,variable=v,n=nrow(dd),interaction_terms=paste(inter_terms,collapse="; "),p_interaction=pint); jj<-jj+1L
  for(lv in levels(dd[[v]])) {
    ds <- droplevels(dd[get(v)==lv]); form_sub <- as.formula(paste("MBI_EE ~ exposure +",paste(covs,collapse=" + ")))
    fit_sub <- lm(form_sub,data=ds); st <- robust_stats(fit_sub,"exposure",type="HC1")
    subgroup_rows[[ii]] <- data.table(subgroup=nm,level=lv,n=nrow(ds),exposed_n=sum(ds$exposure==1),reference_n=sum(ds$exposure==0),
      mean_difference=st["estimate"],ci_lower=st["lower"],ci_upper=st["upper"],standardized_difference=st["estimate"]/sd(ds$MBI_EE),p=st["p"]);ii<-ii+1L
  }
}
subgroup_results <- rbindlist(subgroup_rows,fill=TRUE); interaction_tests <- rbindlist(interaction_rows,fill=TRUE)
interaction_tests[,p_interaction_fdr:=p.adjust(p_interaction,method="BH")]
subgroup_results <- merge(subgroup_results,interaction_tests[,.(subgroup,p_interaction,p_interaction_fdr)],by="subgroup",all.x=TRUE)
write_csv(subgroup_results,"35_subgroup_interaction_results.csv")

p_sub <- ggplot(subgroup_results,aes(x=mean_difference,y=interaction(subgroup,level,sep=": "))) +
  geom_vline(xintercept=0,linetype=2,colour="grey45") + geom_errorbar(aes(xmin=ci_lower,xmax=ci_upper),width=.15,colour="#3B6FB6") +
  geom_point(size=2.3,colour="#B13A3A") + labs(x="Adjusted mean difference in MBI emotional exhaustion (ever vs no contact)",y=NULL,
  title="Exploratory subgroup associations") + theme_minimal(base_size=10)
ggsave(file.path(output_dir,"Figure7_subgroup_forest.png"),p_sub,width=9,height=6,dpi=300)
ggsave(file.path(output_dir,"Figure7_subgroup_forest.pdf"),p_sub,width=9,height=6)

# Additional robustness analyses for the primary outcome.
robustness <- list(); rr <- 1L
add_lm_sens <- function(form, data, label) {
  fit <- lm(form,data=data); st <- robust_stats(fit,"exposure")
  data.table(analysis=label,n=nrow(model.frame(fit)),estimand="Adjusted mean difference",estimate=st["estimate"],ci_lower=st["lower"],ci_upper=st["upper"],p=st["p"])
}
robustness[[rr]] <- add_lm_sens(as.formula(paste("MBI_EE~",rhs_core)),primary_frame,"Core model, HC3 SE");rr<-rr+1L
robustness[[rr]] <- add_lm_sens(MBI_EE ~ exposure + ns(age,3) + ns(work_years,3) + sex + first_education + highest_education + marital + employment + title,
                                primary_frame,"Nonlinear age/work experience using natural splines");rr<-rr+1L
robustness[[rr]] <- add_lm_sens(MBI_EE_recalc ~ exposure + age + work_years + sex + first_education + highest_education + marital + employment + title,
                                primary_frame,"Dictionary item-recalculated MBI EE");rr<-rr+1L

rqfit <- suppressWarnings(rq(as.formula(paste("MBI_EE~",rhs_core)),data=primary_frame,tau=.5,method="fn"))
rqs <- suppressWarnings(summary(rqfit,se="boot",R=500,bsmethod="xy")$coefficients)
if("exposure"%in%rownames(rqs)) robustness[[rr]] <- data.table(analysis="Median regression (tau=0.50)",n=nrow(primary_frame),estimand="Adjusted median difference",
  estimate=rqs["exposure",1],ci_lower=rqs["exposure",1]-1.96*rqs["exposure",2],ci_upper=rqs["exposure",1]+1.96*rqs["exposure",2],p=rqs["exposure",4]);rr<-rr+1L

qlo <- quantile(ps_data$ow,.01); qhi <- quantile(ps_data$ow,.99); ps_data[,ow_trim:=pmin(qhi,pmax(qlo,ow))]
ow_primary <- overlap_continuous_extended[outcome=="MBI_emotional_exhaustion"]
robustness[[rr]] <- data.table(analysis="Overlap weighting after common-support restriction",n=ow_primary$n,estimand="ATO weighted mean difference",
  estimate=ow_primary$mean_difference,ci_lower=ow_primary$ci_lower,ci_upper=ow_primary$ci_upper,p=ow_primary$p);rr<-rr+1L
ow_dd <- ps_data[!is.na(MBI_EE)]; des_trim <- svydesign(ids=~1,weights=~ow_trim,data=ow_dd); owfit <- svyglm(MBI_EE~exposure,design=des_trim)
cc <- summary(owfit)$coefficients; est<-cc["exposure","Estimate"];se<-cc["exposure","Std. Error"]
robustness[[rr]] <- data.table(analysis="Overlap weights truncated at 1st/99th percentiles",n=nrow(ow_dd),estimand="ATO weighted mean difference",
  estimate=est,ci_lower=est-1.96*se,ci_upper=est+1.96*se,p=cc["exposure",ncol(cc)]);rr<-rr+1L

manager_vars <- c("MBI_EE","exposure",core_cov,"admin_role"); manager_frame <- droplevels(or_all[complete.cases(or_all[,..manager_vars])])
robustness[[rr]] <- add_lm_sens(MBI_EE ~ exposure + age + work_years + sex + first_education + highest_education + marital + employment + title + admin_role,
                                manager_frame,"Including nurse managers");rr<-rr+1L
female_frame <- droplevels(d[sex=="Female" & complete.cases(d[,c("MBI_EE","exposure",setdiff(core_cov,"sex")),with=FALSE])])
robustness[[rr]] <- add_lm_sens(MBI_EE ~ exposure + age + work_years + first_education + highest_education + marital + employment + title,
                                female_frame,"Female nurses only");rr<-rr+1L
robustness <- rbindlist(robustness,fill=TRUE)
write_csv(robustness,"36_additional_robustness_results.csv")

# Missing-data decision and analyses that cannot be validly completed.
required_model_vars <- unique(c("exposure",core_cov,schedule_cov,coexposure_cov,unname(continuous_map),unname(binary_map)))
missing_decision <- data.table(variable=required_model_vars,n_missing=sapply(required_model_vars,function(v)sum(is.na(d[[v]]))),
                               missing_pct=sapply(required_model_vars,function(v)100*mean(is.na(d[[v]]))))
missing_decision[,decision:=ifelse(missing_pct>5,"Multiple-imputation sensitivity indicated","Complete-case analysis acceptable; MI trigger not met")]
write_csv(missing_decision,"37_missing_data_decision.csv")

analysis_status <- data.table(
  analysis=c("Hospital-cluster robust SE / hospital random intercept","Within-hospital exact matching","Multiple imputation","Exposure time-window sensitivity",
             "Overlap-weight truncation","Female-only analysis","Managers-included analysis","Nonlinear covariate adjustment","Median regression"),
  status=c("Not estimable","Not estimable",ifelse(max(missing_decision$missing_pct)>5,"Completed/required","Not triggered"),"Not estimable",
           "Completed","Completed","Completed","Completed","Completed"),
  reason=c("No hospital ID, hospital name, center code, or province variable was confirmed; no field was guessed.",
           "Hospital identifier unavailable.","No primary model variable exceeded 5% missingness.",
           "E_q20 asks whether contact ever occurred at work but provides no date/frequency window.",
           "Overlap weights are bounded; 1st/99th percentile truncation added as sensitivity.","Prespecified sensitivity.","Prespecified sensitivity.",
           "Natural splines for age and work experience.","Robustness to non-normal residuals; estimates an adjusted median difference."))
write_csv(analysis_status,"38_analysis_status_and_limitations.csv")

# Primary model diagnostics extension.
fit_core <- linear_fits$Core; mf <- model.frame(fit_core); cooks <- cooks.distance(fit_core)
diag_extended <- data.table(
  metric=c("N","R_squared","Adjusted_R_squared","Maximum_Cooks_distance","Observations_Cooks_D_gt_4_over_n",
           "Outcome_skewness","Outcome_kurtosis","Residual_KS_p","Breusch_Pagan_p","Maximum_adjusted_GVIF"),
  value=c(nobs(fit_core),summary(fit_core)$r.squared,summary(fit_core)$adj.r.squared,max(cooks),sum(cooks>4/length(cooks)),
          psych::skew(mf$MBI_EE),psych::kurtosi(mf$MBI_EE),
          suppressWarnings(ks.test(as.numeric(scale(residuals(fit_core))),"pnorm")$p.value),bptest(fit_core)$p.value,
          max(if(is.matrix(car::vif(fit_core))) car::vif(fit_core)[,"GVIF"]^(1/(2*car::vif(fit_core)[,"Df"])) else car::vif(fit_core))))
write_csv(diag_extended,"39_primary_diagnostics_extended.csv")

# Model specification register prevents significance-driven changes.
model_register <- data.table(
  component=c("Design","Exposure","Primary outcome","Primary adjustment","Schedule adjustment","Coexposure adjustment","Propensity estimand","Multiplicity","Inference","Subgroup stability","Causal wording"),
  specification=c("Multicenter cross-sectional baseline analysis","E_q20: ever work-related blood/body-fluid contact, yes versus no",
                  "MBI emotional exhaustion, continuous; high EE >=27 as companion binary endpoint",
                  paste(core_cov,collapse=", "),paste(schedule_cov,collapse=", "),paste(coexposure_cov,collapse=", "),
                  "ATO using overlap weighting after common-support restriction","Benjamini-Hochberg FDR for secondary continuous and binary outcome families",
                  "HC3 robust SE for regression; survey robust SE for overlap weighting","Education and rare marital, employment, and title categories collapsed only in exploratory subgroup models to avoid sparse-cell instability",
                  "Associations/differences only; no causal, risk-factor, or longitudinal language"))
write_csv(model_register,"40_model_specification_register.csv")

# Compact manuscript-facing summary.
primary <- extended_continuous[outcome=="MBI_emotional_exhaustion"]
high <- extended_binary[outcome=="High_emotional_exhaustion"]
top <- extended_continuous[outcome!="MBI_emotional_exhaustion" & outcome!="MBI_personal_accomplishment"][order(-abs(standardized_difference))][1:5]
summary_lines <- c(
  "# Complete statistical analysis: OR nurses and occupational blood/body-fluid contact",
  "",paste0("Generated: ",Sys.Date()),"",
  sprintf("Main cohort: %d ordinary clinical operating-room nurses; %d ever contact and %d no contact (%.1f%% exposed).",nrow(d),sum(d$exposure==1),sum(d$exposure==0),100*mean(d$exposure==1)),
  sprintf("Core-adjusted MBI emotional-exhaustion difference: %.2f points (95%% CI %.2f to %.2f), %.3f SD; p=%s.",primary$mean_difference,primary$ci_lower,primary$ci_upper,primary$standardized_difference,fmt_p(primary$p)),
  sprintf("Core-adjusted high-emotional-exhaustion PR: %.3f (95%% CI %.3f to %.3f); adjusted prevalence difference %.1f percentage points.",high$prevalence_ratio,high$pr_ci_lower,high$pr_ci_upper,100*high$prevalence_difference),
  sprintf("After adding schedule and coexposure variables, the MBI-EE difference was %.2f points (95%% CI %.2f to %.2f), a %.1f%% attenuation from the core model.",
          primary_sequence_continuous[model=="Full_coexposure",mean_difference],primary_sequence_continuous[model=="Full_coexposure",ci_lower],
          primary_sequence_continuous[model=="Full_coexposure",ci_upper],primary_sequence_continuous[model=="Full_coexposure",attenuation_from_core_pct]),
  sprintf("Overlap weighting retained %d nurses after common-support restriction; ESS %.1f; maximum weighted |SMD| %.4f.",nrow(ps_data),sum(ps_data$ow)^2/sum(ps_data$ow^2),max(abs(table1_weighted$smd),na.rm=TRUE)),
  sprintf("Scale reliability: MBI emotional exhaustion alpha %.3f; PSS alpha %.3f; mental-fatigue alpha %.3f. The latter two should not be headline outcomes.",
          scale_quality[scale=="MBI_EE",cronbach_alpha_raw],scale_quality[scale=="PSS",cronbach_alpha_raw],scale_quality[scale=="Mental_fatigue",cronbach_alpha_raw]),
  "Largest secondary standardized differences (excluding personal accomplishment because higher is better):",
  paste0("- ",top$outcome,": ",sprintf("%.3f SD (95%% CI %.3f to %.3f)",top$standardized_difference,top$standardized_ci_lower,top$standardized_ci_upper)),
  "",
  "Interpretation: results show cross-sectional differences/associations. They do not establish that contact caused worse health.",
  "Hospital-level inference remains unavailable because no confirmed hospital/center/province variable was found; this is the main submission limitation.",
  "The E_q20 item has no explicit timing or frequency, so dose-response and temporal-window analyses are not possible."
)
writeLines(summary_lines,file.path(output_dir,"complete_statistics_summary.md"),useBytes=TRUE)

capture.output(sessionInfo(),file=file.path(output_dir,"sessionInfo_full_statistics.txt"))
cat("Complete supplementary statistics finished.\n")
