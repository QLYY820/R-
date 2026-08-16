# Analysis: Prepare the operating-room musculoskeletal co-occurrence dataset.
# Date: 2026-08-17
# Random seed: centralized in config/analysis_config.R
# R: 4.5+
# Key packages: data.table, openxlsx

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))
}
project_root <- normalizePath(Sys.getenv("OR_MSK_PROJECT_ROOT"), winslash = "/", mustWork = TRUE)
source(Sys.getenv("OR_MSK_CONFIG"))
source(file.path(project_root, "R", "date_utils.R"))
config <- build_analysis_config(
  project_root,
  mode = Sys.getenv("OR_MSK_MODE", "formal"),
  output_root = Sys.getenv("OR_MSK_OUTPUT_ROOT")
)
set.seed(config$seed)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript scripts/02_prepare_operating_room_data.R <clean_input.rds> <output_dir>")
}

input_path <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(args[2], winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (pkg in c("data.table", "openxlsx")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing package: ", pkg)
}

cat("R:", R.version.string, "\n")
cat("data.table:", as.character(utils::packageVersion("data.table")), "\n")
cat("openxlsx:", as.character(utils::packageVersion("openxlsx")), "\n")

raw <- readRDS(input_path)
if (!is.data.frame(raw) || nrow(raw) == 0L) stop("Input RDS must contain a nonempty data frame")
source_raw_rows <- nrow(raw)

first_present <- function(candidates, label) {
  present <- candidates[candidates %in% names(raw)]
  if (!length(present)) stop("Missing source column for ", label, "; tried: ", paste(candidates, collapse = ", "))
  present[[1L]]
}

id_variable <- first_present(config$variables$id_candidates, "coded participant ID")
response_time_variables <- config$variables$response_time_variables
core_scale_variables <- intersect(
  config$cohort_cleaning$core_scale_variables,
  names(raw)
)
if (!length(core_scale_variables)) stop("No configured parent-cohort core-scale columns are present")

required <- c(
  id_variable, config$variables$department, config$variables$survey_date,
  config$variables$birth_year, config$variables$work_start_year,
  config$variables$height_cm, config$variables$weight_kg,
  response_time_variables,
  "A_q2", "A_q5", "A_q6",
  "A_q10", "A_q11", "A_q12", "A_q13", "C_q1", "C_q6", "C_q7",
  "C_q8", "C_q9", "C_q10", "C_q24", "C_q42", "C_q44",
  paste0("E_q24_", rep(1:9, each = 4), "_", rep(1:4, 9))
)
missing_required <- setdiff(required, names(raw))
if (length(missing_required)) stop("Missing required columns: ", paste(missing_required, collapse = ", "))

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
is_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

# Reproduce the prespecified TARGET parent-cohort cleaning before selecting the
# operating-room subgroup. The submission time is intentionally `submittime`;
# the suffixed fields belong to separate merged survey modules.
id_text_raw <- trimws(as.character(raw[[id_variable]]))
nonblank_id <- !is_blank(id_text_raw)
duplicate_row <- rep(FALSE, nrow(raw))
duplicate_row[nonblank_id] <- duplicated(id_text_raw[nonblank_id])
duplicate_id_removed <- sum(duplicate_row)
raw <- raw[!duplicate_row, , drop = FALSE]

core_missing_matrix <- as.data.frame(lapply(
  raw[core_scale_variables],
  is_blank
))
core_missing_fraction <- rowMeans(core_missing_matrix)
core_bad <- core_missing_fraction > config$cohort_cleaning$maximum_core_missing_fraction
core_keep <- !core_bad

survey_year_all <- extract_submission_year(raw[[config$variables$survey_date]])
survey_year_all[
  !is.na(survey_year_all) &
    !survey_year_all %in% config$variables$survey_year_allowed
] <- NA_integer_
birth_year_all <- to_num(raw[[config$variables$birth_year]])
work_start_year_all <- to_num(raw[[config$variables$work_start_year]])
age_all <- survey_year_all - birth_year_all
work_years_all <- survey_year_all - work_start_year_all
nursing_entry_age_all <- work_start_year_all - birth_year_all

response_time_matrix <- as.data.frame(lapply(
  raw[response_time_variables],
  to_num
))
total_response_time_all <- rowSums(response_time_matrix, na.rm = TRUE)
total_response_time_all[rowSums(!is.na(response_time_matrix)) == 0L] <- NA_real_

work_years_bad <- !is.na(work_years_all) &
  work_years_all < config$cohort_cleaning$minimum_work_years
keep_after_work_years <- core_keep & !work_years_bad
nursing_entry_age_bad <- !is.na(nursing_entry_age_all) &
  nursing_entry_age_all < config$cohort_cleaning$minimum_nursing_entry_age
keep_after_entry_age <- keep_after_work_years & !nursing_entry_age_bad
response_time_bad <- !is.na(total_response_time_all) &
  total_response_time_all < config$cohort_cleaning$minimum_response_time_seconds
clean_keep <- keep_after_entry_age & !response_time_bad

cleaning_exclusions <- c(
  duplicate_id = duplicate_id_removed,
  core_missing = sum(core_bad),
  work_years = sum(core_keep & work_years_bad),
  nursing_entry_age = sum(keep_after_work_years & nursing_entry_age_bad),
  response_time = sum(keep_after_entry_age & response_time_bad)
)

clean_raw <- raw[clean_keep, , drop = FALSE]
clean_age <- age_all[clean_keep]
clean_work_years <- work_years_all[clean_keep]
clean_survey_year <- survey_year_all[clean_keep]
clean_height <- to_num(clean_raw[[config$variables$height_cm]])
clean_weight <- to_num(clean_raw[[config$variables$weight_kg]])
clean_height[
  clean_height < config$cohort_cleaning$valid_height_cm_range[[1L]] |
    clean_height > config$cohort_cleaning$valid_height_cm_range[[2L]]
] <- NA_real_
clean_weight[
  clean_weight < config$cohort_cleaning$valid_weight_kg_range[[1L]] |
    clean_weight > config$cohort_cleaning$valid_weight_kg_range[[2L]]
] <- NA_real_
clean_bmi <- clean_weight / (clean_height / 100)^2

if (source_raw_rows - sum(cleaning_exclusions) != nrow(clean_raw)) {
  stop("Parent-cohort sample-flow reconciliation failed")
}

department <- trimws(as.character(clean_raw[[config$variables$department]]))
operating_index <- !is.na(department) & department == config$variables$operating_room_code
operating_room <- clean_raw[operating_index, , drop = FALSE]
operating_age <- clean_age[operating_index]
operating_work_years <- clean_work_years[operating_index]
operating_bmi <- clean_bmi[operating_index]
operating_survey_year <- clean_survey_year[operating_index]
if (nrow(operating_room) == 0L) stop("No operating-room rows were found")

recode_yes_no <- function(x) {
  value <- trimws(as.character(x))
  out <- rep(NA_integer_, length(value))
  out[value %in% c("否", "1", "0", "No", "no")] <- 0L
  out[value %in% c("是", "2", "1.0", "Yes", "yes")] <- 1L
  out
}

site_map <- data.frame(
  site = config$variables$sites,
  label_cn = c("颈部", "肩部", "上背部", "肘部", "腕/手部", "下背部", "臀/股部", "膝部", "踝/足部"),
  label_en = c("Neck", "Shoulder", "Upper back", "Elbow", "Wrist/hand", "Lower back", "Hip/thigh", "Knee", "Ankle/foot"),
  source_index = 1:9,
  stringsAsFactors = FALSE
)

analysis <- data.frame(row_id = seq_len(nrow(operating_room)))
for (question in 1:4) {
  suffix <- c("symptom_12m", "activity_limit_12m", "healthcare_use_12m", "symptom_7d")[question]
  for (index in site_map$source_index) {
    source_name <- paste0("E_q24_", index, "_", question)
    target_name <- paste0(site_map$site[index], "_", suffix)
    analysis[[target_name]] <- recode_yes_no(operating_room[[source_name]])
  }
}

primary_vars <- paste0(site_map$site, "_symptom_12m")
analysis$multisite_burden_12m <- rowSums(analysis[primary_vars], na.rm = FALSE)

analysis$age <- operating_age
analysis$age[
  analysis$age < config$cohort_cleaning$valid_age_range[[1L]] |
    analysis$age > config$cohort_cleaning$valid_age_range[[2L]]
] <- NA_real_
analysis$work_years <- operating_work_years
analysis$BMI <- operating_bmi
analysis$sex <- factor(as.character(operating_room[["A_q2"]]), levels = c("2", "1"), labels = c("Female", "Male"))
analysis$education <- factor(as.character(operating_room[["A_q5"]]), levels = c("1", "2", "3", "4"), labels = c("Secondary", "College", "Bachelor", "Master_or_above"))
analysis$bachelor_or_above <- factor(ifelse(as.character(operating_room[["A_q5"]]) %in% c("3", "4"), "Yes", "No"), levels = c("No", "Yes"))
analysis$married <- factor(ifelse(as.character(operating_room[["A_q6"]]) %in% c("2", "5"), "Yes", "No"), levels = c("No", "Yes"))
analysis$permanent_employment <- factor(ifelse(as.character(operating_room[["A_q10"]]) == "1", "Yes", "No"), levels = c("No", "Yes"))
analysis$professional_title <- factor(as.character(operating_room[["A_q11"]]), levels = c("1", "2", "3", "4", "5", "6"), labels = c("Nurse", "Senior_nurse", "Supervisor_nurse", "Associate_chief", "Chief_nurse", "Other"))
analysis$supervisor_title_or_above <- factor(ifelse(as.character(operating_room[["A_q11"]]) %in% c("3", "4", "5"), "Yes", "No"), levels = c("No", "Yes"))
analysis$administrative_role <- factor(ifelse(as.character(operating_room[["A_q12"]]) == "1", "No", "Yes"), levels = c("No", "Yes"))
analysis$income_level <- ordered(as.character(operating_room[["A_q13"]]), levels = as.character(1:5))

shift_code <- as.character(operating_room[["C_q1"]])
analysis$shift_pattern <- factor(shift_code, levels = c("1", "2", "3", "4"), labels = c("Day_only", "Evening_only", "Night_only", "Rotating"))
analysis$any_night_shift <- factor(ifelse(shift_code == "1", "No", "Yes"), levels = c("No", "Yes"))
analysis$night_shift_years_category <- factor(ifelse(as.character(operating_room[["C_q6"]]) == "-3", NA, as.character(operating_room[["C_q6"]])), levels = as.character(1:5), labels = c("0_to_5", "6_to_10", "11_to_15", "16_to_20", "over_20"))
analysis$night_months_last_year <- ifelse(as.character(operating_room[["C_q7"]]) == "-3", NA_real_, 13 - to_num(operating_room[["C_q7"]]))
analysis$night_shifts_per_month <- factor(ifelse(as.character(operating_room[["C_q8"]]) == "-3", NA, as.character(operating_room[["C_q8"]])), levels = c("1", "2", "3"), labels = c("4_or_less", "5_to_9", "10_or_more"))
analysis$night_busy <- factor(ifelse(as.character(operating_room[["C_q24"]]) == "-3", NA, as.character(operating_room[["C_q24"]])), levels = as.character(1:5), labels = c("Very_busy", "Busy", "Average", "Relaxed", "Very_relaxed"))
analysis$overtime_weeks_last_month <- to_num(operating_room[["C_q42"]]) - 1
overlap_code <- as.character(operating_room[["C_q44"]])
analysis$work_sleep_overlap <- factor(ifelse(overlap_code == "-3", NA, ifelse(overlap_code == "1", "Yes", "No")), levels = c("No", "Yes"))

analysis$survey_year <- operating_survey_year
if (anyNA(analysis$survey_year)) {
  stop(
    "The operating-room survey year derived from ", config$variables$survey_date,
    " has values that are missing, unparseable, or outside ",
    paste(range(config$variables$survey_year_allowed), collapse = "-"),
    " (n=", sum(is.na(analysis$survey_year)), ")"
  )
}

id_text <- trimws(as.character(operating_room[[id_variable]]))
duplicate_id_n <- sum(duplicated(id_text[id_text != "" & !is.na(id_text)]))

wilson_ci <- function(events, total) {
  if (total == 0) return(c(NA_real_, NA_real_))
  interval <- suppressWarnings(stats::prop.test(events, total, correct = FALSE)$conf.int)
  c(max(0, interval[1]), min(1, interval[2]))
}

prevalence_rows <- lapply(seq_len(nrow(site_map)), function(index) {
  values <- analysis[[primary_vars[index]]]
  total <- sum(!is.na(values))
  events <- sum(values == 1, na.rm = TRUE)
  interval <- wilson_ci(events, total)
  data.frame(
    site = site_map$site[index], label_cn = site_map$label_cn[index], label_en = site_map$label_en[index],
    events = events, total = total, prevalence = events / total,
    ci_lower = interval[1], ci_upper = interval[2], missing = sum(is.na(values))
  )
})
site_prevalence <- do.call(rbind, prevalence_rows)

burden_values <- analysis$multisite_burden_12m
burden_distribution <- as.data.frame(table(burden_values, useNA = "ifany"), stringsAsFactors = FALSE)
names(burden_distribution) <- c("number_of_sites", "n")
burden_distribution$percent <- burden_distribution$n / nrow(analysis)

pattern_matrix <- analysis[primary_vars]
pattern_text <- apply(pattern_matrix, 1, function(row) {
  if (anyNA(row)) return(NA_character_)
  positive <- site_map$label_en[which(row == 1)]
  if (length(positive) == 0) "None" else paste(positive, collapse = " + ")
})
pattern_table <- as.data.frame(sort(table(pattern_text, useNA = "ifany"), decreasing = TRUE), stringsAsFactors = FALSE)
names(pattern_table) <- c("cooccurrence_pattern", "n")
pattern_table$percent <- pattern_table$n / nrow(analysis)
pattern_table <- utils::head(pattern_table, 25)

summary_vars <- c(primary_vars, "age", "work_years", "BMI", "sex", "education", "married", "permanent_employment", "professional_title", "administrative_role", "income_level", "shift_pattern", "any_night_shift", "night_shift_years_category", "night_months_last_year", "night_shifts_per_month", "night_busy", "overtime_weeks_last_month", "work_sleep_overlap")
missingness <- do.call(rbind, lapply(summary_vars, function(variable) {
  values <- analysis[[variable]]
  missing <- sum(is.na(values) | trimws(as.character(values)) == "")
  data.frame(variable = variable, missing_n = missing, missing_percent = missing / nrow(analysis), unique_nonmissing = length(unique(values[!is.na(values)])))
}))

sample_audit <- data.frame(
  metric = c(
    "source_raw_rows", "duplicate_id_rows_removed", "core_missing_excluded_rows",
    "work_years_lt_1_excluded_rows", "nursing_entry_age_lt_16_excluded_rows",
    "response_time_lt_600_excluded_rows", "source_clean_rows",
    "operating_room_rows", "operating_room_percent",
    "duplicate_coded_id_rows", "complete_primary_symptom_rows", "survey_year_missing_rows"
  ),
  value = c(
    source_raw_rows, cleaning_exclusions[["duplicate_id"]], cleaning_exclusions[["core_missing"]],
    cleaning_exclusions[["work_years"]], cleaning_exclusions[["nursing_entry_age"]],
    cleaning_exclusions[["response_time"]], nrow(clean_raw),
    nrow(analysis), nrow(analysis) / nrow(clean_raw), duplicate_id_n,
    sum(stats::complete.cases(analysis[primary_vars])), sum(is.na(analysis$survey_year))
  )
)

data.table::fwrite(analysis, file.path(output_dir, "operating_room_analysis_data.csv"), bom = TRUE)
saveRDS(analysis, file.path(output_dir, "operating_room_analysis_data.rds"))
data.table::fwrite(
  analysis[c("row_id", "survey_year")],
  file.path(output_dir, "operating_room_survey_year.csv"),
  bom = TRUE
)
data.table::fwrite(sample_audit, file.path(output_dir, "sample_audit.csv"), bom = TRUE)
data.table::fwrite(missingness, file.path(output_dir, "variable_missingness.csv"), bom = TRUE)
data.table::fwrite(site_prevalence, file.path(output_dir, "site_prevalence_12m.csv"), bom = TRUE)
data.table::fwrite(burden_distribution, file.path(output_dir, "multisite_burden_distribution.csv"), bom = TRUE)
data.table::fwrite(pattern_table, file.path(output_dir, "top_cooccurrence_patterns.csv"), bom = TRUE)

workbook <- openxlsx::createWorkbook()
for (sheet_name in c("sample_audit", "missingness", "site_prevalence", "burden_distribution", "top_patterns", "site_dictionary")) {
  openxlsx::addWorksheet(workbook, sheet_name)
}
openxlsx::writeData(workbook, "sample_audit", sample_audit)
openxlsx::writeData(workbook, "missingness", missingness)
openxlsx::writeData(workbook, "site_prevalence", site_prevalence)
openxlsx::writeData(workbook, "burden_distribution", burden_distribution)
openxlsx::writeData(workbook, "top_patterns", pattern_table)
openxlsx::writeData(workbook, "site_dictionary", site_map)
openxlsx::saveWorkbook(workbook, file.path(output_dir, "data_audit_and_descriptives.xlsx"), overwrite = TRUE)

cat("Source raw rows:", source_raw_rows, "\n")
cat(
  "Sequential exclusions:",
  paste(names(cleaning_exclusions), cleaning_exclusions, sep = "=", collapse = "; "),
  "\n"
)
cat("Source clean rows:", nrow(clean_raw), "\n")
cat("Operating-room rows:", nrow(analysis), "\n")
cat("Complete primary symptom rows:", sum(stats::complete.cases(analysis[primary_vars])), "\n")
cat("Duplicate coded IDs:", duplicate_id_n, "\n")
cat("Survey year source:", config$variables$survey_date, "\n")
cat("\nSite prevalence:\n")
print(site_prevalence)
cat("\nMissingness:\n")
print(missingness)
cat("STEP_COMPLETE=prepare_analysis_data\n")
