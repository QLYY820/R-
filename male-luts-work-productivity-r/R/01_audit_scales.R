source(file.path(PIPELINE_ROOT, "R", "00_utils.R"), encoding = "UTF-8")
library(data.table)

vars <- CFG$variables
luts_cols <- vars$luts
sps6_cols <- vars$sps6
requested <- unique(c(vars$id, vars$sex, vars$luts, vars$sps6, unname(vars$covariates),
                      unname(vars$quality), vars$source_luts_total, vars$source_sps6_total))

data_object_available <- nzchar(CFG$data_object_name %||% "") &&
  exists(CFG$data_object_name, envir = .GlobalEnv, inherits = FALSE) &&
  is.data.frame(get(CFG$data_object_name, envir = .GlobalEnv, inherits = FALSE))

input_path <- NULL
input_label <- NULL
if (!is.null(CFG$data_override) && nzchar(CFG$data_override)) {
  input_path <- normalizePath(CFG$data_override, winslash = "/", mustWork = TRUE)
} else if (!data_object_available) {
  input_path <- normalizePath(resolve_path(CFG$data_file), winslash = "/", mustWork = TRUE)
}

if (data_object_available && is.null(input_path)) {
  raw_object <- data.table::as.data.table(get(CFG$data_object_name, envir = .GlobalEnv, inherits = FALSE))
  header <- names(raw_object)
  input_label <- paste0("RStudio Global Environment object: ", CFG$data_object_name)
} else {
  ext <- tolower(tools::file_ext(input_path))
  if (ext %in% c("xlsx", "xls")) {
    stop("Large XLSX input is not read directly. Load it as the R object named '",
         CFG$data_object_name, "', or convert it to CSV/RDS before running.")
  }
  if (ext == "rds") {
    raw_object <- data.table::as.data.table(readRDS(input_path))
    header <- names(raw_object)
  } else if (ext %in% c("csv", "gz", "txt", "tsv")) {
    header <- names(fread(input_path, nrows = 0L, check.names = FALSE, showProgress = FALSE))
  } else {
    stop("Supported inputs are an in-memory data.frame/data.table, CSV/CSV.GZ/TSV, or RDS. Received: ", input_path)
  }
  input_label <- input_path
}

present <- intersect(requested, header)
missing <- setdiff(requested, header)

gate(vars$id %in% present, "ID_MISSING", "Unique participant ID variable is absent.")
gate(vars$sex %in% present, "SEX_MISSING", "Sex variable A_q2 is absent.")
gate(all(vars$luts %in% present), "LUTS_ITEMS_MISSING",
     paste("Missing LUTS items:", paste(setdiff(vars$luts, present), collapse = ", ")))
gate(all(vars$sps6 %in% present), "SPS6_ITEMS_MISSING",
     paste("Missing SPS-6 items:", paste(setdiff(vars$sps6, present), collapse = ", ")))

if (exists("raw_object", inherits = FALSE)) {
  dt <- copy(raw_object[, ..present])
  rm(raw_object)
} else {
  dt <- fread(input_path, select = present, colClasses = "character", check.names = FALSE,
              na.strings = c("", "NA", "N/A", "NULL"), encoding = "UTF-8", showProgress = TRUE)
}
dt[] <- lapply(dt, norm_char)

if (isTRUE(CFG$enforce_expected_source_records)) {
  gate(nrow(dt) == CFG$expected_source_records, "SOURCE_RECORD_COUNT_MISMATCH",
       paste0("Expected ", CFG$expected_source_records, " source records but read ", nrow(dt), "."))
}

dictionary_path <- resolve_path(CFG$dictionary_file)
reference_docx <- resolve_path(CFG$reference_docx)

if (!vars$id %in% names(dt)) dt[, (vars$id) := paste0("TEST_ROW_", .I)]
id_vec <- dt[[vars$id]]
id_missing <- is.na(id_vec)
duplicate_members <- duplicated(id_vec) | duplicated(id_vec, fromLast = TRUE)
duplicate_ids <- unique(id_vec[duplicate_members & !id_missing])
id_summary <- data.table(
  metric = c("total_records", "unique_nonmissing_ids", "missing_id_records", "duplicate_unique_ids",
             "records_in_duplicate_id_groups", "records_retained_after_deduplication"),
  n = c(nrow(dt), uniqueN(id_vec[!id_missing]), sum(id_missing), length(duplicate_ids),
        sum(duplicate_members & !id_missing), nrow(dt) - sum(duplicated(id_vec) & !id_missing)),
  handling = c("counted", "counted", "test: row key; production: stop", "test: retain first; production: stop",
               "reported without exporting raw IDs", "deterministic first-record rule in test mode")
)
write_csv_utf8(id_summary, out_path("00_audit", "id_linkage_summary.csv"))
gate(sum(id_missing) == 0, "ID_VALUES_MISSING", "Some records have missing participant IDs.")
gate(length(duplicate_ids) == 0, "ID_DUPLICATED", "Duplicate participant IDs are present.")

if (CFG$run_mode == "test" && any(id_missing)) dt[id_missing, (vars$id) := paste0("TEST_ROW_", .I)]
if (CFG$run_mode == "test" && length(duplicate_ids)) dt <- dt[!duplicated(get(vars$id))]

# Repository-safe variable metadata are stored as CSV. A compatible XLSX
# dictionary is also accepted when the research team prefers the formal source.
if (!file.exists(dictionary_path)) stop("Variable dictionary is missing: ", dictionary_path)
if (tolower(tools::file_ext(dictionary_path)) == "csv") {
  dict_evidence <- fread(dictionary_path, encoding = "UTF-8")
  required_dictionary_columns <- c("questionnaire_number", "source_question", "variable",
    "item_type", "storage_type", "original_label", "original_options", "scoring",
    "instrument", "dimension", "direction", "interpretation")
  if (!all(required_dictionary_columns %in% names(dict_evidence))) {
    stop("Dictionary CSV is missing columns: ",
         paste(setdiff(required_dictionary_columns, names(dict_evidence)), collapse = ", "))
  }
  dict_evidence <- dict_evidence[variable %in% c(vars$sex, vars$luts, vars$sps6),
                                 ..required_dictionary_columns]
} else {
  dict <- tryCatch(
    openxlsx::read.xlsx(dictionary_path, sheet = 1, colNames = FALSE,
                        skipEmptyRows = FALSE, skipEmptyCols = FALSE),
    error = function(e) as.data.frame(readxl::read_excel(dictionary_path, sheet = 1, col_names = FALSE))
  )
  setDT(dict)
  setnames(dict, paste0("X", seq_len(ncol(dict))))
  while (ncol(dict) < 12) dict[, paste0("X", ncol(dict) + 1L) := NA_character_]
  dict_rows <- dict[as.character(X3) %in% c(vars$sex, vars$luts, vars$sps6)]
  dict_evidence <- dict_rows[, .(
    questionnaire_number = X1, source_question = X2, variable = X3, item_type = X4,
    storage_type = X5, original_label = X6, original_options = X7, scoring = X8,
    instrument = X9, dimension = X10, direction = X11, interpretation = X12
  )]
}
write_csv_utf8(dict_evidence, out_path("00_audit", "dictionary_evidence.csv"))

for (v in intersect(c(vars$luts, vars$sps6, unname(vars$covariates), unname(vars$quality),
                      vars$source_luts_total, vars$source_sps6_total), names(dt))) {
  if (v != vars$covariates[["chronic"]]) set(dt, j = v, value = num(dt[[v]]))
}
if (vars$sex %in% names(dt)) set(dt, j = vars$sex, value = norm_char(dt[[vars$sex]]))

luts_mat <- as.matrix(dt[, ..luts_cols])
valid_luts_matrix <- !is.na(luts_mat) &
  matrix(luts_mat %in% 0:4, nrow = nrow(luts_mat), ncol = ncol(luts_mat))
module_valid_count <- rowSums(valid_luts_matrix)
module_any <- rowSums(!is.na(luts_mat)) > 0
module_complete <- rowSums(!is.na(luts_mat)) == length(vars$luts)
module_valid_complete <- module_valid_count == length(vars$luts)

sex_module <- dt[, .(
  records = .N,
  any_male_module = sum(module_any),
  complete_13_items = sum(module_complete),
  valid_complete_13_items = sum(module_valid_complete),
  mean_valid_items = mean(module_valid_count),
  min_valid_items = min(module_valid_count), max_valid_items = max(module_valid_count)
), by = .(sex_code = get(vars$sex))]
sex_module[is.na(sex_code), sex_code := "<missing>"]
write_csv_utf8(sex_module, out_path("00_audit", "sex_module_crosstab.csv"))

module_sex <- table(factor(dt[[vars$sex]], levels = c("1", "2")), module_valid_complete,
                    useNA = "ifany")
male_complete <- sum(dt[[vars$sex]] == CFG$male_code & module_valid_complete, na.rm = TRUE)
female_complete <- sum(dt[[vars$sex]] != CFG$male_code & module_valid_complete & !is.na(dt[[vars$sex]]), na.rm = TRUE)
missing_sex_complete <- sum(is.na(dt[[vars$sex]]) & module_valid_complete)

linkage_status <- data.table(
  check = c("unique_id_exists", "id_unique", "same_row_linkage_possible", "sex_dictionary_coding_confirmed",
            "male_code_in_config", "male_module_complete_with_male_code", "module_complete_with_nonmale_code",
            "module_complete_with_missing_sex", "module_ownership_confirmed_for_production"),
  value = c(vars$id %in% names(dt), length(duplicate_ids) == 0 && !any(id_missing), TRUE,
            CFG$sex_code_confirmed, CFG$male_code, male_complete, female_complete,
            missing_sex_complete, CFG$male_module_ownership_confirmed),
  interpretation = c("ID variable available", "ID-level row linkage check", "LUTS, SPS-6 and covariates coexist in one wide-table row",
                     "Dictionary states A_q2: 1=male, 2=female", "Configured male code",
                     "Expected production population", "Sex-module routing contradiction if >0",
                     "Incomplete identity evidence", "Must be TRUE for production inference")
)
write_csv_utf8(linkage_status, out_path("00_audit", "id_linkage_audit.csv"))

gate(CFG$unique_id_mapping_confirmed, "ID_MAPPING_NOT_CONFIRMED", "ID linkage has not been confirmed in config.")
gate(CFG$sex_code_confirmed, "SEX_CODE_NOT_CONFIRMED", "A_q2 coding has not been confirmed in config.")
gate(CFG$male_module_ownership_confirmed, "MODULE_OWNERSHIP_NOT_CONFIRMED",
     "Male-module ownership has not been confirmed against the real export.")
gate(female_complete == 0 && missing_sex_complete == 0 && male_complete > 0,
     "SEX_MODULE_CONTRADICTION",
     paste0("Among valid complete male-module records, male-coded n=", male_complete,
            ", nonmale-coded n=", female_complete, ", missing-sex n=", missing_sex_complete, "."))

# In test mode the analytic population is defined neutrally by valid module completion.
sub <- copy(dt[module_valid_complete])
if (!nrow(sub)) stop("No valid complete 13-item LUTS module records.")
sub[, analysis_key := vapply(get(vars$id), function(z) digest::digest(
  paste0("luts-pipeline-v2|", z), algo = "sha256", serialize = FALSE), character(1))]
sub[, (vars$id) := NULL]

longest_same_run <- function(z) if (anyNA(z)) NA_integer_ else max(rle(z)$lengths)
luts_sub <- as.matrix(sub[, ..luts_cols])
sub[, luts_all_same := apply(luts_sub, 1, function(z) length(unique(z)) == 1L)]
sub[, luts_all_zero := apply(luts_sub, 1, function(z) all(z == 0))]
sub[, luts_within_sd_zero := apply(luts_sub, 1, stats::sd) == 0]
sub[, luts_longest_same_run := apply(luts_sub, 1, longest_same_run)]

q <- vars$quality
sub[, start_work_age_candidate := get(q[["work_start_year"]]) - get(q[["birth_year"]])]
sub[, nursing_years_candidate := get(q[["survey_year"]]) - get(q[["work_start_year"]])]
duration_cols <- intersect(unname(q[grep("duration", names(q))]), names(sub))
duration_matrix <- as.matrix(sub[, ..duration_cols])
sub[, survey_duration_sum_candidate := ifelse(rowSums(!is.na(duration_matrix)) == 0, NA_real_,
                                               rowSums(duration_matrix, na.rm = TRUE))]
sub[, flag_age_extreme := is.na(get(vars$covariates[["age"]])) |
      get(vars$covariates[["age"]]) < 18 | get(vars$covariates[["age"]]) > 70]
sub[, flag_bmi_extreme := is.na(get(vars$covariates[["bmi"]])) |
      get(vars$covariates[["bmi"]]) < 12 | get(vars$covariates[["bmi"]]) > 60]
sub[, flag_start_age_lt16 := !is.na(start_work_age_candidate) & start_work_age_candidate < 16]
sub[, flag_work_years_lt1 := !is.na(nursing_years_candidate) & nursing_years_candidate < 1]
sub[, flag_duration_lt600 := !is.na(survey_duration_sum_candidate) & survey_duration_sum_candidate < 600]
sub[, flag_straightline_nonzero := luts_all_same & !luts_all_zero]
sub[, quality_flag_any := flag_age_extreme | flag_bmi_extreme | flag_start_age_lt16 |
      flag_work_years_lt1 | flag_duration_lt600 | flag_straightline_nonzero]

quality_flags <- data.table(
  flag = c("age_extreme_or_missing", "bmi_extreme_or_missing", "start_work_age_lt16",
           "nursing_years_lt1", "survey_duration_lt600", "all_13_same_nonzero",
           "all_13_zero_retained", "within_person_sd_zero", "any_quality_flag"),
  n = c(sum(sub$flag_age_extreme), sum(sub$flag_bmi_extreme), sum(sub$flag_start_age_lt16),
        sum(sub$flag_work_years_lt1), sum(sub$flag_duration_lt600),
        sum(sub$flag_straightline_nonzero), sum(sub$luts_all_zero),
        sum(sub$luts_within_sd_zero), sum(sub$quality_flag_any))
)
quality_flags[, percent := safe_pct(n, nrow(sub))]
write_csv_utf8(quality_flags, out_path("01_data", "response_quality_flags.csv"))

range_checks <- rbindlist(lapply(vars$luts, function(v) data.table(
  variable = v, label = unname(CFG$luts_labels_zh[v]), valid_n = sum(sub[[v]] %in% 0:4, na.rm = TRUE),
  missing_n = sum(is.na(sub[[v]])), out_of_range_n = sum(!is.na(sub[[v]]) & !sub[[v]] %in% 0:4),
  min = min(sub[[v]], na.rm = TRUE), max = max(sub[[v]], na.rm = TRUE)
)))
write_csv_utf8(range_checks, out_path("01_data", "range_checks.csv"))

# SPS-6 scoring audit: dictionary scheme versus the alternative of no reversal.
raw_sps <- as.data.frame(sub[, ..sps6_cols])
current <- raw_sps
current[, 5:6] <- 6 - current[, 5:6]
alternative <- raw_sps
names(current) <- paste0(vars$sps6, "_scored")
names(alternative) <- paste0(vars$sps6, "_scored")

score_scheme <- function(mat, scheme) {
  total <- rowSums(mat)
  a <- suppressWarnings(psych::alpha(mat, check.keys = FALSE, warnings = FALSE))
  rdrop <- a$item.stats$r.drop
  adel <- a$alpha.drop$raw_alpha
  item_rows <- rbindlist(lapply(seq_len(ncol(mat)), function(j) data.table(
    scheme = scheme, item = vars$sps6[j], question = unname(CFG$sps6_questions_zh[vars$sps6[j]]),
    mean = mean(mat[[j]], na.rm = TRUE), sd = sd(mat[[j]], na.rm = TRUE),
    corrected_item_total_r = rdrop[j], alpha_if_deleted = adel[j]
  )))
  r56 <- suppressWarnings(cor(mat[[5]], mat[[6]], use = "pairwise.complete.obs"))
  summary <- data.table(
    scheme = scheme, n = sum(complete.cases(mat)), alpha = a$total$raw_alpha,
    omega_total = omega_safe(mat), work_limitations_alpha = alpha_safe(mat[, 1:4]),
    work_limitations_omega = omega_safe(mat[, 1:4]), work_energy_alpha = alpha_safe(mat[, 5:6]),
    work_energy_spearman_brown = ifelse(is.finite(r56), 2 * r56 / (1 + r56), NA_real_),
    total_mean = mean(total), total_sd = sd(total), total_median = median(total),
    total_p25 = quantile(total, .25), total_p75 = quantile(total, .75),
    total_min = min(total), total_max = max(total), skewness = psych::skew(total),
    kurtosis = psych::kurtosi(total), floor_n = sum(total == 6), ceiling_n = sum(total == 30)
  )
  list(total = total, items = item_rows, summary = summary, correlation = cor(mat, use = "pairwise.complete.obs"))
}
audit_current <- score_scheme(current, "dictionary_reverse_items_5_6")
audit_alt <- score_scheme(alternative, "alternative_no_reverse")

source_total <- if (vars$source_sps6_total %in% names(sub)) sub[[vars$source_sps6_total]] else rep(NA_real_, nrow(sub))
agreement <- rbindlist(lapply(list(dictionary_reverse_items_5_6 = audit_current$total,
                                  alternative_no_reverse = audit_alt$total), function(total) {
  diff <- total - source_total
  data.table(source_total_nonmissing = sum(!is.na(source_total)), compared_n = sum(!is.na(diff)),
             exact_match_n = sum(diff == 0, na.rm = TRUE), exact_match_pct = safe_pct(sum(diff == 0, na.rm = TRUE), sum(!is.na(diff))),
             mismatch_n = sum(diff != 0, na.rm = TRUE), max_abs_difference = if (any(!is.na(diff))) max(abs(diff), na.rm = TRUE) else NA_real_)
}), idcol = "scheme")

freqs <- rbindlist(lapply(vars$sps6, function(v) {
  tab <- as.data.table(table(response = factor(sub[[v]], levels = 1:5), useNA = "ifany"))
  tab[, `:=`(item = v, question = unname(CFG$sps6_questions_zh[v]), percent = safe_pct(N, sum(N)))]
  setnames(tab, "N", "n")
  tab
}))
cor_long <- function(m, scheme) {
  x <- as.data.table(as.table(m))
  setnames(x, c("item1", "item2", "correlation"))
  x[, scheme := scheme]
  x
}
sps_item_audit <- rbind(audit_current$items, audit_alt$items)
sps_summary <- rbind(audit_current$summary, audit_alt$summary)
sps_cor <- rbind(cor_long(audit_current$correlation, "dictionary_reverse_items_5_6"),
                 cor_long(audit_alt$correlation, "alternative_no_reverse"))

selected_total <- if (CFG$sps6_confirmed_scheme == "dictionary_reverse_items_5_6") audit_current$total else audit_alt$total
sub[, sps6_total := selected_total]
sub[, luts_total := rowSums(.SD), .SDcols = vars$luts]
for (nm in names(CFG$luts_dimensions)) {
  sub[, (paste0("luts_", nm)) := rowSums(.SD), .SDcols = CFG$luts_dimensions[[nm]]]
}

gate(CFG$sps6_scoring_confirmed_for_production, "SPS6_SCHEME_NOT_CONFIRMED",
     "SPS-6 scoring has not been explicitly confirmed for production in config.R.")
gate(CFG$sps6_confirmed_scheme == "dictionary_reverse_items_5_6", "SPS6_SCHEME_CONFLICT",
     "Configured SPS-6 scheme differs from the source dictionary direction.")

source_evidence <- dict_evidence[variable %in% vars$sps6]
write_workbook(out_path("outputs", "SPS6_scoring_audit.xlsx"), list(
  `Source_dictionary` = source_evidence,
  `Raw_frequencies` = freqs,
  `Reliability_summary` = sps_summary,
  `Item_statistics` = sps_item_audit,
  `Correlation_long` = sps_cor,
  `Source_total_agreement` = agreement
), warning_banner = if (CFG$run_mode == "test") "TEST DATA WORKBOOK — NOT FOR SUBMISSION" else NULL)
file.copy(out_path("outputs", "SPS6_scoring_audit.xlsx"), out_path("02_scales", "SPS6_scoring_audit.xlsx"), overwrite = TRUE)

sps_md <- c(
  "# SPS-6计分审计（中文）", "",
  if (CFG$run_mode == "test") "> **测试数据工作稿，禁止投稿。**" else "",
  "## 问卷与存储方向", "",
  "量表字典显示，6个条目在数据中保存为1=完全不同意至5=完全同意的原始作答。第1～4题按原方向计分，第5～6题为正向能力表述，最终计分时反向转换为6-原始作答。",
  "因此，字典方案可避免漏反向；若数据源已经提前反向，重复执行6-x会造成二次反向。当前审计同时保留不反向备选方案，并通过原题、选项、存储方向和源总分一致性共同核对，不能按Cronbach α高低选择方案。", "",
  "## 正式模式关口", "",
  paste0("- 配置方案：`", CFG$sps6_confirmed_scheme, "`。"),
  paste0("- 正式确认开关：`", CFG$sps6_scoring_confirmed_for_production, "`。"),
  "- 正式数据运行前必须由研究团队根据原始中文问卷与导出逻辑确认该开关；否则流程停止。", "",
  "完整频数、相关矩阵、条目—总分相关、α、ω、两个维度信度、总分分布及源总分一致性见SPS6_scoring_audit.xlsx。"
)
write_lines_utf8(sps_md, out_path("02_scales", "SPS6_scoring_audit_Chinese.md"))

sex_audit_variables <- dict_evidence[variable %in% c(vars$sex, vars$luts)]
sex_summary <- rbind(id_summary[, .(section = "ID", item = metric, value = n, status = handling)],
                     linkage_status[, .(section = "Linkage", item = check, value, status = interpretation)], fill = TRUE)
write_workbook(out_path("outputs", "sex_module_linkage_audit.xlsx"), list(
  `Audit_summary` = sex_summary,
  `Sex_module_crosstab` = sex_module,
  `Dictionary_evidence` = sex_audit_variables
), warning_banner = if (CFG$run_mode == "test") "TEST MODE — SEX/MODULE CONTRADICTIONS ARE WARNINGS; PRODUCTION WOULD STOP" else NULL)
file.copy(out_path("outputs", "sex_module_linkage_audit.xlsx"), out_path("00_audit", "sex_module_linkage_audit.xlsx"), overwrite = TRUE)

sample_flow <- data.table(
  step = 1:7,
  stage = c("Source records", "Nonmissing unique ID after test handling", "Any male LUTS module response",
            "All 13 LUTS items nonmissing", "All 13 LUTS items valid 0-4", "SPS-6 complete", "Primary-model complete cases (pending)"),
  n = c(nrow(dt), uniqueN(dt[[vars$id]]), sum(module_any), sum(module_complete), nrow(sub),
        sum(complete.cases(sub[, ..sps6_cols])), NA_integer_),
  note = c("Records are not automatically treated as participants", "ID-level count", "Module routing audit",
           "Completeness only", "Test-mode analytic starting population", "Before covariate exclusions", "Filled by association script")
)
write_csv_utf8(sample_flow, out_path("00_audit", "sample_flow.csv"))

saveRDS(sub, out_path("01_data", "analysis_module_completers_deidentified.rds"), compress = "xz")
write_csv_utf8(quality_flags, out_path("01_data", "quality_flag_summary.csv"))
write_csv_utf8(rbindlist(list(sps_summary, agreement), fill = TRUE), out_path("02_scales", "sps6_audit_summary.csv"))

inventory_paths <- c(if (is.null(input_path)) NA_character_ else input_path,
                     dictionary_path, reference_docx)
inventory <- data.table(
  role = c("analysis_input", "source_dictionary", "reference_manuscript"),
  path = c(input_label, dictionary_path,
           if (is.na(reference_docx)) "<clean officer template>" else reference_docx),
  exists = c(data_object_available || (!is.null(input_path) && file.exists(input_path)),
             file.exists(dictionary_path), !is.na(reference_docx) && file.exists(reference_docx)),
  size_bytes = c(if (is.null(input_path)) NA_real_ else file.info(input_path)$size,
                 file.info(dictionary_path)$size,
                 if (is.na(reference_docx)) NA_real_ else file.info(reference_docx)$size),
  sha256 = c(if (is.null(input_path)) NA_character_ else hash_file(input_path),
             hash_file(dictionary_path), hash_file(reference_docx))
)
write_csv_utf8(inventory, out_path("00_audit", "existing_project_inventory.csv"))
log_msg("INFO", "Audit population: valid complete LUTS-module records n=", nrow(sub),
        "; male-coded n=", male_complete, "; nonmale-coded n=", female_complete)
