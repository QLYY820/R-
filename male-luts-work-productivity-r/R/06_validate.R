source(file.path(PIPELINE_ROOT, "R", "00_utils.R"), encoding = "UTF-8")
library(data.table)

r_files <- unique(c(
  list.files(out_path("R"), pattern = "\\.R$", full.names = TRUE),
  list.files(PIPELINE_ROOT, pattern = "\\.R$", full.names = TRUE),
  out_path("config", "config.R")
))
syntax <- rbindlist(lapply(r_files, function(f) {
  err <- tryCatch({parse(file = f, encoding = "UTF-8"); ""}, error = function(e) conditionMessage(e))
  data.table(file = normalizePath(f, winslash = "/"), parse_ok = !nzchar(err), error = err)
}))
write_csv_utf8(syntax, out_path("09_logs", "R_syntax_validation.csv"))

required <- c(
  "outputs/LUTS_work_productivity_working_draft_revised.docx",
  "outputs/Supplementary_Tables_LUTS.xlsx", "outputs/Supplementary_Methods_LUTS.docx",
  "outputs/LCA_model_selection_full.csv", "outputs/LCA_local_dependence_full.csv",
  "outputs/SPS6_scoring_audit.xlsx", "outputs/sex_module_linkage_audit.xlsx",
  "outputs/sensitivity_analysis_full.xlsx", "outputs/continuous_LUTS_spline_results.xlsx",
  "outputs/analysis_run_log.txt", "outputs/sessionInfo.txt", "outputs/FINAL_PIPELINE_AUDIT_CHINESE.md",
  "06_tables/Table1_basic_characteristics.csv", "06_tables/Table2_LCA_core_fit.csv",
  "06_tables/Table3_LCA_SPS6_association.csv",
  "07_figures/Figure1_sample_flow.pdf", "07_figures/Figure1_sample_flow.png", "07_figures/Figure1_sample_flow.tiff",
  "07_figures/Figure2_LCA_profile_heatmap.pdf", "07_figures/Figure2_LCA_profile_heatmap.png", "07_figures/Figure2_LCA_profile_heatmap.tiff",
  "07_figures/Figure3_continuous_LUTS_spline.pdf", "07_figures/Figure3_continuous_LUTS_spline.png", "07_figures/Figure3_continuous_LUTS_spline.tiff"
)
paths <- out_path(required)
artifacts <- data.table(file = required, exists = file.exists(paths), size_bytes = file.info(paths)$size)
artifacts[, nonempty := exists & !is.na(size_bytes) & size_bytes > 0]
write_csv_utf8(artifacts, out_path("09_logs", "artifact_validation.csv"))

workbooks <- required[grepl("\\.xlsx$", required)]
wb_check <- rbindlist(lapply(workbooks, function(w) {
  p <- out_path(w)
  sheets <- tryCatch(openxlsx::getSheetNames(p), error = function(e) character())
  data.table(workbook = w, sheet_count = length(sheets), sheets = paste(sheets, collapse = ";"))
}))
write_csv_utf8(wb_check, out_path("09_logs", "workbook_validation.csv"))

# Exclude this validator from literal scans because the prohibited patterns are
# necessarily present below as audit definitions.
validator_file <- normalizePath(out_path("R", "06_validate.R"), winslash = "/")
audit_files <- r_files[normalizePath(r_files, winslash = "/") != validator_file]
code <- paste(vapply(audit_files, function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), character(1)), collapse = "\n")
hardcoded <- data.table(
  check = c("old_test_sample_literal", "old_regression_sample_literal", "hardcoded_selected_k_4", "hospital_model_call"),
  pattern = c("4,?455", "4,?378", "selected_k\\s*<-\\s*4", "\\b(glmer|lmer|cluster.*hospital|hospital.*fixed)\\b"),
  matches = c(length(gregexpr("4,?455", code, perl = TRUE)[[1]][gregexpr("4,?455", code, perl = TRUE)[[1]] > 0]),
              length(gregexpr("4,?378", code, perl = TRUE)[[1]][gregexpr("4,?378", code, perl = TRUE)[[1]] > 0]),
              length(gregexpr("selected_k\\s*<-\\s*4", code, perl = TRUE)[[1]][gregexpr("selected_k\\s*<-\\s*4", code, perl = TRUE)[[1]] > 0]),
              length(gregexpr("\\b(glmer|lmer|cluster.*hospital|hospital.*fixed)\\b", code, perl = TRUE, ignore.case = TRUE)[[1]][gregexpr("\\b(glmer|lmer|cluster.*hospital|hospital.*fixed)\\b", code, perl = TRUE, ignore.case = TRUE)[[1]] > 0]))
)
write_csv_utf8(hardcoded, out_path("09_logs", "hardcoded_result_audit.csv"))

if (any(!syntax$parse_ok)) stop("R syntax validation failed.")
if (any(!artifacts$nonempty)) stop("Required artifact missing or empty: ", paste(artifacts[!nonempty, file], collapse = ", "))
if (any(wb_check$sheet_count == 0)) stop("Workbook validation failed.")
if (any(hardcoded$matches > 0)) stop("Hardcoded result or prohibited hospital-model audit failed.")

manifest_files <- list.files(PIPELINE_ROOT, recursive = TRUE, full.names = TRUE)
manifest_files <- manifest_files[file.info(manifest_files)$isdir %in% FALSE]
manifest_files <- manifest_files[!grepl("/(_work|_rendered)/", normalizePath(manifest_files, winslash = "/"))]
manifest <- lapply(manifest_files, function(p) list(
  relative_path = substring(normalizePath(p, winslash = "/"), nchar(normalizePath(PIPELINE_ROOT, winslash = "/")) + 2L),
  size_bytes = unname(file.info(p)$size), modified_time = format(file.info(p)$mtime, "%Y-%m-%dT%H:%M:%S%z"),
  sha256 = hash_file(p)
))
result_manifest <- list(
  pipeline_version = CFG$pipeline_version, run_mode = CFG$run_mode, random_seed = CFG$random_seed,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  production_ready = CFG$run_mode == "production" && CFG$male_module_ownership_confirmed && CFG$sps6_scoring_confirmed_for_production,
  hospital_model_used = FALSE, files = manifest
)
jsonlite::write_json(result_manifest, out_path("outputs", "results_manifest.json"), pretty = TRUE,
                     auto_unbox = TRUE, na = "null", digits = 12)
log_msg("INFO", "Validation passed: ", nrow(syntax), " R/config files, ", nrow(artifacts),
        " required artifacts, ", nrow(wb_check), " workbooks.")
