# Run this file first in RStudio. It performs only data/identity/scale audits;
# it does not fit the computationally intensive LCA models.

PIPELINE_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
Sys.setenv(LUTS_PIPELINE_ROOT = PIPELINE_ROOT)
source(file.path(PIPELINE_ROOT, "config", "config.R"), encoding = "UTF-8")
CFG$run_mode <- "test"
CFG$data_override <- Sys.getenv("LUTS_DATA_PATH", unset = "")
if (!nzchar(CFG$data_override)) CFG$data_override <- NULL
assign("CFG", CFG, envir = .GlobalEnv)
assign("PIPELINE_ROOT", PIPELINE_ROOT, envir = .GlobalEnv)
source(file.path(PIPELINE_ROOT, "R", "00_utils.R"), encoding = "UTF-8")
ensure_dirs()
writeLines(character(), out_path("outputs", "analysis_run_log.txt"), useBytes = TRUE)
set.seed(CFG$random_seed)
log_msg("INFO", "Preflight audit started; expected source records=", CFG$expected_source_records)
source(file.path(PIPELINE_ROOT, "R", "01_audit_scales.R"), encoding = "UTF-8")
writeLines(capture.output(sessionInfo()), out_path("outputs", "sessionInfo_preflight.txt"), useBytes = TRUE)
log_msg("INFO", "Preflight complete. Review outputs/sex_module_linkage_audit.xlsx and outputs/SPS6_scoring_audit.xlsx before production.")
