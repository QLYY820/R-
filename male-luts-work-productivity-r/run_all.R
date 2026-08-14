#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) return(default)
  args[idx + 1L]
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
root_env <- Sys.getenv("LUTS_PIPELINE_ROOT", unset = "")
PIPELINE_ROOT <- if (nzchar(root_env)) {
  normalizePath(root_env, winslash = "/", mustWork = TRUE)
} else if (length(script_arg) && basename(sub("^--file=", "", script_arg[1])) == "run_all.R") {
  normalizePath(dirname(sub("^--file=", "", script_arg[1])), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
Sys.setenv(LUTS_PIPELINE_ROOT = PIPELINE_ROOT)
source(file.path(PIPELINE_ROOT, "config", "config.R"), encoding = "UTF-8")

env_mode <- Sys.getenv("LUTS_RUN_MODE", unset = CFG$run_mode)
mode_arg <- arg_value("--mode", env_mode)
if (!mode_arg %in% c("test", "production")) stop("--mode must be test or production")
CFG$run_mode <- mode_arg
env_data <- Sys.getenv("LUTS_DATA_PATH", unset = "")
CFG$data_override <- arg_value("--data", if (nzchar(env_data)) env_data else NULL)
assign("CFG", CFG, envir = .GlobalEnv)
assign("PIPELINE_ROOT", PIPELINE_ROOT, envir = .GlobalEnv)
source(file.path(PIPELINE_ROOT, "R", "00_utils.R"), encoding = "UTF-8")
ensure_dirs()
writeLines(character(), out_path("outputs", "analysis_run_log.txt"), useBytes = TRUE)

set.seed(CFG$random_seed)
log_msg("INFO", "Pipeline version ", CFG$pipeline_version, " started; mode=", CFG$run_mode,
        "; seed=", CFG$random_seed)
log_msg("INFO", "No hospital-level model will be fitted because no hospital identifier is configured.")

scripts <- c("01_audit_scales.R", "02_lca.R", "03_association_continuous.R",
             "04_tables_figures.R")
if (isTRUE(CFG$generate_manuscript)) scripts <- c(scripts, "05_documents_and_manifest.R")

status <- 0L
tryCatch({
  for (s in scripts) {
    log_msg("INFO", "Running ", s)
    source(file.path(PIPELINE_ROOT, "R", s), encoding = "UTF-8", chdir = FALSE)
    log_msg("INFO", "Completed ", s)
  }
}, error = function(e) {
  status <<- 1L
  log_msg("ERROR", conditionMessage(e))
})

if (status == 0L && !isTRUE(CFG$generate_manuscript)) {
  no_doc_report <- c(
    "# 最终流程审计（无Word模式）",
    "",
    paste0("- 运行模式：", CFG$run_mode),
    paste0("- 流水线版本：", CFG$pipeline_version),
    paste0("- 固定随机种子：", CFG$random_seed),
    "- 数据审计、量表审计、1～8类LCA、关联分析、连续LUTS对照分析、敏感性分析、Excel表格及投稿图片均已由代码运行。",
    "- 当前服务器缺少officer/flextable/ragg所需的系统字体和图像库，因此本次按配置跳过DOCX生成。",
    "- 跳过DOCX不会改变任何统计估计；可将输出目录带回具备文稿依赖的环境后单独生成Word文稿。",
    "- 当前身份与男性模块路由仍须按审计结果解释；测试模式结果禁止直接投稿。"
  )
  write_lines_utf8(no_doc_report, out_path("outputs", "FINAL_PIPELINE_AUDIT_CHINESE.md"))
  file.copy(
    out_path("outputs", "FINAL_PIPELINE_AUDIT_CHINESE.md"),
    out_path("FINAL_PIPELINE_AUDIT_CHINESE.md"),
    overwrite = TRUE
  )
  log_msg("INFO", "Word generation skipped by LUTS_GENERATE_MANUSCRIPT=false; statistical outputs are complete.")
}

writeLines(capture.output(sessionInfo()), out_path("outputs", "sessionInfo.txt"), useBytes = TRUE)
file.copy(out_path("outputs", "sessionInfo.txt"), out_path("09_logs", "sessionInfo.txt"), overwrite = TRUE)
if (status == 0L) {
  tryCatch({
    log_msg("INFO", "Running 06_validate.R")
    source(file.path(PIPELINE_ROOT, "R", "06_validate.R"), local = globalenv(), encoding = "UTF-8")
  }, error = function(e) {
    log_msg("ERROR", conditionMessage(e))
    status <<- 1L
  })
}
if (status != 0L) {
  if (interactive()) stop("Pipeline stopped. Review outputs/analysis_run_log.txt.", call. = FALSE)
  quit(save = "no", status = status)
}
log_msg("INFO", "Pipeline completed successfully with exit code 0.")
invisible(status)
