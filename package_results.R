transfer_dir <- file.path(PIPELINE_ROOT, "transfer")
dir.create(transfer_dir, recursive = TRUE, showWarnings = FALSE)

safe_extensions <- "\\.(csv|xlsx|png|pdf|txt|md|json)$"
files_abs <- list.files(
  RUN_ROOT,
  pattern = safe_extensions,
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
files_abs <- files_abs[!grepl("[/\\\\]_work[/\\\\]", files_abs)]
if (!length(files_abs)) stop("No aggregate result files were found to package.")

files_rel <- substring(
  normalizePath(files_abs, winslash = "/"),
  nchar(normalizePath(RUN_ROOT, winslash = "/")) + 2L
)
zip_name <- paste0(basename(RUN_ROOT), "_aggregate_results.zip")
TRANSFER_ZIP <- file.path(transfer_dir, zip_name)

if (!requireNamespace("zip", quietly = TRUE)) {
  stop("Package 'zip' is required to create the aggregate transfer archive.")
}
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(RUN_ROOT)
zip::zipr(
  zipfile = TRANSFER_ZIP,
  files = files_rel,
  recurse = TRUE,
  include_directories = FALSE,
  root = ".",
  mode = "mirror"
)
if (!file.exists(TRANSFER_ZIP) || file.info(TRANSFER_ZIP)$size <= 0) {
  stop("Aggregate transfer ZIP was not generated.")
}
message("Aggregate-only result ZIP created; raw data, IDs, and RDS objects excluded.")
