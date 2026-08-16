# Analysis: Import the fixed nurse-cohort XLSX with every column forced to text.
# Date: 2026-08-17
# Random seed: supplied by OR_MSK_SEED (default 42; no stochastic operations).
# Key package: readxl.

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Chinese"), silent = TRUE))
}
seed <- as.integer(Sys.getenv("OR_MSK_SEED", "42"))
set.seed(seed)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: Rscript R/00_import_data_text.R <raw_data.xlsx> <data_text.rds>")
}
raw_path <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
output_path <- args[[2L]]
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("readxl", quietly = TRUE)) stop("Missing package: readxl")

if (file.exists(output_path) && file.info(output_path)$size > 0) {
  existing <- readRDS(output_path)
  if (!is.data.frame(existing) || nrow(existing) == 0L || ncol(existing) == 0L) {
    stop("Existing data_text.rds is not a nonempty data frame: ", output_path)
  }
  if (!all(vapply(existing, is.character, logical(1)))) {
    stop("Existing data_text.rds contains non-text columns; do not overwrite it automatically")
  }
  cat("Reusing existing all-text RDS:", output_path, "\n")
  cat("Rows:", nrow(existing), "; columns:", ncol(existing), "\n")
  cat("STEP_COMPLETE=import_data_text_reused\n")
  quit(save = "no", status = 0L)
}

cat("Reading all XLSX columns as text from:", raw_path, "\n")
raw <- readxl::read_excel(raw_path, col_types = "text", .name_repair = "minimal", progress = TRUE)
raw <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
if (nrow(raw) == 0L || ncol(raw) == 0L) stop("The fixed source workbook is empty")
if (anyDuplicated(names(raw))) stop("Duplicate column names found in the fixed source workbook")
if (!all(vapply(raw, is.character, logical(1)))) stop("At least one imported column is not text")

temporary_path <- paste0(output_path, ".tmp_", Sys.getpid())
on.exit(if (file.exists(temporary_path)) unlink(temporary_path), add = TRUE)
saveRDS(raw, temporary_path, compress = TRUE)
if (!file.rename(temporary_path, output_path)) stop("Could not atomically create: ", output_path)

cat("Created all-text RDS:", output_path, "\n")
cat("Rows:", nrow(raw), "; columns:", ncol(raw), "\n")
cat("STEP_COMPLETE=import_data_text_created\n")
