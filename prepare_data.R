args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
if (length(file_arg)) {
  this_file <- normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
  pipeline_root <- dirname(this_file)
} else if (exists("PIPELINE_ROOT", inherits = TRUE)) {
  pipeline_root <- PIPELINE_ROOT
} else {
  pipeline_root <- normalizePath(getwd(), mustWork = TRUE)
}

if (!exists("CONFIG", inherits = TRUE)) {
  source(file.path(pipeline_root, "config.R"), encoding = "UTF-8")
}

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required. Run Rscript install_packages.R first.")
}

xlsx_path <- CONFIG$canonical_xlsx
rds_path <- CONFIG$data_path
if (!file.exists(xlsx_path)) {
  stop("Canonical bastion XLSX not found: ", xlsx_path)
}

message("Importing canonical XLSX with every column forced to text: ", xlsx_path)
data_text <- readxl::read_excel(
  path = xlsx_path,
  col_types = "text",
  .name_repair = "unique",
  progress = TRUE
)

if (nrow(data_text) != CONFIG$expected_source_n) {
  stop(
    "Record-count mismatch after XLSX import: observed ", nrow(data_text),
    ", expected ", CONFIG$expected_source_n
  )
}
if (!"id" %in% names(data_text)) stop("Required unique-ID column 'id' is absent.")
if (!"A_q2" %in% names(data_text)) stop("Required sex column 'A_q2' is absent.")

dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(data_text, rds_path, compress = FALSE)

audit_path <- file.path(dirname(rds_path), "data_text_import_audit.txt")
audit_lines <- c(
  paste0("source_xlsx=", normalizePath(xlsx_path, winslash = "/")),
  paste0("output_rds=", normalizePath(rds_path, winslash = "/")),
  paste0("records=", nrow(data_text)),
  paste0("columns=", ncol(data_text)),
  paste0("unique_nonmissing_id=", length(unique(data_text$id[!is.na(data_text$id) & data_text$id != ""]))),
  paste0("duplicate_id_records=", sum(duplicated(data_text$id) & !is.na(data_text$id) & data_text$id != "")),
  paste0("A_q2_values=", paste(sort(unique(data_text$A_q2)), collapse = "|")),
  paste0("created_at=", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
)
writeLines(audit_lines, audit_path, useBytes = TRUE)
message("TEXT_RDS_CREATED=", normalizePath(rds_path, winslash = "/"))
