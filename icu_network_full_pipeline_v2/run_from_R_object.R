.icu_pipeline_root <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(source_file) || !nzchar(source_file)) getwd() else dirname(normalizePath(source_file))
})

run_icu_network_pipeline <- function(
  data,
  output_dir,
  dictionary = file.path(.icu_pipeline_root, "config", "source_dictionary.csv"),
  config = file.path(.icu_pipeline_root, "config", "analysis_config.yml"),
  mode = "real",
  cores = max(1L, min(12L, parallel::detectCores() - 1L)),
  bootstrap_cores = max(1L, min(8L, cores))
) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame or data.table.")
  if (!mode %in% c("real", "simulation")) stop("mode must be 'real' or 'simulation'.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table first.")
  if (!file.exists(dictionary)) stop("Dictionary not found: ", dictionary)
  if (!file.exists(config)) stop("Configuration not found: ", config)

  input_data <- data.table::copy(data.table::as.data.table(data))
  generated_ids <- character()
  if (!"analysis_id" %in% names(input_data)) {
    input_data[, analysis_id := sprintf("AUTO-%08d", seq_len(.N))]
    generated_ids <- c(generated_ids, "analysis_id")
  }
  if (!"participant_hash" %in% names(input_data)) {
    input_data[, participant_hash := paste0("ROW-", analysis_id)]
    generated_ids <- c(generated_ids, "participant_hash")
  }
  if (anyDuplicated(input_data$analysis_id)) stop("analysis_id contains duplicates.")
  if (anyDuplicated(input_data$participant_hash)) stop("participant_hash contains duplicates.")

  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  temp_input <- tempfile(pattern = "icu_network_input_", fileext = ".csv")
  on.exit(unlink(temp_input, force = TRUE), add = TRUE)
  data.table::fwrite(input_data, temp_input)

  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  pipeline <- file.path(.icu_pipeline_root, "R", "final_analysis_pipeline.R")
  old <- Sys.getenv(c("ICU_PIPELINE_CORES", "ICU_BOOTSTRAP_CORES"), unset = NA_character_)
  on.exit({
    if (is.na(old[[1]])) Sys.unsetenv("ICU_PIPELINE_CORES") else Sys.setenv(ICU_PIPELINE_CORES = old[[1]])
    if (is.na(old[[2]])) Sys.unsetenv("ICU_BOOTSTRAP_CORES") else Sys.setenv(ICU_BOOTSTRAP_CORES = old[[2]])
  }, add = TRUE)
  Sys.setenv(ICU_PIPELINE_CORES = as.integer(cores), ICU_BOOTSTRAP_CORES = as.integer(bootstrap_cores))

  args <- c(
    pipeline,
    "--input", temp_input,
    "--dictionary", normalizePath(dictionary, winslash = "/", mustWork = TRUE),
    "--config", normalizePath(config, winslash = "/", mustWork = TRUE),
    "--output", output_dir,
    "--mode", mode
  )
  status <- system2(rscript, args = shQuote(args))
  if (!identical(status, 0L)) {
    stop("Pipeline returned nonzero status ", status, ". Check <output_dir>/logs/.")
  }
  if (length(generated_ids)) {
    writeLines(
      c(
        "Technical identifiers absent from the source object were generated automatically.",
        "They are internal row-tracking fields and do not alter analysis variables.",
        paste("Technical IDs generated from row order:", paste(generated_ids, collapse = ", ")),
        paste("Rows:", nrow(input_data))
      ),
      file.path(output_dir, "INPUT_ID_PROVENANCE.txt")
    )
  }
  invisible(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
}
