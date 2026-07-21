.icu_pipeline_root <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  valid_source <- is.character(source_file) && length(source_file) == 1L &&
    !is.na(source_file) && nzchar(source_file)
  if (!valid_source) getwd() else dirname(normalizePath(source_file))
})

.icu_restore_environment <- function(old_environment) {
  for (name in names(old_environment)) {
    value <- old_environment[[name]]
    if (is.na(value)) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, setNames(list(value), name))
    }
  }
}

.icu_launch_pipeline <- function(
  input_file,
  output_dir,
  dictionary,
  config,
  mode,
  cores,
  bootstrap_cores
) {
  if (!mode %in% c("real", "simulation")) stop("mode must be 'real' or 'simulation'.")
  if (!file.exists(input_file)) stop("Pipeline input not found: ", input_file)
  if (!file.exists(dictionary)) stop("Dictionary not found: ", dictionary)
  if (!file.exists(config)) stop("Configuration not found: ", config)

  requested_cores <- as.integer(cores)
  requested_bootstrap_cores <- as.integer(bootstrap_cores)
  cores <- max(1L, min(requested_cores, 4L))
  bootstrap_cores <- max(1L, min(requested_bootstrap_cores, cores, 2L))
  if (requested_cores != cores || requested_bootstrap_cores != bootstrap_cores) {
    message("Parallel workers capped for bastion safety: main=", cores,
            ", bootstrap=", bootstrap_cores)
  }

  environment_names <- c(
    "ICU_PIPELINE_CORES", "ICU_BOOTSTRAP_CORES", "ICU_PIPELINE_MAX_CORES",
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
  )
  old_environment <- Sys.getenv(environment_names, unset = NA_character_)
  names(old_environment) <- environment_names
  on.exit(.icu_restore_environment(old_environment), add = TRUE)

  Sys.setenv(
    ICU_PIPELINE_CORES = cores,
    ICU_BOOTSTRAP_CORES = bootstrap_cores,
    ICU_PIPELINE_MAX_CORES = 4L,
    OMP_NUM_THREADS = 1L,
    OPENBLAS_NUM_THREADS = 1L,
    MKL_NUM_THREADS = 1L,
    VECLIB_MAXIMUM_THREADS = 1L,
    NUMEXPR_NUM_THREADS = 1L
  )

  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  pipeline <- file.path(.icu_pipeline_root, "R", "final_analysis_pipeline.R")
  args <- c(
    pipeline,
    "--input", normalizePath(input_file, winslash = "/", mustWork = TRUE),
    "--dictionary", normalizePath(dictionary, winslash = "/", mustWork = TRUE),
    "--config", normalizePath(config, winslash = "/", mustWork = TRUE),
    "--output", output_dir,
    "--mode", mode
  )
  status <- system2(rscript, args = shQuote(args))
  if (!identical(status, 0L)) {
    stop("Pipeline returned nonzero status ", status, ". Check <output_dir>/logs/.")
  }
  invisible(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
}

run_icu_network_pipeline <- function(
  data,
  output_dir,
  dictionary = file.path(.icu_pipeline_root, "config", "source_dictionary.csv"),
  config = file.path(.icu_pipeline_root, "config", "analysis_config.yml"),
  mode = "real",
  cores = 3L,
  bootstrap_cores = 2L,
  release_input_memory = FALSE
) {
  data_expression <- substitute(data)
  caller_environment <- parent.frame()
  if (!is.data.frame(data)) stop("`data` must be a data.frame or data.table.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table first.")

  input_data <- data.table::as.data.table(data)
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

  input_rows <- nrow(input_data)
  temp_input <- tempfile(pattern = "icu_network_input_", fileext = ".csv")
  on.exit(unlink(temp_input, force = TRUE), add = TRUE)
  data.table::fwrite(input_data, temp_input)
  rm(input_data)
  if (isTRUE(release_input_memory)) {
    if (is.symbol(data_expression)) {
      data_name <- as.character(data_expression)
      if (exists(data_name, envir = caller_environment, inherits = FALSE)) {
        rm(list = data_name, envir = caller_environment)
      }
    }
    rm(data)
    message("Raw input object released from R memory after the temporary analysis file was written.")
  }
  invisible(gc())

  result <- .icu_launch_pipeline(
    input_file = temp_input,
    output_dir = output_dir,
    dictionary = dictionary,
    config = config,
    mode = mode,
    cores = cores,
    bootstrap_cores = bootstrap_cores
  )
  if (length(generated_ids)) {
    writeLines(
      c(
        "Technical identifiers absent from the source object were generated automatically.",
        "They are internal row-tracking fields and do not alter analysis variables.",
        paste("Technical IDs generated from row order:", paste(generated_ids, collapse = ", ")),
        paste("Rows:", input_rows)
      ),
      file.path(result, "INPUT_ID_PROVENANCE.txt")
    )
  }
  invisible(result)
}

resume_icu_network_pipeline <- function(
  output_dir = "~/ICU_network_REAL_RUN",
  dictionary = file.path(.icu_pipeline_root, "config", "source_dictionary.csv"),
  config = file.path(.icu_pipeline_root, "config", "analysis_config.yml"),
  mode = "real",
  cores = 3L,
  bootstrap_cores = 2L
) {
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  preparation_marker <- file.path(output_dir, ".pipeline_state", "01_preparation.PASS")
  scored_input <- file.path(output_dir, "01_preparation", "scored_analysis_data.csv")
  if (!file.exists(preparation_marker) || !file.exists(scored_input)) {
    stop("A completed preparation checkpoint was not found; a full run with `data` is required.")
  }
  message("Resuming from the saved preparation checkpoint; the raw Excel object is not required.")
  .icu_launch_pipeline(
    input_file = scored_input,
    output_dir = output_dir,
    dictionary = dictionary,
    config = config,
    mode = mode,
    cores = cores,
    bootstrap_cores = bootstrap_cores
  )
}
