.icu_preflight_root <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(source_file) || !nzchar(source_file)) getwd() else dirname(normalizePath(source_file))
})

preflight_icu_data <- function(
  data,
  dictionary = file.path(.icu_preflight_root, "config", "source_dictionary.csv"),
  verbose = TRUE
) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame or data.table.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table first.")
  dictionary <- data.table::fread(dictionary)

  required_ids <- c("analysis_id", "participant_hash")
  required_covariates <- c(
    "A_q2", "A_age", "A_BMI", "A_gongzuoshichang", "A_q5", "A_q9",
    "A_q11", "A_q12", "C_q1", "C_q2", "C_q8"
  )
  required_totals <- c(
    "D_jiaolv_all", "D_yiyu_all", "D_yali_all", "F_qingganshuaijie",
    "F_qurengehua", "F_gerenchengjiugan", "G_pifa_all", "G_qutipifa",
    "G_naolipifa"
  )
  required_items <- dictionary$variable_name
  required <- unique(c(required_ids, required_covariates, required_totals, required_items))
  missing <- setdiff(required, names(data))
  time_columns <- grep("^timetaken", names(data), value = TRUE)
  repaired_columns <- grep("^\\.\\.\\.[0-9]+$", names(data), value = TRUE)

  item_audit <- data.table::rbindlist(lapply(seq_len(nrow(dictionary)), function(i) {
    variable <- dictionary$variable_name[i]
    if (!variable %in% names(data)) {
      return(data.table::data.table(variable = variable, missing_column = TRUE,
        n_missing = NA_integer_, n_out_of_range = NA_integer_))
    }
    x <- data[[variable]]
    data.table::data.table(
      variable = variable,
      missing_column = FALSE,
      n_missing = sum(is.na(x)),
      n_out_of_range = sum(!is.na(x) & (x < dictionary$source_min[i] | x > dictionary$source_max[i]))
    )
  }))

  duplicate_ids <- if ("participant_hash" %in% names(data)) {
    sum(duplicated(data$participant_hash) | duplicated(data$participant_hash, fromLast = TRUE), na.rm = TRUE)
  } else NA_integer_

  checks <- data.table::data.table(
    check = c(
      "data_is_data_frame", "required_columns_present", "all_62_items_present",
      "response_time_column_present", "item_values_in_range", "participant_id_available"
    ),
    status = c(
      "PASS",
      if (length(missing) == 0L) "PASS" else "FAIL",
      if (all(required_items %in% names(data))) "PASS" else "FAIL",
      if (length(time_columns) > 0L) "PASS" else "FAIL",
      if (all(item_audit$n_out_of_range[!item_audit$missing_column] == 0L)) "PASS" else "FAIL",
      if (all(required_ids %in% names(data))) "PASS" else "FAIL"
    ),
    detail = c(
      paste("rows=", nrow(data), "columns=", ncol(data)),
      if (length(missing)) paste(missing, collapse = ", ") else "none missing",
      paste(sum(required_items %in% names(data)), "of 62"),
      if (length(time_columns)) paste(time_columns, collapse = ", ") else "none",
      paste("out-of-range=", sum(item_audit$n_out_of_range, na.rm = TRUE)),
      if (all(required_ids %in% names(data))) paste("duplicate-ID rows=", duplicate_ids) else paste(setdiff(required_ids, names(data)), collapse = ", ")
    )
  )

  result <- list(
    ready = all(checks$status == "PASS"),
    checks = checks,
    missing_required_columns = missing,
    item_audit = item_audit,
    repaired_excel_column_names = repaired_columns,
    response_time_columns = time_columns
  )
  if (verbose) {
    print(checks)
    cat("\nREADY FOR FULL PIPELINE:", result$ready, "\n")
    if (length(repaired_columns)) cat("Excel-repaired columns ignored unless mapped:", paste(repaired_columns, collapse = ", "), "\n")
  }
  invisible(result)
}
