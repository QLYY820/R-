# Short RStudio entry point.
# After loading the full dataset as an object named `data`, run:
# source("go.R")

run_hrpl_review <- function(dat = get("data", envir = .GlobalEnv),
                            out = "analysis_outputs_full",
                            bootstrap = 0,
                            min_subgroup_n = 100,
                            run_cfa = FALSE) {
  if (!is.data.frame(dat) && !is.matrix(dat)) {
    stop("`data` must be a data.frame, tibble, data.table, or matrix-like object.", call. = FALSE)
  }

  script <- file.path("R", "reviewer_response_stats.R")
  if (!file.exists(script)) {
    stop("Cannot find R/reviewer_response_stats.R. Please set the RStudio working directory to the repository root.", call. = FALSE)
  }

  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile("rstudio_data_", fileext = ".rds")
  message("Preparing data object for analysis...")
  saveRDS(as.data.frame(dat), tmp)
  on.exit(unlink(tmp), add = TRUE)

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )

  args <- c(
    script,
    "--data", tmp,
    "--out", out,
    "--bootstrap", as.character(bootstrap),
    "--min_subgroup_n", as.character(min_subgroup_n),
    "--run_cfa", if (isTRUE(run_cfa)) "TRUE" else "FALSE"
  )

  status <- system2(rscript, args = args)
  if (!identical(status, 0L)) {
    stop("Analysis failed. Check the console messages above.", call. = FALSE)
  }

  message("Done. Outputs are in: ", normalizePath(out, winslash = "/", mustWork = FALSE))
  invisible(normalizePath(out, winslash = "/", mustWork = FALSE))
}

if (!exists("data", envir = .GlobalEnv, inherits = FALSE)) {
  stop("No object named `data` found. In RStudio, read the full dataset as `data` first, then run source(\"go.R\").", call. = FALSE)
}

boot_value <- if (exists("boot", envir = .GlobalEnv, inherits = FALSE)) {
  get("boot", envir = .GlobalEnv)
} else {
  0
}

out_value <- if (exists("out", envir = .GlobalEnv, inherits = FALSE)) {
  get("out", envir = .GlobalEnv)
} else {
  "analysis_outputs_full"
}

cfa_value <- if (exists("cfa", envir = .GlobalEnv, inherits = FALSE)) {
  get("cfa", envir = .GlobalEnv)
} else {
  FALSE
}

minn_value <- if (exists("minn", envir = .GlobalEnv, inherits = FALSE)) {
  get("minn", envir = .GlobalEnv)
} else {
  100
}

run_hrpl_review(data, out = out_value, bootstrap = boot_value, min_subgroup_n = minn_value, run_cfa = cfa_value)
