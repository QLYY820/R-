build_analysis_config <- function(project_root, mode = c("formal", "test"), output_root = NULL) {
  mode <- match.arg(mode)
  seed <- as.integer(Sys.getenv("OR_MSK_SEED", "42"))
  if (is.na(seed)) stop("OR_MSK_SEED must be an integer")

  data_home <- file.path(Sys.getenv("HOME"), "我的数据")
  raw_xlsx <- Sys.getenv("OR_MSK_RAW_XLSX", file.path(data_home, "data.xlsx"))
  data_text_rds <- Sys.getenv("OR_MSK_DATA_TEXT_RDS", file.path(data_home, "data_text.rds"))
  if (is.null(output_root)) {
    run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_root <- file.path(project_root, "outputs", paste0("run_", run_stamp, "_", mode))
  }

  list(
    project = list(
      id = "operating_room_msk_cooccurrence",
      title = "Multisite musculoskeletal symptom profiles, symptom network, and work-time correlates among operating-room nurses",
      design = "Nationwide multicenter cross-sectional secondary analysis"
    ),
    mode = mode,
    seed = seed,
    paths = list(
      project_root = normalizePath(project_root, winslash = "/", mustWork = TRUE),
      raw_xlsx = raw_xlsx,
      data_text_rds = data_text_rds,
      output_root = output_root,
      class_labels = file.path(project_root, "config", "lca_class_labels.csv")
    ),
    variables = list(
      id_candidates = c("id", "ID"),
      department = "A_q9",
      operating_room_code = "7",
      age_candidates = c("A_year", "age"),
      work_years_candidates = c("work_y", "work_years"),
      bmi_candidates = c("A_BMI", "BMI"),
      survey_date_candidates = c("submittime", "submittime.x", "submittime.y"),
      survey_year_allowed = 2021:2025,
      symptom_prefix = "E_q24_",
      sites = c("neck", "shoulder", "upper_back", "elbow", "wrist_hand", "lower_back", "hip_thigh", "knee", "ankle_foot"),
      site_labels = c("Neck", "Shoulder", "Upper back", "Elbow", "Wrist/hand", "Lower back", "Hip/thigh", "Knee", "Ankle/foot")
    ),
    runtime = if (mode == "formal") {
      list(
        lca_max_classes = 7L,
        lca_starts_small = 20L,
        lca_starts_large = 100L,
        lca_selected_starts = 100L,
        network_boots = 1000L,
        network_cores = 4L,
        mice_m = 20L,
        mice_maxit = 10L,
        posterior_draws = 20L
      )
    } else {
      list(
        lca_max_classes = 5L,
        lca_starts_small = 2L,
        lca_starts_large = 3L,
        lca_selected_starts = 3L,
        network_boots = 20L,
        network_cores = 2L,
        mice_m = 2L,
        mice_maxit = 2L,
        posterior_draws = 2L
      )
    },
    packages = c(
      "readxl", "data.table", "openxlsx", "poLCA", "bootnet", "qgraph",
      "IsingFit", "ggplot2", "scales", "mice", "nnet", "broom"
    )
  )
}
