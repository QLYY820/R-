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
      survey_date = "submittime",
      survey_year_allowed = 2021:2025,
      birth_year = "A_year",
      work_start_year = "work_y",
      height_cm = "A_q15",
      weight_kg = "A_q16",
      response_time_variables = c(
        "timetaken.x", "timetaken.y", "timetaken.x.x", "timetaken.y.y"
      ),
      symptom_prefix = "E_q24_",
      sites = c("neck", "shoulder", "upper_back", "elbow", "wrist_hand", "lower_back", "hip_thigh", "knee", "ankle_foot"),
      site_labels = c("Neck", "Shoulder", "Upper back", "Elbow", "Wrist/hand", "Lower back", "Hip/thigh", "Knee", "Ankle/foot")
    ),
    cohort_cleaning = list(
      core_scale_variables = unique(c(
        paste0("D_q22_", 1:7), paste0("D_q23_", 1:9),
        paste0("D_q24_", 1:10), paste0("D_q19_", 1:8),
        paste0("F_q3_", 1:22), paste0("D_q10_", 1:7), "D_PSQI_ALL"
      )),
      maximum_core_missing_fraction = 0.30,
      minimum_work_years = 1,
      minimum_nursing_entry_age = 16,
      minimum_response_time_seconds = 600,
      valid_age_range = c(16, 65),
      valid_height_cm_range = c(140, 210),
      valid_weight_kg_range = c(35, 120)
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
