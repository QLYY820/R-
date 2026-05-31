options(stringsAsFactors = FALSE)

find_out_dir <- function() {
  candidates <- c(
    file.path(getwd(), "somatic_exhaustion_network_R_outputs_main_qc"),
    getwd()
  )
  for (d in candidates) {
    if (file.exists(file.path(d, "combined_network_summary.csv")) &&
        file.exists(file.path(d, "quality_control_flow.csv"))) {
      return(normalizePath(d, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Cannot find somatic_exhaustion_network_R_outputs_main_qc in current working directory.")
}

out_dir <- find_out_dir()
cat("Checking output directory:\n", out_dir, "\n\n", sep = "")

read_csv <- function(name) {
  utils::read.csv(file.path(out_dir, name), check.names = FALSE, fileEncoding = "UTF-8-BOM")
}

tol <- 1e-5
checks <- data.frame(item = character(), status = character(), detail = character())
add_check <- function(item, ok, detail = "") {
  checks <<- rbind(
    checks,
    data.frame(item = item, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = detail)
  )
}
near <- function(x, y, tolerance = tol) isTRUE(abs(as.numeric(x) - as.numeric(y)) <= tolerance)

required_files <- c(
  "quality_control_flow.csv",
  "quality_control_summary.csv",
  "combined_network_summary.csv",
  "selected_nodes_12m_cleaned.csv",
  "selected_nodes_7d_cleaned.csv",
  "primary_12m_selected_nodes.csv",
  "sensitivity_7d_selected_nodes.csv",
  "primary_12m_network_summary.csv",
  "sensitivity_7d_network_summary.csv",
  "primary_12m_centrality_predictability.csv",
  "primary_12m_top_edges.csv",
  "primary_12m_nira_alleviating.csv",
  "primary_12m_pain_targets_by_fatigue_reduction.csv",
  "primary_12m_network.png",
  "primary_12m_figure_nira_interventions.png"
)
missing <- required_files[!file.exists(file.path(out_dir, required_files))]
add_check("Required output files", length(missing) == 0,
          if (length(missing)) paste(missing, collapse = "; ") else "all required files present")

qc <- read_csv("quality_control_flow.csv")
expected_flow <- data.frame(
  step = c(
    "Raw records",
    "Age outside 18-65 years or missing",
    "Work tenure <1 year or missing",
    "Work tenure greater than age minus 16 years",
    "Total completion time <10 minutes or missing",
    "NMQ 7-day yes but 12-month no; flagged only, not excluded",
    "Duplicated participant id"
  ),
  n_excluded_at_step = c(0, 388, 205, 423, 1001, 0, 0),
  n_remaining = c(64114, 63726, 63521, 63098, 62097, 62097, 62097)
)
add_check(
  "Quality-control flow",
  nrow(qc) == nrow(expected_flow) &&
    all(qc$step == expected_flow$step) &&
    all(qc$n_excluded_at_step == expected_flow$n_excluded_at_step) &&
    all(qc$n_remaining == expected_flow$n_remaining),
  paste(paste(qc$step, qc$n_remaining, sep = "="), collapse = "; ")
)

qsum <- read_csv("quality_control_summary.csv")
qsum_value <- function(metric) as.numeric(qsum$value[qsum$metric == metric][1])
add_check("Raw N", qsum_value("raw_n") == 64114, qsum_value("raw_n"))
add_check("Final QC sample", qsum_value("final_qc_keep") == 62097, qsum_value("final_qc_keep"))

row_count <- function(name) nrow(read_csv(name))
add_check("12-month cleaned node rows", row_count("selected_nodes_12m_cleaned.csv") == 62097,
          row_count("selected_nodes_12m_cleaned.csv"))
add_check("7-day cleaned node rows", row_count("selected_nodes_7d_cleaned.csv") == 62097,
          row_count("selected_nodes_7d_cleaned.csv"))
add_check("12-month complete cases", row_count("primary_12m_selected_nodes.csv") == 62091,
          row_count("primary_12m_selected_nodes.csv"))
add_check("7-day complete cases", row_count("sensitivity_7d_selected_nodes.csv") == 62091,
          row_count("sensitivity_7d_selected_nodes.csv"))

summary <- read_csv("combined_network_summary.csv")
expected_summary <- data.frame(
  analysis = c("primary_12m", "sensitivity_7d"),
  n_complete = c(62091, 62091),
  nodes = c(23, 23),
  edges = c(185, 189),
  density = c(0.731225296442688, 0.74703557312253),
  strongest_node = c("Fatigue_1", "Fatigue_1"),
  strongest_node_abbr = c("PF1", "PF1"),
  strongest_node_strength = c(9.01321073347892, 8.55672169732321)
)
for (i in seq_len(nrow(expected_summary))) {
  obs <- summary[summary$analysis == expected_summary$analysis[i], , drop = FALSE]
  ok <- nrow(obs) == 1 &&
    obs$n_complete == expected_summary$n_complete[i] &&
    obs$nodes == expected_summary$nodes[i] &&
    obs$edges == expected_summary$edges[i] &&
    near(obs$density, expected_summary$density[i]) &&
    obs$strongest_node == expected_summary$strongest_node[i] &&
    obs$strongest_node_abbr == expected_summary$strongest_node_abbr[i] &&
    near(obs$strongest_node_strength, expected_summary$strongest_node_strength[i])
  add_check(paste0("Network summary: ", expected_summary$analysis[i]), ok,
            if (nrow(obs)) paste(capture.output(print(obs)), collapse = " ") else "missing row")
}

nira <- read_csv("primary_12m_nira_alleviating.csv")
expected_nira_abbr <- c("PF1", "PF3", "PF2", "PF6", "PF8", "PF7", "PF5", "PF4", "P1", "MF1")
expected_nira_effect <- c(
  3.41131566737875, 3.39768878083343, 3.31551912044965, 3.28870632681451,
  3.21619833144374, 3.09008423523063, 2.90337088289392, 2.73698753779270,
  2.63249931219042, 2.53725567859346
)
add_check(
  "Primary 12m NIRA top-10 order",
  all(nira$abbr[1:10] == expected_nira_abbr),
  paste(nira$abbr[1:10], collapse = ", ")
)
add_check(
  "Primary 12m NIRA top-10 effects",
  all(abs(as.numeric(nira$nira_effect[1:10]) - expected_nira_effect) <= tol),
  paste(round(as.numeric(nira$nira_effect[1:10]), 6), collapse = ", ")
)

pain <- read_csv("primary_12m_pain_targets_by_fatigue_reduction.csv")
expected_pain_abbr <- c("P1", "P2", "P3", "P5", "P8", "P6", "P9", "P7", "P4")
expected_pain_total <- c(
  1.06810714809076, 0.878294798182901, 0.677619947409457,
  0.565909744917374, 0.534495457614972, 0.522153995983615,
  0.419842146643477, 0.407828785076206, 0.322390004800955
)
add_check(
  "Pain targets by fatigue reduction order",
  all(pain$abbr == expected_pain_abbr),
  paste(pain$abbr, collapse = ", ")
)
add_check(
  "Pain targets fatigue-reduction effects",
  all(abs(as.numeric(pain$total_fatigue_effect) - expected_pain_total) <= tol),
  paste(round(as.numeric(pain$total_fatigue_effect), 6), collapse = ", ")
)

centrality <- read_csv("primary_12m_centrality_predictability.csv")
centrality <- centrality[order(-as.numeric(centrality$strength)), ]
expected_cent_abbr <- c("PF1", "PF6", "PF4", "PF8", "PF3")
expected_cent_strength <- c(
  9.01321073347892, 7.83714634416924, 7.76847791177150,
  7.42775484229078, 7.13980074867314
)
add_check(
  "Primary 12m centrality top-5 order",
  all(centrality$abbr[1:5] == expected_cent_abbr),
  paste(centrality$abbr[1:5], collapse = ", ")
)
add_check(
  "Primary 12m centrality top-5 strength",
  all(abs(as.numeric(centrality$strength[1:5]) - expected_cent_strength) <= tol),
  paste(round(as.numeric(centrality$strength[1:5]), 6), collapse = ", ")
)

edges <- read_csv("primary_12m_top_edges.csv")
expected_edges <- paste(
  c("MF3", "MF5", "P1", "MF2", "PF1"),
  c("MF4", "MF6", "P2", "MF5", "PF2"),
  sep = "-"
)
obs_edges <- paste(edges$source_abbr[1:5], edges$target_abbr[1:5], sep = "-")
add_check("Primary 12m top-5 edge order", all(obs_edges == expected_edges),
          paste(obs_edges, collapse = ", "))

manifest <- data.frame(
  file = basename(required_files[file.exists(file.path(out_dir, required_files))]),
  size_bytes = file.info(file.path(out_dir, required_files[file.exists(file.path(out_dir, required_files))]))$size,
  md5 = unname(tools::md5sum(file.path(out_dir, required_files[file.exists(file.path(out_dir, required_files))]))),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(out_dir, "verification_md5_manifest.csv"), row.names = FALSE)
utils::write.csv(checks, file.path(out_dir, "verification_report_main_qc.csv"), row.names = FALSE)

print(checks, row.names = FALSE)
cat("\nOverall result: ", if (all(checks$status == "PASS")) "PASS" else "FAIL", "\n", sep = "")
cat("Saved report: ", file.path(out_dir, "verification_report_main_qc.csv"), "\n", sep = "")
cat("Saved MD5 manifest: ", file.path(out_dir, "verification_md5_manifest.csv"), "\n", sep = "")
