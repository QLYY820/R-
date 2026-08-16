# Analysis: Ising network of nine 12-month musculoskeletal symptom sites.
# Date: 2026-08-17
# Random seed: centralized in config/analysis_config.R
# R: 4.5+
# Key packages: bootnet, qgraph, IsingFit, data.table, openxlsx

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Chinese"), silent = TRUE))
}
project_root <- normalizePath(Sys.getenv("OR_MSK_PROJECT_ROOT"), winslash = "/", mustWork = TRUE)
source(Sys.getenv("OR_MSK_CONFIG"))
config <- build_analysis_config(
  project_root,
  mode = Sys.getenv("OR_MSK_MODE", "formal"),
  output_root = Sys.getenv("OR_MSK_OUTPUT_ROOT")
)
set.seed(config$seed)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript scripts/04_ising_network_analysis.R <analysis_data.rds> <output_dir>")
input_path <- args[1]
output_dir <- args[2]
if (!file.exists(input_path)) stop("Analysis RDS does not exist: ", input_path)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (pkg in c("bootnet", "qgraph", "IsingFit", "data.table", "openxlsx", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing package: ", pkg)
}

cat("R:", R.version.string, "\n")
for (pkg in c("bootnet", "qgraph", "IsingFit", "data.table", "openxlsx", "ggplot2")) {
  cat(pkg, ": ", as.character(utils::packageVersion(pkg)), "\n", sep = "")
}

data <- readRDS(input_path)
sites <- config$variables$sites
labels <- config$variables$site_labels
symptom_vars <- paste0(sites, "_symptom_12m")
network_data <- data[stats::complete.cases(data[symptom_vars]), symptom_vars]
network_data[] <- lapply(network_data, as.numeric)
names(network_data) <- labels
if (!all(vapply(network_data, function(x) all(x %in% c(0, 1)), logical(1)))) stop("Network indicators must be binary 0/1.")

estimated <- bootnet::estimateNetwork(
  network_data,
  default = "IsingFit",
  tuning = 0.5,
  rule = "AND",
  labels = labels,
  verbose = FALSE
)
adjacency <- estimated$graph
diag(adjacency) <- 0

centrality <- data.frame(
  node = labels,
  strength = rowSums(abs(adjacency)),
  expected_influence = rowSums(adjacency),
  degree = rowSums(adjacency != 0),
  stringsAsFactors = FALSE
)
centrality$strength_rank <- rank(-centrality$strength, ties.method = "min")
centrality$expected_influence_rank <- rank(-centrality$expected_influence, ties.method = "min")
centrality <- centrality[order(centrality$expected_influence_rank, centrality$strength_rank), ]

edge_rows <- list()
counter <- 0L
for (i in seq_len(nrow(adjacency) - 1)) {
  for (j in (i + 1):ncol(adjacency)) {
    counter <- counter + 1L
    edge_rows[[counter]] <- data.frame(node1 = labels[i], node2 = labels[j], weight = adjacency[i, j], selected = adjacency[i, j] != 0)
  }
}
edge_weights <- do.call(rbind, edge_rows)
edge_weights <- edge_weights[order(-abs(edge_weights$weight)), ]

nonparametric_path <- file.path(output_dir, "ising_bootstrap_nonparametric.rds")
if (file.exists(nonparametric_path)) {
  nonparametric_boot <- readRDS(nonparametric_path)
} else {
  cat("Running", config$runtime$network_boots, "nonparametric bootstrap samples...\n")
  set.seed(config$seed)
  nonparametric_boot <- bootnet::bootnet(
    estimated,
    nBoots = config$runtime$network_boots,
    type = "nonparametric",
    statistics = c("edge", "strength", "expectedInfluence"),
    nCores = config$runtime$network_cores,
    verbose = FALSE
  )
  saveRDS(nonparametric_boot, nonparametric_path)
}

case_path <- file.path(output_dir, "ising_bootstrap_case.rds")
if (file.exists(case_path)) {
  case_boot <- readRDS(case_path)
} else {
  cat("Running", config$runtime$network_boots, "case-dropping bootstrap samples...\n")
  set.seed(config$seed)
  case_boot <- bootnet::bootnet(
    estimated,
    nBoots = config$runtime$network_boots,
    type = "case",
    statistics = c("strength", "expectedInfluence"),
    nCores = config$runtime$network_cores,
    verbose = FALSE
  )
  saveRDS(case_boot, case_path)
}

stability_values <- tryCatch(
  bootnet::corStability(case_boot, statistics = c("strength", "expectedInfluence")),
  error = function(error) c(strength = NA_real_, expectedInfluence = NA_real_)
)
stability <- data.frame(statistic = names(stability_values), cs_coefficient = as.numeric(stability_values))

boot_table <- as.data.frame(nonparametric_boot$bootTable)
case_table <- as.data.frame(case_boot$bootTable)

edge_boot <- boot_table[boot_table$type == "edge", , drop = FALSE]
edge_ci <- data.frame()
if (nrow(edge_boot) && all(c("node1", "node2", "value") %in% names(edge_boot))) {
  edge_ci <- data.table::as.data.table(edge_boot)[, .(
    bootstrap_mean = mean(value, na.rm = TRUE),
    ci_lower = stats::quantile(value, 0.025, na.rm = TRUE),
    ci_upper = stats::quantile(value, 0.975, na.rm = TRUE)
  ), by = .(node1, node2)]
  edge_ci <- as.data.frame(edge_ci)
}

data.table::fwrite(cbind(node = labels, as.data.frame(adjacency)), file.path(output_dir, "ising_adjacency_matrix.csv"), bom = TRUE)
data.table::fwrite(edge_weights, file.path(output_dir, "ising_edge_weights.csv"), bom = TRUE)
data.table::fwrite(centrality, file.path(output_dir, "ising_centrality.csv"), bom = TRUE)
data.table::fwrite(stability, file.path(output_dir, "ising_stability.csv"), bom = TRUE)
data.table::fwrite(boot_table, file.path(output_dir, "ising_bootstrap_table.csv"), bom = TRUE)
data.table::fwrite(case_table, file.path(output_dir, "ising_case_bootstrap_table.csv"), bom = TRUE)
if (nrow(edge_ci)) data.table::fwrite(edge_ci, file.path(output_dir, "ising_edge_bootstrap_ci.csv"), bom = TRUE)

workbook <- openxlsx::createWorkbook()
for (sheet in c("adjacency", "edge_weights", "centrality", "stability", "edge_bootstrap_ci")) openxlsx::addWorksheet(workbook, sheet)
openxlsx::writeData(workbook, "adjacency", cbind(node = labels, as.data.frame(adjacency)))
openxlsx::writeData(workbook, "edge_weights", edge_weights)
openxlsx::writeData(workbook, "centrality", centrality)
openxlsx::writeData(workbook, "stability", stability)
openxlsx::writeData(workbook, "edge_bootstrap_ci", edge_ci)
openxlsx::saveWorkbook(workbook, file.path(output_dir, "ising_network_results.xlsx"), overwrite = TRUE)

node_colors <- rep("#4477AA", length(labels))
layout <- qgraph::qgraph(adjacency, layout = "spring", DoNotPlot = TRUE)$layout
for (extension in c("png", "pdf")) {
  path <- file.path(output_dir, paste0("ising_network.", extension))
  if (extension == "png") grDevices::png(path, width = 2100, height = 1500, res = 300) else grDevices::cairo_pdf(path, width = 7, height = 5)
  qgraph::qgraph(
    adjacency,
    layout = layout,
    labels = labels,
    color = node_colors,
    theme = "colorblind",
    vsize = 7,
    label.cex = 0.8,
    edge.labels = FALSE,
    legend = FALSE,
    title = "Ising network of 12-month musculoskeletal symptoms"
  )
  grDevices::dev.off()
}

centrality_long <- rbind(
  data.frame(node = centrality$node, statistic = "Strength", value = centrality$strength),
  data.frame(node = centrality$node, statistic = "Expected influence", value = centrality$expected_influence)
)
centrality_long$node <- factor(centrality_long$node, levels = rev(centrality$node))
centrality_plot <- ggplot2::ggplot(centrality_long, ggplot2::aes(x = value, y = node, color = statistic)) +
  ggplot2::geom_point(size = 2.2) + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = value, yend = node), linewidth = 0.5) +
  ggplot2::facet_wrap(~ statistic, scales = "free_x") +
  ggplot2::labs(x = "Centrality", y = NULL, title = "Node centrality") +
  ggplot2::theme_minimal(base_family = "Arial", base_size = 9) + ggplot2::theme(legend.position = "none")
ggplot2::ggsave(file.path(output_dir, "ising_centrality.png"), centrality_plot, width = 7, height = 4.5, dpi = 300)
ggplot2::ggsave(file.path(output_dir, "ising_centrality.pdf"), centrality_plot, width = 7, height = 4.5, device = grDevices::cairo_pdf)

edge_interval_plot <- plot(nonparametric_boot, labels = FALSE, order = "sample", plot = "interval", statistics = "edge")
ggplot2::ggsave(file.path(output_dir, "ising_edge_accuracy.png"), edge_interval_plot, width = 7, height = 8, dpi = 300)
ggplot2::ggsave(file.path(output_dir, "ising_edge_accuracy.pdf"), edge_interval_plot, width = 7, height = 8, device = grDevices::cairo_pdf)

case_plot <- plot(case_boot, statistics = c("strength", "expectedInfluence"))
ggplot2::ggsave(file.path(output_dir, "ising_centrality_stability.png"), case_plot, width = 7, height = 5, dpi = 300)
ggplot2::ggsave(file.path(output_dir, "ising_centrality_stability.pdf"), case_plot, width = 7, height = 5, device = grDevices::cairo_pdf)

saveRDS(estimated, file.path(output_dir, "ising_network_model.rds"))

cat("\nCentrality:\n")
print(centrality)
cat("\nTop edges:\n")
print(utils::head(edge_weights, 10))
cat("\nCorrelation-stability coefficients:\n")
print(stability)
cat("STEP_COMPLETE=ising_network_analysis\n")
