# Analysis: Latent class analysis of nine 12-month musculoskeletal symptom sites.
# Date: 2026-08-17
# Random seed: centralized in config/analysis_config.R
# R: 4.5+
# Key packages: poLCA, data.table, openxlsx, ggplot2

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
if (!length(args) %in% c(2, 3)) stop("Usage: Rscript scripts/03_latent_class_analysis.R <analysis_data.rds> <output_dir> [class_labels.csv]")
input_path <- args[1]
output_dir <- args[2]
if (!file.exists(input_path)) stop("Analysis RDS does not exist: ", input_path)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
label_config <- if (length(args) == 3) data.table::fread(args[3], data.table = FALSE) else NULL

for (pkg in c("poLCA", "data.table", "openxlsx", "ggplot2", "scales")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing package: ", pkg)
}

cat("R:", R.version.string, "\n")
for (pkg in c("poLCA", "data.table", "openxlsx", "ggplot2", "scales")) {
  cat(pkg, ": ", as.character(utils::packageVersion(pkg)), "\n", sep = "")
}

data <- readRDS(input_path)
sites <- config$variables$sites
site_labels <- config$variables$site_labels
symptom_vars <- paste0(sites, "_symptom_12m")
analysis_frame <- data[stats::complete.cases(data[symptom_vars]), c("row_id", symptom_vars)]
for (variable in symptom_vars) analysis_frame[[variable]] <- as.integer(analysis_frame[[variable]]) + 1L

formula_text <- paste0("cbind(", paste(symptom_vars, collapse = ", "), ") ~ 1")
lca_formula <- stats::as.formula(formula_text)
n <- nrow(analysis_frame)

checkpoint_dir <- file.path(output_dir, "lca_checkpoints")
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
maximum_classes <- config$runtime$lca_max_classes
models <- vector("list", maximum_classes)
fit_rows <- vector("list", maximum_classes)
for (classes in seq_len(maximum_classes)) {
  requested_nrep <- if (classes == 1) {
    1L
  } else if (classes >= 6) {
    config$runtime$lca_starts_large
  } else {
    config$runtime$lca_starts_small
  }
  checkpoint_suffix <- paste0("_", requested_nrep, "starts")
  checkpoint_path <- file.path(checkpoint_dir, paste0("lca_k", classes, checkpoint_suffix, ".rds"))
  if (file.exists(checkpoint_path)) {
    cat("Loading checkpoint for", classes, "class model...\n")
    fit <- readRDS(checkpoint_path)
  } else {
    cat("Fitting", classes, "class model...\n")
    fit <- poLCA::poLCA(
      lca_formula,
      data = analysis_frame,
      nclass = classes,
      nrep = requested_nrep,
      maxiter = 5000,
      tol = 1e-10,
      graphs = FALSE,
      verbose = FALSE,
      calc.se = TRUE
    )
    saveRDS(fit, checkpoint_path)
  }
  models[[classes]] <- fit
  log_likelihood <- as.numeric(fit$llik)
  parameters <- as.numeric(fit$npar)
  posterior <- fit$posterior
  relative_entropy <- if (classes == 1) {
    NA_real_
  } else {
    1 + sum(posterior * log(pmax(posterior, .Machine$double.eps))) / (n * log(classes))
  }
  assigned <- fit$predclass
  assigned_counts <- table(factor(assigned, levels = seq_len(classes)))
  attempts <- as.numeric(fit$attempts)
  fit_rows[[classes]] <- data.frame(
    classes = classes,
    log_likelihood = log_likelihood,
    parameters = parameters,
    AIC = fit$aic,
    BIC = fit$bic,
    CAIC = -2 * log_likelihood + parameters * (log(n) + 1),
    SABIC = -2 * log_likelihood + parameters * log((n + 2) / 24),
    relative_entropy = relative_entropy,
    smallest_class_n = min(assigned_counts),
    smallest_class_percent = min(assigned_counts) / n,
    maximum_iterations_used = fit$numiter,
    n_random_starts = requested_nrep,
    best_log_likelihood_exact_repetitions = if (classes == 1) 1 else sum(abs(attempts - log_likelihood) < 1e-8),
    best_log_likelihood_within_1e4_repetitions = if (classes == 1) 1 else sum(abs(attempts - log_likelihood) < 1e-4),
    stringsAsFactors = FALSE
  )
}

fit_statistics <- do.call(rbind, fit_rows)
eligible <- fit_statistics[fit_statistics$smallest_class_percent >= 0.05 | fit_statistics$classes == 1, ]
eligible <- eligible[eligible$classes >= 2, ]
if (nrow(eligible) == 0) stop("No eligible model with all classes >=5%.")
selected_k <- eligible$classes[which.min(eligible$BIC)]
selected_starts <- config$runtime$lca_selected_starts
final_selected_path <- file.path(checkpoint_dir, paste0("lca_selected_k", selected_k, "_", selected_starts, "starts.rds"))
if (file.exists(final_selected_path)) {
  selected_model <- readRDS(final_selected_path)
} else {
  cat("Refitting selected", selected_k, "class model with", selected_starts, "random starts...\n")
  selected_model <- poLCA::poLCA(
    lca_formula,
    data = analysis_frame,
    nclass = selected_k,
    nrep = selected_starts,
    maxiter = 5000,
    tol = 1e-10,
    graphs = FALSE,
    verbose = FALSE,
    calc.se = TRUE
  )
  saveRDS(selected_model, final_selected_path)
}
cat("Selected classes:", selected_k, "by minimum BIC among models with smallest class >=5%.\n")

final_posterior <- selected_model$posterior
final_entropy <- 1 + sum(final_posterior * log(pmax(final_posterior, .Machine$double.eps))) / (n * log(selected_k))
final_counts <- table(factor(selected_model$predclass, levels = seq_len(selected_k)))
fit_statistics[fit_statistics$classes == selected_k, c(
  "log_likelihood", "parameters", "AIC", "BIC", "CAIC", "SABIC",
  "relative_entropy", "smallest_class_n", "smallest_class_percent", "maximum_iterations_used",
  "n_random_starts", "best_log_likelihood_exact_repetitions",
  "best_log_likelihood_within_1e4_repetitions"
)] <- list(
  as.numeric(selected_model$llik), as.numeric(selected_model$npar), selected_model$aic, selected_model$bic,
  -2 * as.numeric(selected_model$llik) + as.numeric(selected_model$npar) * (log(n) + 1),
  -2 * as.numeric(selected_model$llik) + as.numeric(selected_model$npar) * log((n + 2) / 24),
  final_entropy, min(final_counts), min(final_counts) / n, selected_model$numiter,
  length(selected_model$attempts),
  sum(abs(as.numeric(selected_model$attempts) - as.numeric(selected_model$llik)) < 1e-8),
  sum(abs(as.numeric(selected_model$attempts) - as.numeric(selected_model$llik)) < 1e-4)
)

profiles <- do.call(rbind, lapply(seq_along(symptom_vars), function(index) {
  probability_matrix <- selected_model$probs[[symptom_vars[index]]]
  data.frame(
    class = seq_len(selected_k),
    site = sites[index],
    label = site_labels[index],
    probability = probability_matrix[, ncol(probability_matrix)],
    stringsAsFactors = FALSE
  )
}))

class_burden <- aggregate(probability ~ class, profiles, mean)
names(class_burden)[2] <- "mean_site_probability"
class_burden$expected_number_of_sites <- class_burden$mean_site_probability * length(sites)

assigned <- selected_model$predclass
posterior <- selected_model$posterior
class_quality <- do.call(rbind, lapply(seq_len(selected_k), function(class_number) {
  members <- assigned == class_number
  average_posterior <- mean(posterior[members, class_number])
  class_proportion <- mean(members)
  odds_correct_classification <- (average_posterior / (1 - average_posterior)) / (class_proportion / (1 - class_proportion))
  data.frame(
    class = class_number,
    n = sum(members),
    percent = class_proportion,
    average_posterior_probability = average_posterior,
    odds_correct_classification = odds_correct_classification,
    stringsAsFactors = FALSE
  )
}))
class_quality <- merge(class_quality, class_burden, by = "class", all.x = TRUE)

suggest_label <- function(class_number) {
  profile <- profiles[profiles$class == class_number, ]
  values <- setNames(profile$probability, profile$site)
  burden <- mean(values)
  axial_upper <- mean(values[c("neck", "shoulder", "upper_back")])
  distal_upper <- mean(values[c("elbow", "wrist_hand")])
  lower <- mean(values[c("lower_back", "hip_thigh", "knee", "ankle_foot")])
  if (burden <= 0.15) return("Low burden")
  if (burden >= 0.75) return("Generalized high burden")
  if (axial_upper - lower >= 0.20) return("Neck-shoulder-upper-back dominant")
  if (lower - axial_upper >= 0.20) return("Lower-back/lower-limb dominant")
  if (distal_upper >= max(axial_upper, lower)) return("Upper-limb dominant")
  "Intermediate multisite burden"
}
class_quality$suggested_label <- vapply(class_quality$class, suggest_label, character(1))
if (!is.null(label_config)) {
  class_quality <- merge(class_quality, label_config, by.x = "class", by.y = "latent_class", all.x = TRUE, sort = FALSE)
  class_quality <- class_quality[order(class_quality$presentation_order), ]
  profiles <- merge(profiles, label_config, by.x = "class", by.y = "latent_class", all.x = TRUE, sort = FALSE)
  profiles <- profiles[order(profiles$presentation_order, match(profiles$site, sites)), ]
}

assignment <- data.frame(
  row_id = analysis_frame$row_id,
  latent_class = assigned,
  posterior_max = apply(posterior, 1, max),
  stringsAsFactors = FALSE
)
for (class_number in seq_len(selected_k)) assignment[[paste0("posterior_class_", class_number)]] <- posterior[, class_number]
if (!is.null(label_config)) {
  assignment$class_label_en <- label_config$class_label_en[match(assignment$latent_class, label_config$latent_class)]
  assignment$class_label_cn <- label_config$class_label_cn[match(assignment$latent_class, label_config$latent_class)]
}

data.table::fwrite(fit_statistics, file.path(output_dir, "lca_fit_statistics.csv"), bom = TRUE)
data.table::fwrite(profiles, file.path(output_dir, "lca_class_profiles.csv"), bom = TRUE)
data.table::fwrite(class_quality, file.path(output_dir, "lca_class_quality.csv"), bom = TRUE)
data.table::fwrite(assignment, file.path(output_dir, "lca_class_assignments.csv"), bom = TRUE)
saveRDS(list(models = models, selected_k = selected_k, selected_model = selected_model, fit_statistics = fit_statistics, profiles = profiles, class_quality = class_quality), file.path(output_dir, "lca_models.rds"))

workbook <- openxlsx::createWorkbook()
for (sheet in c("fit_statistics", "class_profiles", "class_quality")) openxlsx::addWorksheet(workbook, sheet)
openxlsx::writeData(workbook, "fit_statistics", fit_statistics)
openxlsx::writeData(workbook, "class_profiles", profiles)
openxlsx::writeData(workbook, "class_quality", class_quality)
openxlsx::saveWorkbook(workbook, file.path(output_dir, "lca_results.xlsx"), overwrite = TRUE)

profiles$display_class <- if (!is.null(label_config)) factor(profiles$class_label_en, levels = label_config$class_label_en[order(label_config$presentation_order)]) else factor(profiles$class)
profile_plot <- ggplot2::ggplot(profiles, ggplot2::aes(x = factor(label, levels = site_labels), y = probability, group = display_class, color = display_class)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(x = NULL, y = "Conditional probability of 12-month symptoms", color = "Class", title = "Latent class symptom profiles") +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
  ggplot2::theme_minimal(base_family = "Arial", base_size = 9) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "bottom")
ggplot2::ggsave(file.path(output_dir, "lca_profile_plot.png"), profile_plot, width = 8.5, height = 5, dpi = 300)
ggplot2::ggsave(file.path(output_dir, "lca_profile_plot.pdf"), profile_plot, width = 8.5, height = 5, device = grDevices::cairo_pdf)

fit_long <- rbind(
  data.frame(classes = fit_statistics$classes, criterion = "BIC", value = fit_statistics$BIC),
  data.frame(classes = fit_statistics$classes, criterion = "SABIC", value = fit_statistics$SABIC),
  data.frame(classes = fit_statistics$classes, criterion = "AIC", value = fit_statistics$AIC)
)
fit_long$criterion <- factor(fit_long$criterion, levels = c("AIC", "BIC", "SABIC"))
fit_plot <- ggplot2::ggplot(
  fit_long,
  ggplot2::aes(x = classes, y = value, color = criterion, shape = criterion, linetype = criterion)
) +
  ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = fit_statistics$classes) +
  ggplot2::scale_color_manual(values = c("AIC" = "#D55E00", "BIC" = "#0072B2", "SABIC" = "#CC79A7"), name = NULL) +
  ggplot2::scale_shape_manual(values = c("AIC" = 16, "BIC" = 17, "SABIC" = 15), name = NULL) +
  ggplot2::scale_linetype_manual(values = c("AIC" = "solid", "BIC" = "dashed", "SABIC" = "dotted"), name = NULL) +
  ggplot2::labs(x = "Number of classes", y = "Information criterion") +
  ggplot2::theme_minimal(base_family = "Arial", base_size = 9) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(t = 6, r = 8, b = 6, l = 16, unit = "mm")
  )
ggplot2::ggsave(file.path(output_dir, "lca_fit_plot.png"), fit_plot, width = 7.0, height = 5.0, dpi = 610, bg = "white")
ggplot2::ggsave(file.path(output_dir, "lca_fit_plot.pdf"), fit_plot, width = 5.5, height = 4, device = grDevices::cairo_pdf)

cat("\nFit statistics:\n")
print(fit_statistics)
cat("\nSelected class quality:\n")
print(class_quality)
cat("\nSelected class profiles:\n")
print(profiles)
cat("STEP_COMPLETE=latent_class_analysis\n")
