# Optional publication export: AMA-style Table 1 using gtsummary/flextable.
# Date: 2026-08-17
# Random seed: centralized in config/analysis_config.R (no stochastic operations)

options(stringsAsFactors = FALSE)
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))
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
if (length(args) != 4) {
  stop("Usage: Rscript scripts/07_gtsummary_table1.R <analysis_data.rds> <assignments.csv> <class_labels.csv> <output.docx>")
}

for (pkg in c("data.table", "gtsummary", "flextable")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing package: ", pkg)
}

data <- readRDS(args[1])
assignments <- data.table::fread(args[2], data.table = FALSE)
labels <- data.table::fread(args[3], data.table = FALSE)
labels <- labels[order(labels$presentation_order), ]
analysis <- merge(data, assignments[, c("row_id", "latent_class")], by = "row_id", all.x = TRUE, sort = FALSE)
analysis <- merge(analysis, labels[, c("latent_class", "class_label_en")], by = "latent_class", all.x = TRUE, sort = FALSE)
analysis$class_label_en <- factor(analysis$class_label_en, levels = labels$class_label_en)

table_data <- analysis[, c(
  "class_label_en", "age", "work_years", "BMI", "sex", "bachelor_or_above",
  "married", "permanent_employment", "supervisor_title_or_above",
  "administrative_role", "any_night_shift", "overtime_weeks_last_month"
)]

gtsummary::theme_gtsummary_compact()
table_one <- gtsummary::tbl_summary(
  table_data,
  by = class_label_en,
  statistic = list(
    gtsummary::all_continuous() ~ "{median} ({p25}–{p75})",
    gtsummary::all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(gtsummary::all_continuous() ~ 1, gtsummary::all_categorical() ~ c(0, 1)),
  missing = "ifany",
  label = list(
    age ~ "Age, y",
    work_years ~ "Nursing work duration, y",
    BMI ~ "Body mass index, kg/m²",
    bachelor_or_above ~ "Bachelor degree or above",
    permanent_employment ~ "Permanent employment",
    supervisor_title_or_above ~ "Supervisor title or above",
    administrative_role ~ "Administrative role",
    any_night_shift ~ "Any night shift",
    overtime_weeks_last_month ~ "Weeks >40 work h in previous month"
  )
) |>
  gtsummary::add_overall(last = FALSE) |>
  gtsummary::add_p(
    test = list(gtsummary::all_continuous() ~ "kruskal.test", gtsummary::all_categorical() ~ "chisq.test.no.correct")
  ) |>
  gtsummary::modify_caption("**Table 1. Characteristics of operating-room nurses by latent symptom class.**") |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols() ~ "Median (IQR) or n (%).")

table_one |>
  gtsummary::as_flex_table() |>
  flextable::save_as_docx(path = args[4])
cat("STEP_COMPLETE=table1_export\n")
