options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(officer)
  library(flextable)
})

base_dir <- Sys.getenv(
  "HEAD_NURSE_BASE_DIR",
  unset = file.path("analysis_outputs", "head_nurse_propensity_health_20260601")
)
main_docx_candidates <- file.path(
  base_dir,
  c(
    Sys.getenv(
      "HEAD_NURSE_MAIN_DOCX_NAME",
      unset = "head_nurse_occupational_health_baseline_cross_sectional_manuscript.docx"
    ),
    "head_nurse_occupational_health_baseline_cross_sectional_manuscript_tables_aligned.docx",
    "head_nurse_occupational_health_baseline_cross_sectional_manuscript.docx"
  )
)
main_docx_candidates <- unique(main_docx_candidates)
main_docx <- main_docx_candidates[file.exists(main_docx_candidates)][1]
if (is.na(main_docx)) main_docx <- main_docx_candidates[1]
out_docx <- file.path(base_dir, "head_nurse_occupational_health_supplementary_material.docx")

read_csv <- function(name) data.table::fread(file.path(base_dir, name), encoding = "UTF-8")

main_title <- "Occupational-Health Risk Profiles of Head Nurses and Staff Nurses: A Propensity-Weighted Baseline Cross-Sectional Analysis"
if (file.exists(main_docx)) {
  main_text <- docx_summary(read_docx(main_docx))[["text"]]
  main_text <- main_text[nzchar(main_text)]
  if (length(main_text) > 0) main_title <- main_text[1]
}

cleaning_flow <- read_csv("01_cleaning_flow.csv")
score_summary <- read_csv("02_score_range_summary.csv")
baseline_att <- read_csv("03b_baseline_table1_att.csv")
ps_summary <- read_csv("04_propensity_summary.csv")
balance_full <- read_csv("05_balance_model1_att.csv")
balance_day <- read_csv("06_balance_model3_day_only_att.csv")
outcome_models <- read_csv("07_outcome_models_att.csv")
risk_profile <- read_csv("08_risk_transition_summary.csv")

cleaning_rules <- readLines(file.path(base_dir, "00_cleaning_rules.md"), warn = FALSE, encoding = "UTF-8")
cleaning_rules <- cleaning_rules[grepl("^\\d+\\.", cleaning_rules)]
cleaning_rules <- sub("^\\d+\\.\\s*", "", cleaning_rules)

fmt_n <- function(x) format(as.integer(round(as.numeric(x))), big.mark = ",")
fmt_num <- function(x, digits = 3) ifelse(is.na(as.numeric(x)), "", sprintf(paste0("%.", digits, "f"), as.numeric(x)))
fmt_p <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}
fmt_ci <- function(lo, hi, digits = 2) paste0(fmt_num(lo, digits), " to ", fmt_num(hi, digits))

font_main <- "Times New Roman"
font_cjk <- "SimSun"
fp_body <- fp_text(font.size = 11, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_body_bold <- fp_text(font.size = 11, bold = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_title <- fp_text(font.size = 15, bold = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_heading1 <- fp_text(font.size = 13, bold = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_heading2 <- fp_text(font.size = 11, bold = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_note <- fp_text(font.size = 9.5, italic = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)

par_body <- fp_par(line_spacing = 1.15, word_style = NA_character_)
par_title <- fp_par(text.align = "center", line_spacing = 1.15, keep_with_next = TRUE, word_style = NA_character_)
par_heading <- fp_par(line_spacing = 1.15, keep_with_next = TRUE, word_style = NA_character_)
par_caption <- fp_par(line_spacing = 1.15, keep_with_next = TRUE, word_style = NA_character_)
par_note <- fp_par(line_spacing = 1.05, word_style = NA_character_)

add_text <- function(doc, text = "") body_add_fpar(doc, fpar(ftext(text, prop = fp_body), fp_p = par_body))
add_title <- function(doc, text) body_add_fpar(doc, fpar(ftext(text, prop = fp_title), fp_p = par_title))
add_heading1 <- function(doc, text) body_add_fpar(doc, fpar(ftext(text, prop = fp_heading1), fp_p = par_heading))
add_heading2 <- function(doc, text) body_add_fpar(doc, fpar(ftext(text, prop = fp_heading2), fp_p = par_heading))
add_caption <- function(doc, text) body_add_fpar(doc, fpar(ftext(text, prop = fp_body), fp_p = par_caption))
add_note <- function(doc, text) body_add_fpar(doc, fpar(ftext(text, prop = fp_note), fp_p = par_note))
add_blank <- function(doc) body_add_par(doc, "", style = "Normal")
add_bullet <- function(doc, text) {
  body_add_list(
    doc,
    block_list_items(list_item(fpar(ftext(text, prop = fp_body), fp_p = par_body)), list_type = "bullet")
  )
}

make_ft <- function(dat, font_size = 7.5) {
  dat <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
  dat[] <- lapply(dat, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  ft <- flextable(dat)
  ft <- theme_booktabs(ft)
  ft <- font(ft, fontname = font_main, part = "all")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- fontsize(ft, size = font_size + 0.3, part = "header")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, j = 1, align = "left", part = "body")
  if (ncol(dat) > 1) ft <- align(ft, j = 2:ncol(dat), align = "center", part = "body")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- padding(ft, padding.top = 2.5, padding.bottom = 2.5, padding.left = 3, padding.right = 3, part = "all")
  ft <- autofit(ft)
  set_table_properties(ft, layout = "autofit", width = 1)
}

add_table <- function(doc, dat, caption, note = NULL, font_size = 7.5) {
  doc <- add_caption(doc, caption)
  doc <- body_add_flextable(doc, make_ft(dat, font_size = font_size))
  if (!is.null(note) && nzchar(note)) doc <- add_note(doc, paste0("Note. ", note))
  add_blank(doc)
}

add_img <- function(doc, path, caption, width = 6.5, height = 3.6) {
  if (!file.exists(path)) return(doc)
  doc <- add_caption(doc, caption)
  doc <- body_add_img(doc, src = path, width = width, height = height)
  add_blank(doc)
}

cleaning_flow_tbl <- cleaning_flow %>%
  transmute(
    Step = step,
    `Excluded at step` = fmt_n(n_excluded_at_step),
    `Remaining` = fmt_n(n_remaining)
  )

score_tbl <- score_summary %>%
  transmute(
    Variable = variable,
    `Available after range check` = fmt_n(n_available_after_range_check)
  )

ps_metric_labels <- c(
  n_analysis = "Final analytic sample",
  n_head_nurse = "Head nurses",
  n_staff = "Staff nurses",
  treated_day_only_pct = "Day-shift-only among head nurses, %",
  staff_day_only_pct = "Day-shift-only among staff nurses, %",
  model1_weight_p99_cap = "Model 1 staff ATT weight cap, 99th percentile",
  max_abs_smd_before = "Maximum absolute SMD before weighting",
  max_abs_smd_after = "Maximum absolute SMD after ATT weighting",
  model3_n_day_only = "Day-shift-only analytic sample",
  model3_n_head_nurse = "Head nurses in day-shift-only sample",
  model3_n_staff = "Staff nurses in day-shift-only sample",
  model3_max_abs_smd_after = "Maximum absolute SMD after ATT weighting, day-shift-only sample"
)

ps_tbl <- ps_summary %>%
  mutate(
    Metric = ifelse(metric %in% names(ps_metric_labels), unname(ps_metric_labels[metric]), metric),
    Value = ifelse(grepl("pct|smd|cap", metric), fmt_num(value, 3), fmt_n(value))
  ) %>%
  select(Metric, Value)

baseline_tbl <- baseline_att %>%
  mutate(
    Characteristic = ifelse(level == "" | is.na(level), characteristic, paste0("  ", level)),
    `SMD before` = ifelse(is.na(smd_before), "", fmt_num(smd_before, 3)),
    `SMD after ATT` = ifelse(is.na(smd_after_att), "", fmt_num(smd_after_att, 3))
  ) %>%
  transmute(
    Characteristic,
    `Head nurse, unweighted` = head_unweighted,
    `Staff nurse, unweighted` = staff_unweighted,
    `SMD before`,
    `Head nurse, ATT weighted` = head_att_weighted,
    `Staff nurse, ATT weighted` = staff_att_weighted,
    `SMD after ATT`
  )

balance_full_tbl <- balance_full %>%
  transmute(
    Covariate = covariate,
    `SMD before` = fmt_num(smd_before, 3),
    `SMD after ATT` = fmt_num(smd_after_att_weighting, 3),
    `Abs SMD before` = fmt_num(abs_smd_before, 3),
    `Abs SMD after ATT` = fmt_num(abs_smd_after_att_weighting, 3)
  )

balance_day_tbl <- balance_day %>%
  transmute(
    Covariate = covariate,
    `SMD before` = fmt_num(smd_before, 3),
    `SMD after ATT` = fmt_num(smd_after_att_weighting, 3),
    `Abs SMD before` = fmt_num(abs_smd_before, 3),
    `Abs SMD after ATT` = fmt_num(abs_smd_after_att_weighting, 3)
  )

outcome_tbl <- outcome_models %>%
  mutate(
    Model = gsub("Model 1: ATT weighted, no night-shift adjustment", "M1: total role association", model, fixed = TRUE),
    Model = gsub("Model 2: ATT weighted, plus night-shift adjustment", "M2: plus night shift", Model, fixed = TRUE),
    Model = gsub("Model 3: day-shift-only ATT weighted", "M3: day-shift only", Model, fixed = TRUE),
    Model = gsub("Sensitivity 1: ATT weighted plus baseline covariates", "Sensitivity 1: M1 plus baseline covariates", Model, fixed = TRUE),
    Model = gsub("Sensitivity 2: day-shift-only ATT weighted plus baseline covariates", "Sensitivity 2: M3 plus baseline covariates", Model, fixed = TRUE),
    Effect = ifelse(type == "binary", paste0("PR ", fmt_num(estimate, 2)), paste0("MD ", fmt_num(estimate, 2))),
    `95% CI` = fmt_ci(conf_low, conf_high, 2),
    P = fmt_p(p_value),
    `FDR P` = fmt_p(p_fdr),
    `Head mean/prevalence` = fmt_num(head_mean_or_prev, 3),
    `Staff mean/prevalence` = fmt_num(staff_mean_or_prev, 3)
  ) %>%
  transmute(Model, Outcome = label, N = fmt_n(n), `Head n` = fmt_n(n_head_nurse), `Staff n` = fmt_n(n_staff),
            Effect, `95% CI`, P, `FDR P`, `Head mean/prevalence`, `Staff mean/prevalence`)

risk_tbl <- risk_profile %>%
  transmute(
    Outcome = label,
    Type = type,
    M1 = ifelse(type == "binary", paste0("PR ", fmt_num(model1_estimate, 2)), fmt_num(model1_estimate, 2)),
    `M1 P` = fmt_p(model1_p),
    M2 = ifelse(type == "binary", paste0("PR ", fmt_num(model2_estimate, 2)), fmt_num(model2_estimate, 2)),
    `M2 P` = fmt_p(model2_p),
    M3 = ifelse(type == "binary", paste0("PR ", fmt_num(model3_estimate, 2)), fmt_num(model3_estimate, 2)),
    `M3 P` = fmt_p(model3_p),
    `M2 minus M1` = fmt_num(model2_minus_model1, 2),
    `M3 minus M1` = fmt_num(model3_minus_model1, 2),
    Interpretation = gsub("risk-transition", "risk-profile", interpretation, fixed = TRUE)
  )

doc <- read_docx()
doc <- body_set_default_section(
  doc,
  prop_section(
    page_size = page_size(orient = "portrait"),
    page_margins = page_mar(top = 1, bottom = 1, left = 0.8, right = 0.8, header = 0.5, footer = 0.5)
  )
)

doc <- add_title(doc, "Supplementary Material")
doc <- add_title(doc, main_title)
doc <- add_blank(doc)
doc <- add_text(doc, "This supplementary file accompanies the baseline cross-sectional manuscript and is aligned with the Risk Profiles framing used in the main text.")
doc <- add_note(doc, "Preparation note: this file uses local trial-run results and should be regenerated with the final bastion/RStudio Server analysis before submission.")
doc <- add_blank(doc)

doc <- add_heading1(doc, "Supplementary Methods")
doc <- add_heading2(doc, "Cleaning and Analysis Basis")
for (rule in cleaning_rules) doc <- add_bullet(doc, rule)

doc <- body_add_break(doc)
doc <- add_heading1(doc, "Supplementary Tables")
doc <- add_table(doc, cleaning_flow_tbl, "Supplementary Table S1. Data cleaning flow.", "The administrative-position exclusion restricts the analytic contrast to head nurses and staff nurses without administrative roles.", font_size = 8)
doc <- add_table(doc, score_tbl, "Supplementary Table S2. Score range and available outcome data after range checks.", "Out-of-range scores were set to missing before outcome-specific complete-case models.", font_size = 8)
doc <- add_table(doc, ps_tbl, "Supplementary Table S3. Propensity-score weighting summary.", "ATT, average treatment effect on the treated; SMD, standardized mean difference.", font_size = 8)
doc <- add_table(doc, baseline_tbl, "Supplementary Table S4. Full baseline distribution before and after ATT weighting.", "Work schedule is shown for transparency but was not included in the propensity-score model because it was handled as a role-pathway variable.", font_size = 6.2)
doc <- add_table(doc, balance_full_tbl, "Supplementary Table S5. Covariate balance diagnostics for the full-sample ATT model.", "Work schedule is not included because it was not a propensity-score covariate.", font_size = 7.5)
doc <- add_table(doc, balance_day_tbl, "Supplementary Table S6. Covariate balance diagnostics for the day-shift-only ATT model.", "The day-shift-only analysis re-estimated propensity scores within nurses without night-shift exposure.", font_size = 7.5)
doc <- add_table(doc, outcome_tbl, "Supplementary Table S7. Full weighted outcome models and sensitivity analyses.", "MD, mean difference; PR, prevalence ratio; FDR P, false-discovery-rate adjusted P value.", font_size = 5.8)
doc <- add_table(doc, risk_tbl, "Supplementary Table S8. Full risk-profile pattern across total, night-shift-adjusted, and day-shift-only models.", "For continuous outcomes and domains, values are mean differences; for poor sleep, values are prevalence ratios.", font_size = 5.8)

doc <- body_add_break(doc)
doc <- add_heading1(doc, "Supplementary Figures")
doc <- add_img(doc, file.path(base_dir, "figure_love_plot_model1.png"), "Supplementary Figure S1. Full-sample covariate balance before and after ATT weighting.", width = 6.5, height = 4.0)
doc <- add_img(doc, file.path(base_dir, "figure_propensity_overlap_weights.png"), "Supplementary Figure S2. Propensity-score overlap and ATT weight distribution.", width = 6.5, height = 3.8)
doc <- add_img(doc, file.path(base_dir, "figure_love_plot_model3_day_only.png"), "Supplementary Figure S3. Day-shift-only covariate balance before and after ATT weighting.", width = 6.5, height = 4.0)
doc <- add_img(doc, file.path(base_dir, "figure_continuous_outcome_differences.png"), "Supplementary Figure S4. Continuous-outcome mean differences across the three baseline cross-sectional models.", width = 6.5, height = 4.0)

print(doc, target = out_docx)
cat(out_docx, "\n")
