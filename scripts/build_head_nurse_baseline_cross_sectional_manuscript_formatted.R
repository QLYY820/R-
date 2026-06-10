options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(officer)
  library(flextable)
})

base_dir <- Sys.getenv(
  "HEAD_NURSE_BASE_DIR",
  unset = file.path("analysis_outputs", "head_nurse_propensity_health_20260601")
)
out_docx <- file.path(
  base_dir,
  Sys.getenv(
    "HEAD_NURSE_MAIN_DOCX_NAME",
    unset = "head_nurse_occupational_health_baseline_cross_sectional_manuscript_tables_aligned.docx"
  )
)

read_csv <- function(name) data.table::fread(file.path(base_dir, name), encoding = "UTF-8")

baseline <- read_csv("03c_baseline_table1_journal.csv")
results <- read_csv("07_outcome_models_att.csv")
transition <- read_csv("08_risk_transition_summary.csv")
ps_summary <- read_csv("04_propensity_summary.csv")
flow <- read_csv("01_cleaning_flow.csv")

main_title <- "Head-Nurse Occupational Health in a Baseline Cross-Sectional Analysis of the Chinese Nurses' Health Cohort"
running_title <- "Head-nurse role and occupational-health risk profile"
interpretive_framing <- "From schedule advantage to managerial strain"

fmt_n <- function(x) format(as.integer(round(as.numeric(x))), big.mark = ",")
fmt_num <- function(x, digits = 2) ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), as.numeric(x)))
fmt_p <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}
fmt_ci <- function(lo, hi, digits = 2) paste0(fmt_num(lo, digits), " to ", fmt_num(hi, digits))

get_row <- function(outcome_id, model_id) {
  results[results[["outcome"]] == outcome_id & results[["model"]] == model_id][1]
}

m1 <- "Model 1: ATT weighted, no night-shift adjustment"
m2 <- "Model 2: ATT weighted, plus night-shift adjustment"
m3 <- "Model 3: day-shift-only ATT weighted"

r_psqi_m1 <- get_row("psqi", m1)
r_sleep_m1 <- get_row("poor_sleep", m1)
r_sleep_m2 <- get_row("poor_sleep", m2)
r_circ_m1 <- get_row("circadian_disruption_domain", m1)
r_circ_m2 <- get_row("circadian_disruption_domain", m2)
r_circ_m3 <- get_row("circadian_disruption_domain", m3)
r_mgmt_m1 <- get_row("management_strain_domain", m1)
r_mgmt_m2 <- get_row("management_strain_domain", m2)
r_mgmt_m3 <- get_row("management_strain_domain", m3)
r_mbi_m2 <- get_row("mbi_ee", m2)
r_gad_m2 <- get_row("gad7", m2)
r_to_m2 <- get_row("turnover_intention", m2)
r_mbi_m3 <- get_row("mbi_ee", m3)
r_gad_m3 <- get_row("gad7", m3)
r_to_m3 <- get_row("turnover_intention", m3)

n_analysis <- as.integer(ps_summary[metric == "n_analysis", value])
n_head <- as.integer(ps_summary[metric == "n_head_nurse", value])
n_staff <- as.integer(ps_summary[metric == "n_staff", value])
day_head <- as.numeric(ps_summary[metric == "treated_day_only_pct", value])
day_staff <- as.numeric(ps_summary[metric == "staff_day_only_pct", value])
max_smd_before <- as.numeric(ps_summary[metric == "max_abs_smd_before", value])
max_smd_after <- as.numeric(ps_summary[metric == "max_abs_smd_after", value])
n_day <- as.integer(ps_summary[metric == "model3_n_day_only", value])
n_day_head <- as.integer(ps_summary[metric == "model3_n_head_nurse", value])
n_day_staff <- as.integer(ps_summary[metric == "model3_n_staff", value])
max_smd_day <- as.numeric(ps_summary[metric == "model3_max_abs_smd_after", value])

font_main <- "Times New Roman"
font_cjk <- "SimSun"
fp_body <- fp_text(font.size = 12, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_body_bold <- fp_text(font.size = 12, bold = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_title <- fp_text(font.size = 16, bold = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_heading1 <- fp_text(font.size = 14, bold = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_heading2 <- fp_text(font.size = 12, bold = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)
fp_note <- fp_text(font.size = 10, italic = TRUE, font.family = font_main, hansi.family = font_main, eastasia.family = font_cjk)

par_body <- fp_par(line_spacing = 1.5, word_style = NA_character_)
par_title <- fp_par(text.align = "center", line_spacing = 1.15, keep_with_next = TRUE, word_style = NA_character_)
par_heading <- fp_par(line_spacing = 1.15, keep_with_next = TRUE, word_style = NA_character_)
par_caption <- fp_par(line_spacing = 1.15, keep_with_next = TRUE, word_style = NA_character_)
par_note <- fp_par(line_spacing = 1.15, word_style = NA_character_)

add_text <- function(doc, text = "") {
  body_add_fpar(doc, fpar(ftext(text, prop = fp_body), fp_p = par_body))
}

add_label_text <- function(doc, label, text) {
  body_add_fpar(
    doc,
    fpar(
      ftext(paste0(label, ": "), prop = fp_body_bold),
      ftext(text, prop = fp_body),
      fp_p = par_body
    )
  )
}

add_heading1 <- function(doc, text) {
  body_add_fpar(doc, fpar(ftext(text, prop = fp_heading1), fp_p = par_heading))
}

add_heading2 <- function(doc, text) {
  body_add_fpar(doc, fpar(ftext(text, prop = fp_heading2), fp_p = par_heading))
}

add_caption <- function(doc, text) {
  body_add_fpar(doc, fpar(ftext(text, prop = fp_body), fp_p = par_caption))
}

add_note <- function(doc, text) {
  body_add_fpar(doc, fpar(ftext(text, prop = fp_note), fp_p = par_note))
}

add_title_line <- function(doc, text) {
  body_add_fpar(doc, fpar(ftext(text, prop = fp_title), fp_p = par_title))
}

add_title_meta <- function(doc, label, text) {
  body_add_fpar(
    doc,
    fpar(
      ftext(paste0(label, ": "), prop = fp_body_bold),
      ftext(text, prop = fp_body),
      fp_p = fp_par(line_spacing = 1.15, word_style = NA_character_)
    )
  )
}

add_bullet <- function(doc, text) {
  item <- list_item(fpar(ftext(text, prop = fp_body), fp_p = fp_par(line_spacing = 1.15, word_style = NA_character_)))
  body_add_list(doc, block_list_items(item, list_type = "bullet"))
}

add_blank <- function(doc) body_add_par(doc, "", style = "Normal")

make_ft <- function(dat, font_size = 8) {
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
  ft <- align(ft, j = seq_len(ncol(dat)), align = "left", part = "body")
  ft <- align(ft, j = intersect(2:ncol(dat), seq_len(ncol(dat))), align = "center", part = "body")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- padding(ft, padding.top = 3, padding.bottom = 3, padding.left = 4, padding.right = 4, part = "all")
  ft <- autofit(ft)
  set_table_properties(ft, layout = "autofit", width = 1)
}

add_table <- function(doc, dat, caption, note = NULL, font_size = 8) {
  doc <- add_caption(doc, caption)
  doc <- body_add_flextable(doc, make_ft(dat, font_size = font_size))
  if (!is.null(note) && nzchar(note)) doc <- add_note(doc, paste0("Note. ", note))
  add_blank(doc)
}

add_img <- function(doc, path, caption, width = 6.3, height = 3.0) {
  if (!file.exists(path)) return(doc)
  doc <- add_caption(doc, caption)
  doc <- body_add_img(doc, src = path, width = width, height = height)
  add_blank(doc)
}

model_label <- c(
  "Model 1: ATT weighted, no night-shift adjustment" = "M1: total role association",
  "Model 2: ATT weighted, plus night-shift adjustment" = "M2: plus night shift",
  "Model 3: day-shift-only ATT weighted" = "M3: day-shift only"
)

outcome_label <- c(
  psqi = "PSQI total",
  poor_sleep = "Poor sleep",
  pss = "PSS",
  mbi_ee = "MBI-EE",
  gad7 = "GAD-7",
  phq9 = "PHQ-9",
  turnover_intention = "Turnover intention",
  work_family_balance = "WFB",
  circadian_disruption_domain = "Circadian-disruption domain",
  management_strain_domain = "Managerial-strain domain"
)

fig_domain_manuscript <- file.path(base_dir, "figure_risk_domain_transition_manuscript.png")

make_domain_figure <- function(path) {
  model_short <- c(
    "Model 1: ATT weighted, no night-shift adjustment" = "M1 total",
    "Model 2: ATT weighted, plus night-shift adjustment" = "M2 + night shift",
    "Model 3: day-shift-only ATT weighted" = "M3 day-only"
  )
  offsets <- c("M1 total" = -0.16, "M2 + night shift" = 0, "M3 day-only" = 0.16)
  plot_dat <- results %>%
    filter(
      outcome %in% c("circadian_disruption_domain", "management_strain_domain"),
      model %in% names(model_short)
    ) %>%
    mutate(
      domain = ifelse(
        outcome == "management_strain_domain",
        "Managerial-strain risk domain",
        "Circadian-disruption risk domain"
      ),
      y_base = ifelse(outcome == "management_strain_domain", 2, 1),
      model_short = factor(unname(model_short[model]), levels = names(offsets)),
      y = y_base + unname(offsets[as.character(model_short)])
    )

  p <- ggplot(plot_dat, aes(color = model_short)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.4) +
    geom_segment(aes(x = conf_low, xend = conf_high, y = y, yend = y), linewidth = 0.65) +
    geom_point(aes(x = estimate, y = y), size = 2.4) +
    scale_color_manual(values = c("M1 total" = "#F8766D", "M2 + night shift" = "#00BA38", "M3 day-only" = "#619CFF")) +
    scale_y_continuous(
      breaks = c(1, 2),
      labels = c("Circadian-disruption risk domain", "Managerial-strain risk domain"),
      limits = c(0.65, 2.35)
    ) +
    scale_x_continuous(breaks = c(-0.1, 0, 0.1), limits = c(-0.13, 0.17)) +
    labs(x = "Head nurse minus staff nurse standardized domain difference", y = NULL, color = NULL) +
    theme_minimal(base_size = 10, base_family = font_main) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      axis.text.y = element_text(size = 9),
      panel.grid.minor = element_blank(),
      plot.margin = margin(6, 16, 6, 6)
    )
  ggsave(path, p, width = 6.6, height = 3.0, dpi = 300, bg = "white")
}

make_domain_figure(fig_domain_manuscript)

build_key_results_table <- function() {
  keep_outcomes <- c(
    "psqi", "poor_sleep", "circadian_disruption_domain",
    "management_strain_domain", "mbi_ee", "gad7",
    "turnover_intention", "work_family_balance"
  )
  keep_models <- c(m1, m2, m3)
  results %>%
    filter(outcome %in% keep_outcomes, model %in% keep_models) %>%
    mutate(
      Outcome = unname(outcome_label[outcome]),
      Model = unname(model_label[model]),
      Effect = ifelse(type == "binary", paste0("PR ", fmt_num(estimate, 2)), paste0("MD ", fmt_num(estimate, 2))),
      `95% CI` = fmt_ci(conf_low, conf_high, 2),
      P = fmt_p(p_value)
    ) %>%
    select(Outcome, Model, Effect, `95% CI`, P)
}

build_transition_table <- function() {
  keep <- c(
    "circadian_disruption_domain", "management_strain_domain",
    "psqi", "poor_sleep", "mbi_ee", "gad7", "turnover_intention",
    "work_family_balance"
  )
  signal <- c(
    "Apparent sleep/circadian advantage attenuates after night-shift accounting" = "Sleep/circadian advantage attenuates",
    "Managerial-strain signal emerges or strengthens after night-shift accounting" = "Managerial-strain signal emerges",
    "Work-family advantage reverses toward poorer balance after night-shift accounting" = "Work-family advantage reverses"
  )
  transition %>%
    filter(outcome %in% keep) %>%
    mutate(
      Outcome = unname(outcome_label[outcome]),
      M1 = ifelse(type == "binary", paste0("PR ", fmt_num(model1_estimate, 2)), fmt_num(model1_estimate, 2)),
      M2 = ifelse(type == "binary", paste0("PR ", fmt_num(model2_estimate, 2)), fmt_num(model2_estimate, 2)),
      M3 = ifelse(type == "binary", paste0("PR ", fmt_num(model3_estimate, 2)), fmt_num(model3_estimate, 2)),
      Signal = ifelse(interpretation %in% names(signal), unname(signal[interpretation]), interpretation)
    ) %>%
    select(Outcome, M1, M2, M3, Signal)
}

baseline_table <- baseline %>%
  transmute(
    Characteristic = row_label,
    `Head nurse` = head_nurse,
    `Staff nurse` = staff_nurse,
    `Test statistic` = test_statistic,
    P = p_value,
    `SMD before` = smd_before,
    `SMD after ATT` = smd_after_att
  )

schedule_start_idx <- which(trimws(baseline$row_label) == "Work schedule in past 6 months")[1]
schedule_end_idx <- which(trimws(baseline$row_label) == "Rotating day/night shifts")[1]

balance_table <- baseline_table %>%
  slice(seq_len(schedule_start_idx - 1))

schedule_table <- baseline %>%
  slice(schedule_start_idx:schedule_end_idx) %>%
  transmute(
    Characteristic = ifelse(row_type == "level", paste0("  ", trimws(row_label)), trimws(row_label)),
    `Head nurse` = head_nurse,
    `Staff nurse` = staff_nurse,
    `Test statistic` = test_statistic,
    P = p_value
  )

doc <- read_docx()
doc <- body_set_default_section(
  doc,
  prop_section(
    page_size = page_size(orient = "portrait"),
    page_margins = page_mar(top = 1, bottom = 1, left = 1, right = 1, header = 0.5, footer = 0.5)
  )
)

doc <- add_title_line(doc, main_title)
doc <- add_blank(doc)
doc <- add_title_meta(doc, "Running title", running_title)
doc <- add_title_meta(doc, "Article type", "Original research")
doc <- add_title_meta(doc, "Study design", "Cross-sectional analysis of baseline data from a cohort")
doc <- add_title_meta(doc, "Interpretive framing", interpretive_framing)
doc <- add_title_meta(doc, "Authors", "[Insert author names]")
doc <- add_title_meta(doc, "Affiliations", "[Insert institutional affiliations]")
doc <- add_title_meta(doc, "Corresponding author", "[Insert name, postal address, email, and telephone]")
doc <- add_title_meta(doc, "ORCID", "[Insert ORCID IDs where available]")
doc <- add_title_meta(doc, "Funding", "[Insert verified funding sources and grant numbers]")
doc <- add_title_meta(doc, "Competing interests", "[Insert statement]")
doc <- add_title_meta(doc, "Author contributions", "[Insert CRediT roles]")
doc <- add_note(doc, "Preparation note for authors: this draft uses local trial-run results. Replace all sample sizes, estimates, tables, figures, ethics/site details, and references with the final bastion/RStudio Server analysis before submission.")
doc <- body_add_break(doc)

doc <- add_title_line(doc, main_title)
doc <- add_heading1(doc, "Abstract")
doc <- add_label_text(doc, "Background", "Head nurses often move away from night-shift work as they assume managerial responsibilities. A baseline comparison with staff nurses may therefore show apparent sleep advantages while obscuring pressure-related occupational-health risks.")
doc <- add_label_text(doc, "Objective", "To examine whether head-nurse status was associated with a different occupational-health profile, framed as a possible shift from circadian-disruption burden toward managerial strain.")
doc <- add_label_text(doc, "Design", "Cross-sectional analysis of baseline data from a nurse health cohort.")
doc <- add_label_text(doc, "Participants", paste0("The analytic sample included ", fmt_n(n_analysis), " registered nurses after prespecified data cleaning, including ", fmt_n(n_head), " head nurses and ", fmt_n(n_staff), " staff nurses without administrative roles."))
doc <- add_label_text(doc, "Methods", "Head nurses were compared with staff nurses using average-treatment-effect-on-the-treated propensity-score weighting based on baseline demographic and occupational covariates. Because night-shift exposure may be part of the head-nurse role pathway, it was not included in the propensity-score model. Instead, three models were used: total role association, additional night-shift adjustment in the outcome model, and a day-shift-only restriction. Outcomes included sleep quality, daytime sleepiness, productivity loss, perceived stress, burnout, anxiety, depression, turnover intention, and work-family balance. Exploratory standardized circadian-disruption and managerial-strain domains summarized the expected risk profile.")
doc <- add_label_text(
  doc,
  "Results",
  paste0(
    "Day-shift-only work was more common among head nurses than staff nurses (",
    sprintf("%.1f", day_head), "% vs ", sprintf("%.1f", day_staff),
    "%). Propensity weighting reduced the maximum absolute standardized mean difference from ",
    sprintf("%.3f", max_smd_before), " to ", sprintf("%.3f", max_smd_after),
    ". In the total role model, head nurses had lower PSQI scores (MD ",
    fmt_num(r_psqi_m1$estimate, 2), "; 95% CI ", fmt_ci(r_psqi_m1$conf_low, r_psqi_m1$conf_high, 2),
    ") and lower poor-sleep prevalence (PR ", fmt_num(r_sleep_m1$estimate, 2), "; 95% CI ",
    fmt_ci(r_sleep_m1$conf_low, r_sleep_m1$conf_high, 2), "). The circadian-disruption domain advantage attenuated after night-shift accounting (MD ",
    fmt_num(r_circ_m1$estimate, 2), " in Model 1; ", fmt_num(r_circ_m2$estimate, 2),
    " in Model 2; ", fmt_num(r_circ_m3$estimate, 2), " in Model 3). Conversely, the managerial-strain domain became higher among head nurses after night-shift adjustment (MD ",
    fmt_num(r_mgmt_m2$estimate, 2), "; 95% CI ", fmt_ci(r_mgmt_m2$conf_low, r_mgmt_m2$conf_high, 2),
    ") and in the day-shift-only model (MD ", fmt_num(r_mgmt_m3$estimate, 2), "; 95% CI ",
    fmt_ci(r_mgmt_m3$conf_low, r_mgmt_m3$conf_high, 2), ")."
  )
)
doc <- add_label_text(doc, "Conclusions", "In this baseline cross-sectional analysis, lower night-shift exposure among head nurses should not be interpreted as broad occupational-health protection. The findings support a risk-profile interpretation in which apparent sleep advantages coexist with, and may mask, managerial-strain signals.")
doc <- add_label_text(doc, "Keywords", "head nurse; nurse manager; occupational health; baseline cross-sectional analysis; night shift; burnout; anxiety; turnover intention; propensity score")

doc <- add_heading2(doc, "What is already known")
for (x in c(
  "Shift and night work are associated with poorer sleep, fatigue, and psychological strain among nurses.",
  "Head nurses usually have fewer night-shift duties than staff nurses but carry greater managerial and supervisory responsibilities.",
  "Baseline cohort analyses must distinguish cross-sectional role associations from longitudinal risk estimates."
)) doc <- add_bullet(doc, x)

doc <- add_heading2(doc, "What this study adds")
for (x in c(
  "This analysis frames head-nurse occupational health as a risk-profile contrast rather than a simple healthier-versus-less-healthy comparison.",
  "A three-model strategy separates total role differences, night-shift-related explanation, and day-shift-only role contrasts.",
  "Apparent sleep/circadian advantages attenuated after night-shift accounting, while managerial-strain indicators became more visible."
)) doc <- add_bullet(doc, x)

doc <- add_heading1(doc, "Introduction")
intro <- c(
  "Nursing workforce sustainability depends not only on the number of nurses retained in clinical practice but also on the occupational health of nurses who assume leadership roles. Head nurses are central to ward-level quality, staffing coordination, patient safety, conflict management, and implementation of hospital policies. Yet their occupational-health profile is difficult to interpret because the role often combines reduced exposure to night shifts with increased managerial responsibility.",
  "Night-shift and rotating-shift work are established occupational exposures in nursing. They may disrupt sleep, circadian alignment, alertness, mood, and work performance. In a crude comparison, nurses who do not work night shifts may appear healthier on sleep-related outcomes. However, for head nurses, absence of night shift is not simply a confounder; it may be part of the managerial role itself.",
  "At the same time, managerial work introduces other demands. Head nurses may manage staffing shortages, patient and family complaints, ward quality indicators, team conflict, documentation requirements, and competing expectations from frontline staff and hospital leadership. These demands may be expressed less as circadian disruption and more as emotional exhaustion, anxiety, work-family imbalance, or turnover intention.",
  "For this reason, a baseline analysis of cohort data should avoid treating head-nurse status as a simple exposure whose effects can be separated from all role features. Instead, it can ask whether the observed occupational-health profile differs across domains. We therefore used a risk-profile framing: head nurses may show schedule-related sleep advantages while simultaneously showing managerial-strain signals.",
  "This study used baseline data from a nurse health cohort to compare head nurses with staff nurses without administrative roles. We estimated propensity-weighted associations after balancing baseline demographic and occupational covariates and handled night-shift exposure through a three-model strategy rather than including it directly in the propensity-score model. The objective was to determine whether head-nurse status was associated with a profile shift from circadian-disruption burden toward managerial strain."
)
for (x in intro) doc <- add_text(doc, x)
doc <- body_add_break(doc)
doc <- add_img(doc, file.path(base_dir, "figure_conceptual_risk_transition.png"), "Figure 1. Conceptual framework for interpreting head-nurse occupational-health profiles in baseline cohort data.", width = 6.4, height = 3.4)

doc <- add_heading1(doc, "Methods")
doc <- add_heading2(doc, "Study Design and Data Source")
doc <- add_text(doc, "This was a cross-sectional analysis of baseline data from the Chinese Nurses' Health Cohort Study (TARGET). Because only baseline questionnaire data were used in the present analysis, all exposure, covariate, and outcome measures were interpreted as baseline associations. The draft uses local trial-run output and should be updated with final database details, including exact baseline survey dates, participating hospitals, regions, and final ethics metadata before submission.")
doc <- add_heading2(doc, "Participants")
doc <- add_text(doc, paste0("The main analytic contrast was head nurses versus staff nurses without administrative roles. The local trial dataset contained ", fmt_n(flow[step == 'Raw records', n_remaining]), " raw records. Nurses with administrative-role values other than staff nurse or head nurse were excluded to keep the contrast interpretable. Additional quality-control rules excluded implausible age or work-tenure values and work tenure inconsistent with age. The final analytic sample included ", fmt_n(n_analysis), " nurses."))
doc <- add_heading2(doc, "Exposure and Work-Schedule Variables")
doc <- add_text(doc, "Head-nurse status was defined using the questionnaire administrative-position item. Head nurses were coded as the exposed group, and staff nurses without an administrative role were the reference group. Work schedule in the past 6 months was used to define day-shift-only work and night-shift exposure. Because schedule allocation may be a defining feature of the head-nurse role pathway, work schedule was displayed descriptively but was not included in the propensity-score model.")
doc <- add_heading2(doc, "Outcomes")
doc <- add_text(doc, "Baseline occupational-health outcomes included sleep quality measured by the Pittsburgh Sleep Quality Index (PSQI), poor sleep quality defined as PSQI >= 8, daytime sleepiness measured by the Epworth Sleepiness Scale, productivity loss measured by the Stanford Presenteeism Scale, perceived stress, Maslach Burnout Inventory dimensions, anxiety measured by the GAD-7, depression measured by the PHQ-9, turnover intention, and work-family balance. Scores outside theoretical ranges were set to missing before analysis.")
doc <- add_heading2(doc, "Risk-Domain Scores")
doc <- add_text(doc, "Two exploratory standardized domains were created to summarize the hypothesized profile. The circadian-disruption domain averaged standardized PSQI, daytime sleepiness, and productivity-loss scores. The managerial-strain domain averaged standardized perceived stress, emotional exhaustion, anxiety, turnover intention, and reversed work-family balance. Higher domain scores indicated worse occupational-health risk.")
doc <- add_heading2(doc, "Covariates and Missing Data")
doc <- add_text(doc, "Candidate propensity-score covariates were baseline or relatively stable variables, including age, work tenure, BMI, sex, education, marital status, department, employment type, professional title, sunlight exposure when available, smoking, drinking, and pregnancy status when available. Numeric covariates were median-imputed with missing indicators, and categorical covariates used an explicit missing category. Outcome models used outcome-specific complete cases after score-range checks.")
doc <- add_heading2(doc, "Statistical Analysis")
doc <- add_text(doc, "The main estimand was the average treatment effect on the treated. Head nurses received weight 1, and staff nurses received propensity-score odds weights PS/(1-PS), with weights capped at the 99th percentile. Covariate balance was assessed using standardized mean differences, with absolute values below 0.10 considered acceptable for measured propensity-score covariates. Continuous outcomes were analyzed with weighted linear regression and reported as mean differences. Binary outcomes were analyzed with weighted log-link quasi-Poisson models and reported as prevalence ratios.")
doc <- add_text(doc, "Three models were specified. Model 1 estimated the total baseline role association without night-shift adjustment. Model 2 added night-shift exposure to the outcome model to examine whether the total role association changed after accounting for work schedule. Model 3 restricted the sample to day-shift-only nurses, re-estimated propensity scores, and compared head nurses with staff nurses under a common no-night-shift schedule. Sensitivity analyses additionally adjusted weighted outcome models for baseline covariates.")
doc <- add_heading2(doc, "Ethics")
doc <- add_text(doc, "The original cohort protocol and baseline survey ethics approval, consent process, data-availability statement, funding, and author contributions should be inserted from the final TARGET project records before submission. The project draft materials indicate approval by the Medical Ethics Committee of Qilu Hospital, Shandong University, and electronic informed consent from participants; these details require final verification.")

doc <- add_heading1(doc, "Results")
doc <- add_heading2(doc, "Sample Flow and Baseline Role Structure")
doc <- add_text(doc, paste0("Of ", fmt_n(flow[step == 'Raw records', n_remaining]), " raw records, ", fmt_n(flow[step == 'Administrative position not A_q12=1 or A_q12=3', n_excluded_at_step]), " were excluded because the administrative-position item did not define either staff nurse without administrative role or head nurse. After age and work-tenure checks, ", fmt_n(n_analysis), " nurses remained, including ", fmt_n(n_head), " head nurses and ", fmt_n(n_staff), " staff nurses."))
doc <- add_heading2(doc, "Baseline Characteristics and Balance")
doc <- add_text(doc, paste0("Before weighting, head nurses were older, had longer work tenure, and were much more likely to hold senior professional titles and permanent employment. Among variables included in the propensity-score model, ATT weighting improved measured covariate balance, reducing the maximum absolute standardized mean difference from ", sprintf("%.3f", max_smd_before), " before weighting to ", sprintf("%.3f", max_smd_after), " after weighting. Work schedule was not included in this balance summary because it was treated as a role-pathway variable rather than a propensity-score covariate."))
doc <- add_table(
  doc,
  balance_table,
  "Table 1. Baseline characteristics and propensity-score covariate balance.",
  "Values are mean (SD) or n (%). ATT, average treatment effect on the treated; SMD, standardized mean difference; X2, chi-square statistic. This table excludes work schedule because work schedule was handled as a role-pathway variable rather than a propensity-score covariate.",
  font_size = 6.5
)

doc <- add_heading2(doc, "Work-Schedule Distribution")
doc <- add_text(doc, paste0("Work schedule differed sharply by role and was intentionally left outside the propensity-score model. Day-shift-only work was reported by ", sprintf("%.1f", day_head), "% of head nurses and ", sprintf("%.1f", day_staff), "% of staff nurses; rotating day/night shifts were reported by 5.6% of head nurses and 69.9% of staff nurses. These work-schedule differences motivate the night-shift-adjusted and day-shift-only models reported below."))
doc <- add_table(
  doc,
  schedule_table,
  "Table 2. Work-schedule distribution by head-nurse status.",
  "Values are n (%). Work schedule was displayed descriptively and was not balanced in the propensity-score model.",
  font_size = 7.2
)

doc <- add_heading2(doc, "Weighted Occupational-Health Associations")
doc <- add_text(doc, paste0(
  "In Model 1, preserving the real-world schedule difference, head nurses had lower PSQI scores (MD ",
  fmt_num(r_psqi_m1$estimate, 2), "; 95% CI ", fmt_ci(r_psqi_m1$conf_low, r_psqi_m1$conf_high, 2),
  "; P ", fmt_p(r_psqi_m1$p_value), ") and lower prevalence of poor sleep quality (PR ",
  fmt_num(r_sleep_m1$estimate, 2), "; 95% CI ", fmt_ci(r_sleep_m1$conf_low, r_sleep_m1$conf_high, 2),
  "; P ", fmt_p(r_sleep_m1$p_value), ")."
))
doc <- add_text(doc, paste0(
  "After night-shift adjustment in Model 2, the sleep advantage attenuated. Poor sleep quality was no longer clearly lower (PR ",
  fmt_num(r_sleep_m2$estimate, 2), "; 95% CI ", fmt_ci(r_sleep_m2$conf_low, r_sleep_m2$conf_high, 2),
  "), while emotional exhaustion (MD ", fmt_num(r_mbi_m2$estimate, 2), "; 95% CI ",
  fmt_ci(r_mbi_m2$conf_low, r_mbi_m2$conf_high, 2), "), anxiety (MD ",
  fmt_num(r_gad_m2$estimate, 2), "; 95% CI ", fmt_ci(r_gad_m2$conf_low, r_gad_m2$conf_high, 2),
  "), and turnover intention (MD ", fmt_num(r_to_m2$estimate, 2), "; 95% CI ",
  fmt_ci(r_to_m2$conf_low, r_to_m2$conf_high, 2), ") were higher among head nurses."
))
doc <- add_text(doc, paste0(
  "In the day-shift-only analysis (", fmt_n(n_day), " nurses; ", fmt_n(n_day_head), " head nurses and ", fmt_n(n_day_staff),
  " staff nurses), balance remained acceptable after weighting (maximum absolute SMD ", sprintf("%.3f", max_smd_day),
  "). Emotional exhaustion (MD ", fmt_num(r_mbi_m3$estimate, 2), "; 95% CI ", fmt_ci(r_mbi_m3$conf_low, r_mbi_m3$conf_high, 2),
  "), anxiety (MD ", fmt_num(r_gad_m3$estimate, 2), "; 95% CI ", fmt_ci(r_gad_m3$conf_low, r_gad_m3$conf_high, 2),
  "), and turnover intention (MD ", fmt_num(r_to_m3$estimate, 2), "; 95% CI ", fmt_ci(r_to_m3$conf_low, r_to_m3$conf_high, 2),
  ") remained higher among head nurses."
))
doc <- add_table(
  doc,
  build_key_results_table(),
  "Table 3. Weighted associations between head-nurse status and key occupational-health outcomes.",
  "M1 estimates the total baseline role association; M2 additionally adjusts the outcome model for night shift; M3 restricts the analysis to day-shift-only nurses. MD, mean difference; PR, prevalence ratio.",
  font_size = 7.2
)

doc <- add_heading2(doc, "Risk-Domain Transition")
doc <- add_text(doc, paste0(
  "The domain-level analysis clarified the profile shift. The circadian-disruption domain was lower among head nurses in Model 1 (MD ",
  fmt_num(r_circ_m1$estimate, 2), "; 95% CI ", fmt_ci(r_circ_m1$conf_low, r_circ_m1$conf_high, 2),
  ") but was near null after night-shift accounting (Model 2 MD ", fmt_num(r_circ_m2$estimate, 2),
  "; Model 3 MD ", fmt_num(r_circ_m3$estimate, 2), "). In contrast, the managerial-strain domain was near null in Model 1 (MD ",
  fmt_num(r_mgmt_m1$estimate, 2), ") but higher among head nurses in Model 2 (MD ", fmt_num(r_mgmt_m2$estimate, 2),
  "; 95% CI ", fmt_ci(r_mgmt_m2$conf_low, r_mgmt_m2$conf_high, 2), ") and Model 3 (MD ",
  fmt_num(r_mgmt_m3$estimate, 2), "; 95% CI ", fmt_ci(r_mgmt_m3$conf_low, r_mgmt_m3$conf_high, 2), ")."
))
doc <- add_table(doc, build_transition_table(), "Table 4. Risk-profile transition across total, night-shift-adjusted, and day-shift-only models.", "For continuous outcomes and domains, values are mean differences; for poor sleep, values are prevalence ratios. Figure 2 displays the two domain-level rows from this table.", font_size = 7.2)
doc <- add_img(doc, fig_domain_manuscript, "Figure 2. Domain-level occupational-health profile across the three baseline cross-sectional models.", width = 6.4, height = 2.8)

doc <- body_add_break(doc)
doc <- add_heading1(doc, "Discussion")
discussion <- c(
  "In this baseline cross-sectional analysis of nurse cohort data, head nurses differed markedly from staff nurses in work schedule and occupational-health profile. The total role comparison suggested better sleep-related outcomes among head nurses, but this apparent advantage was strongly linked to their lower night-shift exposure. Once night shift was accounted for analytically or removed by restricting the sample to day-shift-only nurses, managerial-strain signals became more visible.",
  "These findings should not be interpreted as longitudinal evidence that becoming a head nurse causes later health changes. Rather, they describe baseline differences in occupational-health profiles between nurses occupying different roles. The design is useful for identifying role-related patterns and generating hypotheses, but the temporal sequence between role transition and health outcomes cannot be established from baseline data alone.",
  "The results are consistent with the idea that occupational-health risks may shift in form across nursing roles. Staff nurses remain heavily exposed to rotating or night-shift schedules, which are plausibly reflected in sleep and circadian outcomes. Head nurses, by contrast, are less exposed to night shift but may carry higher demands related to leadership, staffing decisions, performance accountability, conflict management, and work-family boundaries. This may explain why emotional exhaustion, anxiety, turnover intention, and the managerial-strain domain were higher after schedule differences were handled.",
  "For nursing management, the practical implication is that absence of night shift should not be used as a simple marker of occupational-health protection. Interventions for head nurses should address administrative workload, leadership support, role clarity, staffing resources, emotional labor, and early detection of burnout and turnover intention. Support strategies for staff nurses and head nurses may need to differ because the dominant risk pathways differ.",
  "This analysis has several strengths. It used a large baseline dataset, explicit cleaning rules, a clinically interpretable role contrast, propensity-score weighting, balance diagnostics, a three-model schedule strategy, and both individual outcomes and profile-level domains. The analytic approach also followed the writing requirement for cohort-derived baseline data by consistently presenting the analysis as a baseline cross-sectional comparison.",
  "Limitations should be emphasized. The baseline cross-sectional design precludes causal inference and cannot establish whether role status preceded health outcomes. Residual confounding is possible, especially for unit-level workload, staffing ratios, hospital culture, span of control, leadership support, and prior health status. Measures were self-reported, and the managerial-strain domain was exploratory. Work schedule was intentionally not balanced in the propensity-score model, so the total role association and schedule-adjusted association answer different questions. Finally, the present document uses local trial-run results that must be replaced by the final bastion analysis before submission."
)
for (x in discussion) doc <- add_text(doc, x)

doc <- add_heading1(doc, "Conclusions")
doc <- add_text(doc, "In baseline cohort data, head nurses showed an apparent sleep/circadian advantage largely aligned with lower night-shift exposure. After accounting for schedule differences, a managerial-strain profile became more apparent, particularly emotional exhaustion, anxiety, turnover intention, and the managerial-strain domain. These findings support a role-profile interpretation: head nurses may experience less circadian burden but more managerial strain. Longitudinal follow-up is needed to examine whether role transitions predict subsequent changes in occupational health.")

doc <- add_heading1(doc, "Declarations")
declarations <- c(
  "Ethics approval and consent to participate: Insert the verified ethics approval number, approving committee, and consent procedure from the final TARGET records.",
  "Consent for publication: Not applicable unless identifiable material is included.",
  "Availability of data and materials: Insert the final data-sharing statement approved by the TARGET study team.",
  "Competing interests: The authors should declare whether they have competing interests.",
  "Funding: Insert verified funding sources and grant numbers.",
  "Authors' contributions: Insert author initials and CRediT roles.",
  "Acknowledgements: The authors should acknowledge participating nurses, hospitals, and the TARGET project team.",
  "Use of generative AI: AI assistance was used to prepare an initial manuscript draft and formatting plan under author supervision. All scientific content, analyses, references, and final wording must be verified by the authors before submission."
)
for (x in declarations) doc <- add_text(doc, x)

doc <- add_heading1(doc, "References to Verify Before Submission")
references <- c(
  "International Journal of Nursing Studies. Guide for Authors. Elsevier.",
  "von Elm E, Altman DG, Egger M, Pocock SJ, Gotzsche PC, Vandenbroucke JP. The Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) statement.",
  "Sickness presenteeism, job burnout, social support and health-related productivity loss among nurses in the Chinese Nurses' Health Cohort Study (TARGET): A cross-sectional survey. International Journal of Nursing Studies. 2025;162:104962.",
  "Perceived stress, sickness presenteeism, job burnout, and turnover intention among nurses: A cross-sectional survey. Nursing Outlook. 2026;74:102650.",
  "Predictors of shift work sleep disorder among nurses during the COVID-19 pandemic: a multicenter cross-sectional study. Frontiers in Public Health. 2021.",
  "Rosenbaum PR, Rubin DB. The central role of the propensity score in observational studies for causal effects.",
  "Austin PC. An introduction to propensity score methods for reducing the effects of confounding in observational studies.",
  "Demerouti E, Bakker AB, Nachreiner F, Schaufeli WB. The Job Demands-Resources model of burnout.",
  "Hobfoll SE. Conservation of resources: A new attempt at conceptualizing stress.",
  "Buysse DJ, Reynolds CF, Monk TH, Berman SR, Kupfer DJ. The Pittsburgh Sleep Quality Index.",
  "Johns MW. A new method for measuring daytime sleepiness: the Epworth Sleepiness Scale.",
  "Cohen S, Kamarck T, Mermelstein R. A global measure of perceived stress.",
  "Maslach C, Jackson SE, Leiter MP. Maslach Burnout Inventory.",
  "Spitzer RL, Kroenke K, Williams JBW, Lowe B. A brief measure for assessing generalized anxiety disorder.",
  "Kroenke K, Spitzer RL, Williams JBW. The PHQ-9."
)
for (x in references) doc <- add_text(doc, x)

print(doc, target = out_docx)
cat(out_docx, "\n")
