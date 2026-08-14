source(file.path(PIPELINE_ROOT, "R", "00_utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages({library(data.table); library(ggplot2)})

obj <- readRDS(out_path("04_association", "model_objects.rds"))
dat <- as.data.table(obj$analysis_data)
class_levels <- obj$class_levels
K <- length(class_levels)
dat[, class_modal := factor(class_modal, levels = class_levels)]

fmt_cont <- function(x) {
  z <- x[!is.na(x)]; n <- length(z)
  if (!n) return("—")
  if (abs(psych::skew(z)) > 1) sprintf("%.1f (%.1f, %.1f); n=%d", median(z), quantile(z, .25), quantile(z, .75), n)
  else sprintf("%.1f (%.1f); n=%d", mean(z), sd(z), n)
}
fmt_cat <- function(x, level) {
  den <- sum(!is.na(x)); num <- sum(x == level, na.rm = TRUE)
  if (!den) "—" else sprintf("%d/%d (%.1f%%)", num, den, 100 * num / den)
}

table1_rows <- list(); idx <- 0L
add_cont <- function(label, variable) {
  cells <- c(fmt_cont(dat[[variable]]), vapply(class_levels, function(g) fmt_cont(dat[class_modal == g][[variable]]), character(1)))
  idx <<- idx + 1L
  table1_rows[[idx]] <<- as.data.table(as.list(c(characteristic = label, level = "", overall = cells[1], setNames(cells[-1], class_levels))))
}
add_cat <- function(label, variable) {
  x <- droplevels(factor(dat[[variable]]))
  for (j in seq_along(levels(x))) {
    lev <- levels(x)[j]
    cells <- c(fmt_cat(x, lev), vapply(class_levels, function(g) fmt_cat(x[dat$class_modal == g], lev), character(1)))
    idx <<- idx + 1L
    table1_rows[[idx]] <<- as.data.table(as.list(c(characteristic = if (j == 1) label else "", level = lev,
                                                    overall = cells[1], setNames(cells[-1], class_levels))))
  }
}

add_cont("年龄，岁", "age")
add_cont("BMI，kg/m²", "bmi")
add_cat("教育程度", "education")
add_cat("婚姻状况", "married")
add_cat("科室", "department")
add_cat("专业技术职称", "title_group")
add_cat("管理岗位", "management")
add_cat("吸烟", "smoking")
add_cat("饮酒", "drinking")
add_cat("慢性病", "chronic")
add_cat("排班", "night_shift")
add_cont("过去1个月每周>40 h的周数", "overtime_weeks")
table1 <- rbindlist(table1_rows, fill = TRUE)

fit <- fread(out_path("03_lca", "model_fit_1_to_8.csv"), encoding = "UTF-8")
table2 <- fit[, .(
  `类别数` = classes, `对数似然` = log_likelihood, `参数数` = parameters,
  AIC, BIC, aBIC, `熵` = entropy, `最小类别比例(%)` = 100 * min_estimated_class_prop,
  `最小AvePP` = min_AvePP, `选择` = selected
)]

means <- fread(out_path("04_association", "adjusted_marginal_means.csv"), encoding = "UTF-8")
diffs <- fread(out_path("04_association", "main_model.csv"), encoding = "UTF-8")
global <- fread(out_path("04_association", "global_wald_test.csv"), encoding = "UTF-8")
table3 <- merge(means[, .(class, n, adjusted_mean, mean_lower = lower, mean_upper = upper)],
  rbind(data.table(class = class_levels[1], estimate = 0, lower = NA_real_, upper = NA_real_,
                   p_value = NA_real_, standardized_effect = 0),
        diffs[, .(class, estimate, lower, upper, p_value, standardized_effect)]),
  by = "class", all.x = TRUE, sort = FALSE)
setnames(table3, c("class", "n", "adjusted_mean", "mean_lower", "mean_upper", "estimate", "lower", "upper", "p_value", "standardized_effect"),
  c("LUTS类别", "n", "调整后SPS-6均值", "均值95%CI下限", "均值95%CI上限", "较参照类别调整均值差",
    "差值95%CI下限", "差值95%CI上限", "P值", "标准化效应量"))

write_csv_utf8(table1, out_path("06_tables", "Table1_basic_characteristics.csv"))
write_csv_utf8(table2, out_path("06_tables", "Table2_LCA_core_fit.csv"))
write_csv_utf8(table3, out_path("06_tables", "Table3_LCA_SPS6_association.csv"))
warning_banner <- if (CFG$run_mode == "test") "TEST DATA — PIPELINE VALIDATION ONLY — DO NOT SUBMIT" else NULL
write_workbook(out_path("06_tables", "Main_Tables_LUTS.xlsx"), list(
  `Table1` = table1, `Table2` = table2, `Table3` = table3,
  `Table3_global_test` = global
), warning_banner = warning_banner)

profiles <- fread(out_path("03_lca", "class_profiles_full.csv"), encoding = "UTF-8")
expected <- fread(out_path("03_lca", "class_expected_scores.csv"), encoding = "UTF-8")
assignments <- fread(out_path("03_lca", "class_assignment_summary.csv"), encoding = "UTF-8")
local_dep <- fread(out_path("03_lca", "local_dependence_full.csv"), encoding = "UTF-8")
sens <- fread(out_path("05_sensitivity", "sensitivity_effects_full.csv"), encoding = "UTF-8")
sens_global <- fread(out_path("05_sensitivity", "sensitivity_global_tests_full.csv"), encoding = "UTF-8")
quality <- fread(out_path("01_data", "quality_flag_summary.csv"), encoding = "UTF-8")
missingness <- fread(out_path("04_association", "covariate_missingness.csv"), encoding = "UTF-8")
diagnostics <- fread(out_path("04_association", "model_diagnostics.csv"), encoding = "UTF-8")
vif <- fread(out_path("04_association", "multicollinearity_vif.csv"), encoding = "UTF-8")
comparison <- fread(out_path("04_association", "continuous_vs_lca_model_comparison.csv"), encoding = "UTF-8")
strategy <- fread(out_path("04_association", "analysis_strategy_comparison.csv"), encoding = "UTF-8")
binary_fit <- fread(out_path("05_sensitivity", "binary_lca_fit_1_to_8.csv"), encoding = "UTF-8")
binary_profiles <- fread(out_path("05_sensitivity", "binary_lca_profiles_full.csv"), encoding = "UTF-8")
binary_assign <- fread(out_path("05_sensitivity", "binary_lca_class_summary.csv"), encoding = "UTF-8")
binary_compare <- fread(out_path("05_sensitivity", "binary_lca_comparison.csv"), encoding = "UTF-8")
sps_summary <- fread(out_path("02_scales", "sps6_audit_summary.csv"), encoding = "UTF-8")

write_workbook(out_path("outputs", "Supplementary_Tables_LUTS.xlsx"), list(
  `S1_full_LCA_fit` = fit,
  `S2_class_parameters` = assignments,
  `S3_conditional_probabilities` = profiles,
  `S4_expected_scores` = expected,
  `S5_local_dependence` = local_dep,
  `S6_missingness` = missingness,
  `S7_quality_audit` = quality,
  `S8_sensitivity_effects` = sens,
  `S9_sensitivity_global` = sens_global,
  `S10_binary_fit` = binary_fit,
  `S11_binary_profiles` = binary_profiles,
  `S12_binary_classes` = binary_assign,
  `S13_binary_comparison` = binary_compare,
  `S14_continuous_comparison` = comparison,
  `S15_strategy` = strategy,
  `S16_model_diagnostics` = diagnostics,
  `S17_VIF` = vif,
  `S18_SPS6_summary` = sps_summary
), warning_banner = warning_banner)

# Publication figures.
base_theme <- theme_minimal(base_size = 10.5, base_family = "Microsoft YaHei") +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = .5, face = "bold"),
        strip.text = element_text(face = "bold"), legend.position = "top")

flow <- fread(out_path("00_audit", "sample_flow.csv"), encoding = "UTF-8")
flow_plot <- flow[!is.na(n)]
flow_stage_zh <- c(
  "Source records" = "原始问卷记录",
  "Nonmissing unique ID after test handling" = "具有非缺失唯一ID的记录",
  "Any male LUTS module response" = "填写男性LUTS模块任一条目",
  "All 13 LUTS items nonmissing" = "13项LUTS条目均非缺失",
  "All 13 LUTS items valid 0-4" = "13项LUTS条目均为合法0～4值",
  "SPS-6 complete" = "SPS-6六个条目完整",
  "Primary-model complete cases (pending)" = "主分析完整病例"
)
flow_plot[, stage_display := unname(flow_stage_zh[stage])]
flow_plot[is.na(stage_display), stage_display := stage]
flow_plot[, y := rev(seq_len(.N))]
flow_plot[, label := paste0(stage_display, "\nn=", format(n, big.mark = ","))]
flow_arrows <- if (nrow(flow_plot) > 1L) flow_plot[-nrow(flow_plot)] else flow_plot[0]
p_flow <- ggplot(flow_plot, aes(x = 1, y = y)) +
  geom_label(aes(label = label), family = "Microsoft YaHei", size = 3.4, label.padding = unit(.35, "lines"),
             fill = "white", color = "black") +
  geom_segment(data = flow_arrows, aes(x = 1, xend = 1, y = y - .30, yend = y - .70),
               arrow = arrow(length = unit(.15, "cm")), linewidth = .5) +
  xlim(.4, 1.6) + ylim(.4, max(flow_plot$y) + .6) + theme_void(base_family = "Microsoft YaHei") +
  labs(title = if (CFG$run_mode == "test") "图1 测试数据分析样本流程（禁止投稿）" else "图1 研究对象筛选流程") +
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 12))
save_plot_set(p_flow, "Figure1_sample_flow", 7.0, 7.5)

expected[, symptom := factor(symptom, levels = unname(CFG$luts_labels_zh[CFG$variables$luts]))]
expected[, class_label := factor(class_label, levels = class_levels)]
expected[, dimension := factor(dimension, levels = c("储尿症状", "排尿症状", "排尿后症状"))]
p_heat <- ggplot(expected, aes(symptom, class_label, fill = expected_score)) +
  geom_tile(color = "white", linewidth = .35) +
  geom_text(aes(label = sprintf("%.2f", expected_score)), family = "Microsoft YaHei", size = 2.8) +
  facet_grid(. ~ dimension, scales = "free_x", space = "free_x") +
  scale_fill_gradientn(colors = c("#F7FBFF", "#9ECAE1", "#3182BD", "#08306B"), limits = c(0, 4)) +
  labs(x = NULL, y = NULL, fill = "期望得分",
       title = if (CFG$run_mode == "test") "图2 LUTS类别条目期望得分热图（测试数据）" else "图2 LUTS类别条目期望得分热图") +
  base_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot_set(p_heat, "Figure2_LCA_profile_heatmap", 10.5, 4.8)

spline_pred <- fread(out_path("04_association", "continuous_luts_spline_predictions.csv"), encoding = "UTF-8")
p_spline <- ggplot(spline_pred, aes(luts_total, adjusted_sps6)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9ECAE1", alpha = .45) +
  geom_line(color = "#08519C", linewidth = .9) +
  labs(x = "LUTS连续总分", y = "调整后SPS-6预测得分",
       title = if (CFG$run_mode == "test") "图3 LUTS总分与SPS-6的调整剂量-反应曲线（测试数据）" else "图3 LUTS总分与SPS-6的调整剂量-反应曲线") +
  base_theme
save_plot_set(p_spline, "Figure3_continuous_LUTS_spline", 7.0, 5.2)

selected <- fit[selected == TRUE][1]
current_sps <- sps_summary[scheme == CFG$sps6_confirmed_scheme & !is.na(alpha)][1]
agreement <- sps_summary[scheme == CFG$sps6_confirmed_scheme & !is.na(exact_match_n)][1]
sex_cross <- fread(out_path("00_audit", "sex_module_crosstab.csv"), encoding = "UTF-8")
model_status <- readLines(out_path("03_lca", "MODEL_STATUS.txt"), warn = FALSE)

payload <- list(
  pipeline_version = CFG$pipeline_version,
  run_mode = CFG$run_mode,
  test_warning = CFG$run_mode == "test",
  population_term_zh = if (CFG$run_mode == "production" && CFG$male_module_ownership_confirmed) "男护士" else "男性LUTS模块完成记录",
  population_term_en = if (CFG$run_mode == "production" && CFG$male_module_ownership_confirmed) "male nurses" else "records completing the male LUTS module",
  source_n = flow[step == 1, n], module_n = flow[stage == "All 13 LUTS items valid 0-4", n],
  regression_n = global$n[1], selected_k = selected$classes,
  selected_model = as.list(selected), model_status = model_status,
  class_assignments = assignments, adjusted_means = means, adjusted_differences = diffs,
  global_test = global, continuous_model_comparison = comparison,
  continuous_linear_effect = fread(out_path("04_association", "continuous_luts_linear_effect.csv"), encoding = "UTF-8"),
  continuous_dimension_effects = fread(out_path("04_association", "continuous_luts_dimension_effects.csv"), encoding = "UTF-8"),
  spline_global = fread(out_path("04_association", "continuous_luts_spline_global_test.csv"), encoding = "UTF-8"),
  strategy = strategy, sps6_summary = current_sps, sps6_source_agreement = agreement,
  quality_flags = quality, binary_comparison = binary_compare,
  no_hospital_id = TRUE,
  placeholders = list(authors = "【待补充：作者姓名及排序】", affiliations = "【待补充：作者单位、城市及邮政编码】",
                      corresponding = "【待补充：通讯作者姓名及E-mail】", funding = "【待补充：基金项目名称及编号】",
                      ethics = "【待补充：伦理委员会全称、批准日期和批件号】",
                      consent = "【待补充：电子知情同意取得方式】", conflict = "【待补充：利益冲突声明】",
                      availability = "【待补充：数据可获得性说明】", ai = "【待目标期刊政策与实际使用情况确认后补充生成式AI声明】")
)
jsonlite::write_json(payload, out_path("_work", "manuscript_payload.json"), pretty = TRUE,
                     auto_unbox = TRUE, dataframe = "rows", na = "null", digits = 12)
log_msg("INFO", "Tables, figures, and manuscript payload generated.")
