source(file.path(PIPELINE_ROOT, "R", "00_utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages({library(data.table); library(officer); library(flextable)})

payload <- jsonlite::read_json(out_path("_work", "manuscript_payload.json"), simplifyVector = TRUE)
reference_docx <- resolve_path(CFG$reference_docx)
template_available <- !is.na(reference_docx) && file.exists(reference_docx)
ref <- if (template_available) officer::read_docx(reference_docx) else officer::read_docx()
ref_summary <- officer::docx_summary(ref)
reference_paragraphs <- ref_summary[ref_summary$content_type == "paragraph", "text", drop = TRUE]
ref_idx <- match("参考文献", trimws(reference_paragraphs))
references <- if (!is.na(ref_idx) && ref_idx < length(reference_paragraphs)) {
  reference_paragraphs[(ref_idx + 1L):length(reference_paragraphs)]
} else character()
references <- references[nzchar(trimws(references))]

clear_body <- function(doc) {
  for (i in seq_len(1000L)) {
    doc <- officer::cursor_begin(doc)
    current <- tryCatch(officer:::docx_current_block_xml(doc), error = function(e) NULL)
    if (is.null(current)) break
    doc <- officer::body_remove(doc)
  }
  doc
}

styles <- officer::styles_info(ref)
style_pick <- function(preferred, fallback = "Normal") {
  hit <- styles$style_name[tolower(styles$style_name) == tolower(preferred)]
  if (length(hit)) hit[1] else fallback
}
h1 <- style_pick("heading 1"); h2 <- style_pick("heading 2"); h3 <- style_pick("heading 3")

add_p <- function(doc, text, bold = FALSE, color = "000000", size = 10.5,
                  align = "left", before = 0, after = 6) {
  if (grepl("^[0-9A-Fa-f]{6}$", color)) color <- paste0("#", color)
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(text,
    officer::fp_text(font.family = "Microsoft YaHei", font.size = size,
                     bold = bold, color = color)),
    fp_p = officer::fp_par(text.align = align, padding = 0)), pos = "after")
}
add_heading <- function(doc, text, level = 1) {
  officer::body_add_par(doc, text, style = c(h1, h2, h3)[level], pos = "after")
}
add_ft <- function(doc, df, font_size = 8, widths = NULL, max_width = 6.35) {
  ft <- flextable::flextable(as.data.frame(df))
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = "Microsoft YaHei", part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft, bg = "#D9EAF7", part = "header")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft <- flextable::autofit(ft)
  if (!is.null(widths)) ft <- flextable::width(ft, j = seq_along(widths), width = widths)
  if (!is.null(max_width)) ft <- flextable::fit_to_width(ft, max_width = max_width)
  flextable::body_add_flextable(doc, ft, align = "center")
}

fmt <- function(x, d = 2) ifelse(is.na(x), "—", sprintf(paste0("%.", d, "f"), x))
ptext <- function(p) ifelse(is.na(p), "—", ifelse(p < .001, "P＜0.001", paste0("P=", sprintf("%.3f", p))))
test_banner <- if (CFG$run_mode == "test") "【测试数据工作稿，禁止投稿；全部数值仅用于验证分析流程】" else ""

fit <- fread(out_path("03_lca", "model_fit_1_to_8.csv"), encoding = "UTF-8")
sel <- fit[selected == TRUE][1]
assignments <- fread(out_path("03_lca", "class_assignment_summary.csv"), encoding = "UTF-8")
means <- fread(out_path("04_association", "adjusted_marginal_means.csv"), encoding = "UTF-8")
diffs <- fread(out_path("04_association", "main_model.csv"), encoding = "UTF-8")
global <- fread(out_path("04_association", "global_wald_test.csv"), encoding = "UTF-8")
comparison <- fread(out_path("04_association", "continuous_vs_lca_model_comparison.csv"), encoding = "UTF-8")
linear <- fread(out_path("04_association", "continuous_luts_linear_effect.csv"), encoding = "UTF-8")
dims <- fread(out_path("04_association", "continuous_luts_dimension_effects.csv"), encoding = "UTF-8")
spline_global <- fread(out_path("04_association", "continuous_luts_spline_global_test.csv"), encoding = "UTF-8")
strategy <- fread(out_path("04_association", "analysis_strategy_comparison.csv"), encoding = "UTF-8")
quality <- fread(out_path("01_data", "quality_flag_summary.csv"), encoding = "UTF-8")
binary <- fread(out_path("05_sensitivity", "binary_lca_comparison.csv"), encoding = "UTF-8")
sps_audit <- fread(out_path("02_scales", "sps6_audit_summary.csv"), encoding = "UTF-8")
sps_current <- sps_audit[scheme == CFG$sps6_confirmed_scheme & !is.na(alpha)][1]
sps_agree <- sps_audit[scheme == CFG$sps6_confirmed_scheme & !is.na(exact_match_n)][1]
flow <- fread(out_path("00_audit", "sample_flow.csv"), encoding = "UTF-8")
source_n <- flow[step == 1, n]
module_n <- flow[stage == "All 13 LUTS items valid 0-4", n]
reg_n <- global$n[1]
population <- if (CFG$run_mode == "production" && CFG$male_module_ownership_confirmed) "男护士" else "男性LUTS模块完成记录"
model_status <- trimws(readLines(out_path("03_lca", "MODEL_STATUS.txt"), warn = FALSE)[1])
recommend_continuous <- tolower(as.character(strategy[metric == "recommend_continuous_primary_story", value])) == "true"

class_sentence <- paste(vapply(seq_len(nrow(assignments)), function(i) sprintf(
  "%s估计比例%.1f%%、模态分类n=%d、AvePP=%.3f", assignments$class_label[i],
  100 * assignments$estimated_proportion[i], assignments$modal_n[i], assignments$AvePP[i]), character(1)), collapse = "；")
mean_sentence <- paste(vapply(seq_len(nrow(means)), function(i) sprintf(
  "%s为%.2f分（95%%CI %.2f～%.2f）", means$class[i], means$adjusted_mean[i], means$lower[i], means$upper[i]), character(1)), collapse = "；")
diff_sentence <- paste(vapply(seq_len(nrow(diffs)), function(i) sprintf(
  "%s的调整均值差为%.2f分（95%%CI %.2f～%.2f，%s）", diffs$class[i], diffs$estimate[i],
  diffs$lower[i], diffs$upper[i], ptext(diffs$p_value[i])), character(1)), collapse = "；")

doc <- clear_body(if (template_available) officer::read_docx(reference_docx) else officer::read_docx())
doc <- add_p(doc, "男性下尿路症状潜在类别与工作生产力损失", bold = TRUE, size = 18, align = "center", after = 4)
doc <- add_p(doc, "基于TARGET护士队列的横断面分析工作稿", bold = TRUE, size = 13, align = "center")
if (nzchar(test_banner)) doc <- add_p(doc, test_banner, bold = TRUE, color = "C00000", size = 12, align = "center")
for (x in unlist(payload$placeholders[c("authors", "affiliations", "corresponding", "funding")])) doc <- add_p(doc, x, bold = TRUE, color = "7F6000")

doc <- add_heading(doc, "摘要", 1)
source_wording <- if (CFG$run_mode == "test") "测试宽表记录" else "正式原始问卷记录"
result_wording <- if (CFG$run_mode == "test") "当前测试数据" else "正式数据"
abstract_conclusion <- if (CFG$run_mode == "test") {
  "本文全部结果仅用于流程测试。正式数据运行后，应根据症状模式、局部依赖、类别稳定性及连续模型增量价值决定保留LCA主线或改为剂量-反应主线。"
} else {
  "LUTS类别与SPS-6得分的关联及LCA主线价值，应结合症状模式、局部依赖、类别稳定性和连续模型增量价值综合解释；横断面结果不作因果推断。"
}
abstract <- sprintf(paste0(
  "目的 识别%s的下尿路症状（LUTS）潜在类别，比较LCA与连续症状负担对工作生产力损失的解释价值。",
  "方法 本横断面分析从%d条%s中纳入13项ICIQ-MLUTS条目均为0～4合法值的%d条记录。",
  "以多分类条目拟合1～8类模型，使用至少200个随机初始值并对不稳定模型自动增加至500或1 000个。",
  "以SPS-6总分为结局，主要分析采用1 000次后验概率伪类别抽样、Rubin法则和HC3稳健标准误；",
  "同时比较LUTS总分线性、自然样条、症状维度及LCA类别模型。结果 %s选择%d类模型，熵为%.3f，最小类别比例为%.1f%%，模型状态为%s。",
  "主关联分析纳入%d条记录，类别整体Wald检验%s。连续LUTS总分每增加1分与SPS-6改变%.2f分相关（95%%CI %.2f～%.2f）。",
  "结论 %s"),
  population, source_n, source_wording, module_n, result_wording, sel$classes, sel$entropy, 100 * sel$min_estimated_class_prop,
  model_status, reg_n, ptext(global$p_value[1]), linear$estimate[1], linear$lower[1], linear$upper[1], abstract_conclusion)
doc <- add_p(doc, abstract)
doc <- add_p(doc, "关键词 护士；下尿路症状；潜在类别分析；工作生产力损失；限制性立方样条；横断面研究")

doc <- add_heading(doc, "1 前言", 1)
intro <- c(
  "下尿路症状（lower urinary tract symptoms，LUTS）包括储尿、排尿和排尿后症状，可同时出现并形成不同的症状组合。护士群体的工作节奏、隐私顾虑和职业暴露具有特殊性，但现有护士LUTS研究主要集中于女性，男护士及男性专属症状模块的证据明显不足［1-3］。",
  "轮班工作、连续照护任务、延迟排尿和饮水受限可能增加尿路症状负担。夜尿、尿急和排尿困难还可能通过睡眠中断、持续不适、注意力分散及工作中断，与带病出勤和工作生产力下降相关；横断面资料同时可能存在反向路径和共同原因，因而只能解释为关联［7-10］。",
  "传统总分便于量化总体严重程度，却可能掩盖储尿、排尿及排尿后症状的共存模式。潜在类别分析从个体层面识别症状组合，可能为职业健康筛查提供更具体的模式信息；但如果类别仅表现为平行的严重程度梯度，或者相较连续总分没有增量解释价值，强行使用类别标签反而会损失信息。",
  "本研究在统一的数据核验和计分关口后，拟合多分类LCA并分析其与SPS-6得分的关联，同时以连续LUTS总分、自然样条和症状维度模型作为对照，以判断人群中心分类是否提供超出连续症状负担的价值。"
)
for (x in intro) doc <- add_p(doc, x)

doc <- add_heading(doc, "2 对象与方法", 1)
doc <- add_heading(doc, "2.1 研究设计与资料来源", 2)
doc <- add_p(doc, if (CFG$run_mode == "test") {
  sprintf("本研究为TARGET护士健康队列的横断面二次分析。本机数据仅用于测试自动化流程，原始宽表包含%d条记录；调查时间、中心数、省份数和城市数均保留为【真实数据与正式研究记录核实后更新】。", source_n)
} else {
  sprintf("本研究为TARGET护士健康队列的横断面二次分析。正式导出共包含%d条原始问卷记录；调查时间、中心数、省份数和城市数须根据正式研究记录补充，不由代码推测。", source_n)
})
doc <- add_heading(doc, "2.2 研究对象与身份核验", 2)
doc <- add_p(doc, if (CFG$run_mode == "test") {
  sprintf("分析首先检查唯一ID、重复记录、A_q2编码、男性ICIQ-MLUTS模块完成状态及跨模块同ID连接。测试模式允许以“%s”继续运行并生成警告；正式模式仅在唯一ID、性别编码、模块归属和SPS-6计分均确认后继续。当前测试分析纳入%d条13项LUTS合法完整记录。", population, module_n)
} else {
  sprintf("分析首先检查唯一ID、重复记录、A_q2编码、男性ICIQ-MLUTS模块完成状态及跨模块同ID连接。生产关口均通过后，共纳入%d名完成13项LUTS合法作答的%s。", module_n, population)
})
doc <- add_heading(doc, "2.3 测量工具", 2)
doc <- add_heading(doc, "2.3.1 下尿路症状", 3)
doc <- add_p(doc, "使用ICIQ-MLUTS男性模块中的13个0～4级症状条目：尿频、夜尿、尿急、急迫性尿失禁、压力性尿失禁、无明显诱因尿失禁、排尿等待、持续排尿需用力、尿线减弱、尿流中断、排尿不尽感、排尿后滴沥和夜间遗尿［11-12］。总分为13项之和；储尿、排尿和排尿后维度由对应原始症状条目求和。")
doc <- add_heading(doc, "2.3.2 工作生产力损失", 3)
doc <- add_p(doc, "SPS-6包含工作受限4题和工作精力2题［13-14］。原始选项均按1～5存储；依据中文问卷，第1～4题保持原方向，第5～6题按6-x反向计分，总分6～30分，得分越高表示工作生产力损失越多。流程同时审计不反向备选方案，并比较频数、相关矩阵、条目—总分相关、α、ω、两个维度信度、总分分布及源总分一致性。正式模式要求配置文件显式确认计分方案。")
doc <- add_heading(doc, "2.3.3 协变量", 3)
doc <- add_p(doc, "主模型调整年龄、BMI、教育程度、婚姻状况、科室、专业技术职称、管理岗位、吸烟、饮酒、慢性病、夜班和加班。每日总液体摄入量仅用于敏感性分析。当前数据未提供医院编码，因此未建立医院聚类、随机效应、多水平模型或医院固定效应模型。")
doc <- add_heading(doc, "2.4 统计学方法", 2)
doc <- add_p(doc, "使用R完成全部统计。13个LUTS条目作为多分类观测变量拟合1～8类LCA；每个模型至少200个随机初始值、最大迭代10 000次，若最优对数似然重复不足则自动增加至500或1 000个随机初始值。类别选择综合BIC、aBIC、熵、AvePP、类别规模、最优解重复性、边界概率、局部依赖和临床可解释性，不预设类别数。类别比例＜5%提示，＜3%或人数过少列为高风险。若选定模型存在广泛且实质性局部依赖，输出MODEL_NOT_READY_FOR_FINAL_INFERENCE。")
doc <- add_p(doc, "LCA与SPS-6的主要关联采用1 000次后验概率伪类别抽样，分别拟合多变量线性回归并按Rubin法则合并；报告调整后边际均值、相对最低负担类别的均值差、95%CI、P值、整体Wald检验、标准化效应量和类别增量解释度。全部线性模型使用HC3稳健标准误。敏感性分析包括200次抽样、硬分类、额外调整饮水量、无慢性病人群、排除全部质量标记者及二分类LUTS重新建模。")
doc <- add_p(doc, "连续症状负担对照包括LUTS总分线性模型、自然样条模型以及储尿、排尿和排尿后维度模型，并与硬分类LCA模型比较调整R²、AIC、BIC、ΔR²、partial R²和标准化效应。若类别主要呈严重程度梯度且LCA无明显增量价值，预先规定建议将正式论文主线调整为连续LUTS症状负担与SPS-6的剂量-反应关联。")
doc <- add_heading(doc, "2.5 伦理学", 2)
doc <- add_p(doc, payload$placeholders$ethics, bold = TRUE, color = "7F6000")
doc <- add_p(doc, payload$placeholders$consent, bold = TRUE, color = "7F6000")

doc <- add_heading(doc, "3 结果", 1)
if (CFG$run_mode == "test") doc <- add_p(doc, "以下全部样本量、统计量、P值和效应值来自本机测试数据，仅用于核对自动化输出一致性，不构成研究结论。", bold = TRUE, color = "C00000")
doc <- add_heading(doc, "3.1 数据核验、样本与量表审计", 2)
doc <- add_p(doc, sprintf("源数据共有%d条记录，13项LUTS均为合法0～4值的模块完成记录为%d条，主回归完整病例为%d条（图1）。唯一ID、性别—模块连接和SPS-6计分的详细证据分别见sex_module_linkage_audit.xlsx和SPS6_scoring_audit.xlsx。", source_n, module_n, reg_n))
doc <- officer::body_add_img(doc, out_path("07_figures", "Figure1_sample_flow.png"), width = 5.8, height = 6.2)
doc <- add_p(doc, if (CFG$run_mode == "test") "图1 研究对象筛选流程。测试模式以模块完成记录跑通流程；正式模式在身份或计分核验失败时停止。" else "图1 研究对象筛选流程。所有样本量均由正式数据和预设纳排流程自动生成。", size = 9, align = "center")
doc <- add_p(doc, sprintf("字典计分方案的SPS-6 Cronbach α为%.3f，McDonald ω总系数为%.3f；与源总分完全一致率为%.1f%%。这些统计量仅用于审计，计分方向由原始中文题目和导出逻辑决定。", sps_current$alpha, sps_current$omega_total, sps_agree$exact_match_pct))

doc <- add_heading(doc, "3.2 潜在类别模型与症状谱", 2)
doc <- add_p(doc, sprintf("1～8类模型均完成拟合。自动综合选择结果为%d类：BIC=%.1f，aBIC=%.1f，熵=%.3f，最小类别比例=%.1f%%，最小AvePP=%.3f，最优解在%d个随机初始值中重复%d次。当前模型状态为%s。", sel$classes, sel$BIC, sel$aBIC, sel$entropy, 100 * sel$min_estimated_class_prop, sel$min_AvePP, sel$random_starts, sel$best_ll_repetitions, model_status))
doc <- add_p(doc, class_sentence)
doc <- add_p(doc, "表1 样本基本特征（主文不提供大量类别间P值）", bold = TRUE, align = "center")
table1 <- fread(out_path("06_tables", "Table1_basic_characteristics.csv"), encoding = "UTF-8")
setnames(table1, c("特征", "水平", "总体", paste0("C", seq_len(K))))
value_cols <- names(table1)[-(1:2)]
table1[, (value_cols) := lapply(.SD, function(x) gsub("; n=", "\nn=", x, fixed = TRUE)), .SDcols = value_cols]
doc <- add_ft(doc, table1, font_size = if (K > 4) 5.3 else 5.8, max_width = 6.25)
doc <- add_p(doc, paste0("类别简称：", paste(sprintf("C%d=%s", seq_len(K), class_levels), collapse = "；"), "。"), size = 8.2)
doc <- add_p(doc, "注：连续变量根据偏度报告均数（标准差）或中位数（四分位数），并给出有效n；分类变量报告n/分母（%）。", size = 8.5)
doc <- add_p(doc, "表2 1～8类LCA核心拟合指标", bold = TRUE, align = "center")
table2 <- fread(out_path("06_tables", "Table2_LCA_core_fit.csv"), encoding = "UTF-8")
for (v in intersect(c("对数似然", "AIC", "BIC", "aBIC"), names(table2))) set(table2, j = v, value = sprintf("%.2f", table2[[v]]))
for (v in intersect(c("熵", "最小AvePP"), names(table2))) set(table2, j = v, value = sprintf("%.3f", table2[[v]]))
if ("最小类别比例(%)" %in% names(table2)) table2[, `最小类别比例(%)` := sprintf("%.1f", as.numeric(`最小类别比例(%)`))]
if ("选择" %in% names(table2)) table2[, 选择 := fifelse(toupper(as.character(选择)) == "TRUE", "是", "")]
doc <- add_ft(doc, table2, font_size = 6.8, max_width = 6.25)
doc <- add_p(doc, "注：完整收敛、随机初始值、边界概率和局部依赖指标见补充材料。最终类别数由综合规则自动选择。", size = 8.5)
doc <- officer::body_add_img(doc, out_path("07_figures", "Figure2_LCA_profile_heatmap.png"), width = 6.8, height = 3.2)
doc <- add_p(doc, "图2 最终LCA模型各类别13项LUTS期望得分热图。按储尿、排尿和排尿后症状分面。", size = 9, align = "center")

doc <- add_heading(doc, "3.3 连续LUTS症状负担对照", 2)
doc <- add_p(doc, sprintf("在相同完整病例框架中，LUTS总分线性模型的ΔR²为%.4f，硬分类LCA模型的ΔR²为%.4f，二者差值为%.4f。LUTS总分每增加1分，调整后SPS-6改变%.2f分（95%%CI %.2f～%.2f，%s）；从LUTS总分P25到P75的对应差值为%.2f分。自然样条整体检验%s。", comparison[model == "LUTS total linear", delta_R2], comparison[model == "LCA hard classes", delta_R2], comparison[model == "LCA hard classes", delta_R2] - comparison[model == "LUTS total linear", delta_R2], linear$estimate[1], linear$lower[1], linear$upper[1], ptext(linear$p_value[1]), linear$iqr_outcome_difference[1], ptext(spline_global$p_value[1])))
doc <- officer::body_add_img(doc, out_path("07_figures", "Figure3_continuous_LUTS_spline.png"), width = 6.0, height = 4.2)
doc <- add_p(doc, "图3 LUTS连续总分与调整后SPS-6预测得分的剂量-反应曲线及95%CI。", size = 9, align = "center")
doc <- add_p(doc, if (CFG$run_mode == "test") {
  if (recommend_continuous) "按预设比较规则，当前测试结果触发“连续症状负担主线”建议；正式数据重跑后应重新判断，不得沿用本结论。" else "按预设比较规则，当前测试结果未触发自动改为连续症状负担主线；正式数据仍需结合临床可解释性和局部依赖复核。"
} else {
  if (recommend_continuous) "按预设比较规则，正式数据结果支持优先采用连续症状负担剂量-反应主线，并降低“亚型/分型”表述。" else "按预设比较规则，正式数据未触发自动改为连续症状负担主线；是否保留LCA主线仍需结合临床可解释性和局部依赖复核。"
}, bold = TRUE)

doc <- add_heading(doc, "3.4 潜在类别与SPS-6得分的关联", 2)
doc <- add_p(doc, sprintf("主分析纳入%d条记录。调整后边际均值为：%s。以最低负担类别为参照，%s。类别整体Wald χ²=%.2f，df=%d，%s。", reg_n, mean_sentence, diff_sentence, global$wald_chisq[1], global$df[1], ptext(global$p_value[1])))
doc <- add_p(doc, "表3 LUTS潜在类别与SPS-6得分的调整关联", bold = TRUE, align = "center")
table3 <- fread(out_path("06_tables", "Table3_LCA_SPS6_association.csv"), encoding = "UTF-8")
table3_display <- table3[, .(
  `LUTS类别` = `LUTS类别`,
  n = n,
  `调整后均值（95%CI）` = sprintf("%.2f（%.2f～%.2f）", `调整后SPS-6均值`, `均值95%CI下限`, `均值95%CI上限`),
  `较参照差值（95%CI）` = fifelse(is.na(`差值95%CI下限`), "参照", sprintf("%.2f（%.2f～%.2f）", `较参照类别调整均值差`, `差值95%CI下限`, `差值95%CI上限`)),
  P = fifelse(is.na(`P值`), "—", vapply(`P值`, ptext, character(1))),
  `标准化效应量` = sprintf("%.2f", `标准化效应量`)
)]
doc <- add_ft(doc, table3_display, font_size = 7.0, max_width = 6.25)
doc <- add_p(doc, sprintf("二分类LUTS敏感性模型选择%d类，与主模型的调整Rand指数为%.3f；该结果按数值解释类别结构对编码方式的稳定性，不使用“方向一致”替代完整估计。", binary$binary_classes[1], binary$adjusted_rand_index[1]))

doc <- add_heading(doc, "4 讨论", 1)
discussion <- c(
  if (CFG$run_mode == "test") sprintf("本测试分析在%s中自动选择%d类LCA，并观察到类别与SPS-6得分的横断面关联。所有结果用于确认代码、表图和文稿能由原始数据自动更新，不能作为真实护士人群的实证结论。", population, sel$classes) else sprintf("本研究在%s中依据预设综合标准自动选择%d类LCA，并评估类别与SPS-6得分的横断面关联。", population, sel$classes),
  if (recommend_continuous) "类别期望得分主要呈严重程度梯度，且LCA相较连续总分未显示预设阈值以上的增量解释价值。因此应降低“亚型/分型”表述，并优先讨论LUTS症状负担与SPS-6得分的剂量-反应关联。" else "类别结果未满足自动转向连续主线的预设条件；若类别呈现储尿、排尿或排尿后症状的非平行差异，并具有稳定收敛、可接受类别规模和有限局部依赖，可保留人群中心的类别异质性主线。",
  "护士工作中的轮班、延迟排尿和饮水限制可能与LUTS共存；症状还可能通过睡眠中断、工作中断和注意力分散与SPS-6得分相关。但本研究为横断面设计，不能确定时间顺序，也不能将类别差异解释为LUTS导致生产力下降。",
  if (CFG$run_mode == "test") sprintf("本分析的主要限制包括：当前测试数据的性别字段与男性模块路由存在待核验问题；模型状态为%s；SPS-6计分虽有字典证据，正式导出仍需排除提前反向后再次反向；完整病例分析可能产生选择偏倚；数据不含医院编码，无法控制中心内相关。", model_status) else sprintf("本分析的主要限制包括：模型状态为%s；横断面设计不能作因果推断；完整病例分析可能产生选择偏倚；数据不含医院编码，无法控制中心内相关。", model_status),
  "本流程的价值在于把身份核验、计分审计、类别选择、局部依赖、连续负担对照、分类不确定性和文稿更新置于同一可复现链路。正式数据只有通过生产模式关口后，才可生成供投稿判断的结果。"
)
for (x in discussion) doc <- add_p(doc, x)

doc <- add_heading(doc, "5 结论", 1)
doc <- add_p(doc, if (CFG$run_mode == "test") "当前工作稿证明测试数据可以从输入文件自动运行至表格、图片和文稿。任何关于类别数、样本量、效应方向或统计学显著性的表述均须在堡垒机真实数据重跑后更新。" else "在通过身份、模块和计分核验的正式数据中，LUTS类别与SPS-6得分存在关联；类别主线是否保留应由模型稳定性、症状模式和连续负担对照共同决定。")

doc <- add_heading(doc, "声明", 1)
for (x in c(payload$placeholders$funding, payload$placeholders$conflict,
            payload$placeholders$availability, payload$placeholders$ai)) doc <- add_p(doc, x, bold = TRUE, color = "7F6000")
doc <- add_heading(doc, "参考文献", 1)
if (length(references)) for (r in references) doc <- add_p(doc, r, size = 8.5, after = 2) else doc <- add_p(doc, "【沿用最新初稿参考文献，投稿前由文献管理器核验】", bold = TRUE, color = "7F6000")

draft_path <- out_path("outputs", "LUTS_work_productivity_working_draft_revised.docx")
print(doc, target = draft_path)
file.copy(draft_path, out_path("08_manuscript", basename(draft_path)), overwrite = TRUE)

# Supplementary Methods document, generated from the same configuration and source template.
supp <- clear_body(if (template_available) officer::read_docx(reference_docx) else officer::read_docx())
supp <- add_p(supp, "Supplementary Methods: LUTS Latent Classes and Work Productivity", bold = TRUE, size = 17, align = "center")
if (nzchar(test_banner)) supp <- add_p(supp, test_banner, bold = TRUE, color = "C00000", size = 12, align = "center")
supp_sections <- list(
  "S1 双模式与生产关口" = "test模式将身份、模块路由和SPS-6计分异常记录为警告，并继续生成全套测试输出；production模式在唯一ID、A_q2编码、男性模块归属或SPS-6计分未确认时停止，不生成可投稿结果。",
  "S2 ID与模块连接" = "流程报告总记录数、唯一ID数、缺失ID、重复ID组及处理。跨模块数据必须以唯一ID对应；当前宽表在同一ID行保存LUTS、SPS-6及协变量，但性别—男性模块矛盾仍单独作为生产阻断条件。",
  "S3 SPS-6计分审计" = "逐项提取中文题目、原始选项和计分方向，比较字典方案（第1～4题正向，第5～6题反向）与不反向备选方案。输出原始频数、相关矩阵、校正条目—总分相关、Cronbach α、McDonald ω、两个维度信度、总分分布和源总分一致性。",
  "S4 潜在类别分析" = sprintf("以13个0～4级多分类条目拟合1～8类模型，随机种子%d；每个模型至少200个随机初始值，最大迭代10 000次，不稳定模型增加到500或1 000个初始值。", CFG$random_seed),
  "S5 局部独立性" = "对每个候选模型计算全部条目对的二元残差，报告BVR分布、最大值、原始P值和BH校正P值。若选定模型存在广泛且实质性的局部依赖，标记MODEL_NOT_READY_FOR_FINAL_INFERENCE。",
  "S6 类别与SPS-6关联" = "主要分析使用1 000次后验概率伪类别抽样、HC3稳健标准误和Rubin法则。报告调整后边际均值、均值差、95%CI、P值、整体Wald检验、标准化效应量和增量解释度。",
  "S7 连续症状负担对照" = "在相同完整病例上拟合协变量基线模型、LUTS总分线性模型、自然样条模型、症状维度模型和硬分类LCA模型，比较调整R²、AIC、BIC、ΔR²、partial R²和标准化效应，并生成边际剂量-反应曲线。",
  "S8 敏感性分析" = "包括200次伪类别抽样、最大后验概率硬分类、额外调整每日液体摄入量、无慢性病人群、排除全部作答质量标记者、二分类LUTS重新建模以及连续总分和样条分析。完整估计、95%CI、P值、样本量和调整变量均写入补充工作簿。"
)
for (nm in names(supp_sections)) { supp <- add_heading(supp, nm, 1); supp <- add_p(supp, supp_sections[[nm]]) }
supp_path <- out_path("outputs", "Supplementary_Methods_LUTS.docx")
print(supp, target = supp_path)
file.copy(supp_path, out_path("08_manuscript", basename(supp_path)), overwrite = TRUE)

# Chinese final pipeline audit; no result is manually entered.
sex_status <- fread(out_path("00_audit", "id_linkage_audit.csv"), encoding = "UTF-8")
audit_report <- c(
  "# FINAL PIPELINE AUDIT CHINESE", "",
  if (CFG$run_mode == "test") "> **本轮使用测试数据；当前所有统计结果禁止投稿。**" else "",
  "## 已跑通流程", "",
  "- 集中配置、命令行参数、固定随机种子、运行日志和sessionInfo。",
  "- 唯一ID、重复记录、性别—男性模块、LUTS范围、作答质量和SPS-6双方案审计。",
  "- 1～8类多分类LCA、自适应随机初始值、完整局部依赖、二分类LCA和ARI。",
  "- 1 000次伪类别主分析、HC3、Rubin合并、调整边际均值、完整敏感性分析。",
  "- LUTS总分线性、自然样条、症状维度和LCA模型比较，以及剂量-反应图。",
  "- 主表、补充表、PDF/PNG/600 dpi TIFF图、Word工作稿和结果清单自动生成。", "",
  if (CFG$run_mode == "test") "## 测试数据特有问题" else "## 生产关口状态", "",
  paste0("- 身份/模块状态：", if (CFG$male_module_ownership_confirmed) "已确认。" else "未确认；测试模式继续，production模式停止。"),
  paste0("- SPS-6正式确认开关：", CFG$sps6_scoring_confirmed_for_production, "；两套方案审计结果均已保存。"),
  paste0("- 当前LCA模型状态：", model_status, "；该状态由代码根据局部依赖阈值生成。"),
  paste0("- 当前策略建议：", if (recommend_continuous) "若正式数据重复，应优先考虑连续症状负担剂量-反应主线。" else "未自动触发改为连续主线，正式结果仍需临床复核。"), "",
  "## 到堡垒机需要替换", "",
  "1. 在RStudio中载入名为 `data` 的数据框，或使用 `--data` 指向真实CSV、CSV.GZ、TSV或RDS。大型XLSX应先载入RStudio或转换为CSV/RDS。",
  "2. 在config/config.R中核实字典路径；Word模板可选，留空时使用干净模板。",
  "3. 核实变量映射；如真实导出列名不同，只修改config/config.R，不修改统计脚本。",
  "4. 核实R与所需包版本，运行 `Rscript run_all.R --mode production --data \"真实数据路径\"`。", "",
  "## 正式运行前必须确认", "",
  "- 唯一ID定义、重复记录规则及各问卷模块是否属于同一ID。",
  "- A_q2原始编码与男性模块路由；正式数据中的模块完成记录确为男护士。",
  "- 13个ICIQ-MLUTS条目的0～4计分及分支逻辑。",
  "- SPS-6是否保存原始1～5作答，第5～6题是否尚未提前反向；确认后将production开关设为TRUE。",
  "- 年龄、BMI、婚姻、教育、科室、职称、管理岗位、吸烟、饮酒、慢性病、夜班、加班和饮水量的真实编码。",
  "- 调查时间、医院数、省份数、城市数、伦理和作者信息来自正式研究记录，而非测试数据。", "",
  "## 当前状态", "",
  if (CFG$run_mode == "test") "测试模式已达到“可从输入数据一键运行至全部结果文件”的状态。正式模式的代码入口已建立，但身份/模块和SPS-6确认开关故意保持关闭；只有在堡垒机完成上述核验并更新配置后，才达到“真实数据一键重跑并可进入投稿判断”的状态。" else "生产模式已通过全部预设关口并从真实数据生成完整结果；能否投稿仍须结合模型状态、局部依赖、研究记录与作者审核判断。",
  "当前数据没有医院编码，流程未调用医院聚类、随机效应、多水平模型或医院固定效应分析。"
)
write_lines_utf8(audit_report, out_path("outputs", "FINAL_PIPELINE_AUDIT_CHINESE.md"))
file.copy(out_path("outputs", "FINAL_PIPELINE_AUDIT_CHINESE.md"), out_path("FINAL_PIPELINE_AUDIT_CHINESE.md"), overwrite = TRUE)
log_msg("INFO", "Word documents and Chinese audit report generated from analysis outputs.")
