# Complete traditional prediction pipeline:
# Shift nurse sleep-rhythm related depression risk model.
#
# Direct use in RStudio after upload to GitHub:
# source("https://raw.githubusercontent.com/<owner>/<repo>/<branch>/github/shift_sleep_depression_complete_pipeline.R")
#
# Default data path:
# D:/护士队列/data/data.xlsx
#
# Optional before source():
# Sys.setenv(NURSE_DATA_XLSX = "D:/护士队列/data/data.xlsx")
# Sys.setenv(NURSE_OUTPUT_DIR = "C:/Users/yan/Documents/New project 16/analysis_outputs/shift_sleep_depression_model")

set.seed(20260523)

if (!requireNamespace("MASS", quietly = TRUE)) {
  stop("Please install MASS first: install.packages('MASS')", call. = FALSE)
}

num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))

clean_num <- function(x) {
  z <- num(x)
  z[z < 0] <- NA_real_
  z
}

fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

fmt_mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  sprintf("%.2f +/- %.2f", mean(x), stats::sd(x))
}

factor_label <- function(x, labels) {
  z <- clean_num(x)
  factor(z, levels = as.numeric(names(labels)), labels = unname(labels))
}

write_extractor <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  code <- c(
    "#!/usr/bin/env python3",
    "import argparse, csv, os, re, sys, time, zipfile",
    "import xml.etree.ElementTree as ET",
    "",
    "EXACT_COLUMNS = {",
    "    'id','A_q2','A_year','A_q4','A_q5','A_q6','A_q7','A_q9','A_q10','A_q11','A_q12','A_q13','A_q14','A_q15','A_q16','A_age','A_gongzuoshichang','work_y',",
    "    'D_PSQI_ALL','D_yiyu_all','G_pifa_all','G_qutipifa'",
    "}",
    "C_RE = re.compile(r'^C_q([1-9]|[1-3][0-9]|4[0-6])(_\\d+)?$')",
    "G_Q4_RE = re.compile(r'^G_q4_([1-9]|10|11)$')",
    "",
    "def col_to_index(cell_ref):",
    "    idx = 0",
    "    for ch in cell_ref:",
    "        if 'A' <= ch <= 'Z': idx = idx * 26 + ord(ch) - 64",
    "        elif 'a' <= ch <= 'z': idx = idx * 26 + ord(ch) - 96",
    "        elif ch.isdigit(): break",
    "    return idx",
    "",
    "def load_shared_strings(zf):",
    "    if 'xl/sharedStrings.xml' not in zf.namelist(): return None",
    "    values = []",
    "    with zf.open('xl/sharedStrings.xml') as fh:",
    "        for _, elem in ET.iterparse(fh, events=('end',)):",
    "            if elem.tag.rsplit('}', 1)[-1] == 'si':",
    "                values.append(''.join(node.text or '' for node in elem.iter() if node.tag.rsplit('}', 1)[-1] == 't'))",
    "                elem.clear()",
    "    return values",
    "",
    "def cell_text(cell, shared_strings):",
    "    cell_type = cell.attrib.get('t')",
    "    if cell_type == 'inlineStr':",
    "        return ''.join(node.text or '' for node in cell.iter() if node.tag.rsplit('}', 1)[-1] == 't')",
    "    value = ''",
    "    for child in cell:",
    "        if child.tag.rsplit('}', 1)[-1] == 'v':",
    "            value = child.text or ''",
    "            break",
    "    if cell_type == 's' and shared_strings is not None and value != '':",
    "        try: return shared_strings[int(value)]",
    "        except Exception: return value",
    "    return value",
    "",
    "def should_select(name):",
    "    return name in EXACT_COLUMNS or C_RE.match(name) is not None or G_Q4_RE.match(name) is not None",
    "",
    "def stream_xlsx(xlsx_path, out_path, columns_path, progress_every=5000):",
    "    start = time.time()",
    "    os.makedirs(os.path.dirname(os.path.abspath(out_path)), exist_ok=True)",
    "    tmp_out = out_path + '.tmp'",
    "    tmp_cols = columns_path + '.tmp'",
    "    with zipfile.ZipFile(xlsx_path) as zf:",
    "        shared_strings = load_shared_strings(zf)",
    "        with zf.open('xl/worksheets/sheet1.xml') as sheet, open(tmp_out, 'w', newline='', encoding='utf-8') as out:",
    "            writer = csv.writer(out)",
    "            headers = None; selected_indexes = []; selected_positions = {}; row_count = 0",
    "            for _, elem in ET.iterparse(sheet, events=('end',)):",
    "                if elem.tag.rsplit('}', 1)[-1] != 'row': continue",
    "                if headers is None:",
    "                    values = {}",
    "                    for cell in elem:",
    "                        if cell.tag.rsplit('}', 1)[-1] == 'c':",
    "                            idx = col_to_index(cell.attrib.get('r', ''))",
    "                            values[idx] = cell_text(cell, shared_strings)",
    "                    headers = [values.get(i, '') for i in range(1, max(values) + 1)]",
    "                    selected_indexes = [i + 1 for i, name in enumerate(headers) if should_select(name)]",
    "                    selected_positions = {idx: pos for pos, idx in enumerate(selected_indexes)}",
    "                    selected_headers = [headers[i - 1] for i in selected_indexes]",
    "                    writer.writerow(selected_headers)",
    "                    with open(tmp_cols, 'w', newline='', encoding='utf-8') as cols:",
    "                        cw = csv.writer(cols); cw.writerow(['column_index','column_name'])",
    "                        for idx in selected_indexes: cw.writerow([idx, headers[idx - 1]])",
    "                    print('Selected {} columns.'.format(len(selected_indexes)), file=sys.stderr, flush=True)",
    "                else:",
    "                    row = [''] * len(selected_indexes)",
    "                    for cell in elem:",
    "                        if cell.tag.rsplit('}', 1)[-1] != 'c': continue",
    "                        idx = col_to_index(cell.attrib.get('r', ''))",
    "                        pos = selected_positions.get(idx)",
    "                        if pos is not None: row[pos] = cell_text(cell, shared_strings)",
    "                    writer.writerow(row); row_count += 1",
    "                    if row_count % progress_every == 0:",
    "                        print('Wrote {:,} rows in {:.1f}s'.format(row_count, time.time() - start), file=sys.stderr, flush=True)",
    "                elem.clear()",
    "    os.replace(tmp_out, out_path); os.replace(tmp_cols, columns_path)",
    "    print('Done. Wrote {:,} rows to {} in {:.1f}s'.format(row_count, out_path, time.time() - start), file=sys.stderr, flush=True)",
    "",
    "def main():",
    "    parser = argparse.ArgumentParser()",
    "    parser.add_argument('--xlsx', required=True)",
    "    parser.add_argument('--out', required=True)",
    "    parser.add_argument('--columns-out', required=True)",
    "    args = parser.parse_args()",
    "    stream_xlsx(args.xlsx, args.out, args.columns_out)",
    "",
    "if __name__ == '__main__': main()"
  )
  writeLines(code, path, useBytes = TRUE)
}

extract_if_needed <- function(data_xlsx, output_dir) {
  csv_path <- file.path(output_dir, "shift_sleep_depression_data.csv")
  columns_path <- file.path(output_dir, "selected_columns.csv")
  if (file.exists(csv_path)) return(csv_path)
  if (!file.exists(data_xlsx)) stop("Data file not found: ", data_xlsx, call. = FALSE)
  extractor <- file.path(output_dir, "stream_shift_sleep_depression_data.py")
  write_extractor(extractor)
  message("Extracting selected fields from the large XLSX. This may take several minutes...")
  commands <- list(
    c("python"),
    c("py", "-3"),
    c("python3")
  )
  status <- 1L
  for (cmd in commands) {
    status <- tryCatch(
      system2(
        cmd[1],
        args = c(cmd[-1], extractor, "--xlsx", data_xlsx, "--out", csv_path, "--columns-out", columns_path)
      ),
      error = function(e) 1L
    )
    if (identical(status, 0L)) break
  }
  if (!identical(status, 0L) || !file.exists(csv_path)) {
    stop("Python extraction failed. Please install Python or make sure python/py/python3 is available in PATH.", call. = FALSE)
  }
  csv_path
}

build_dataset <- function(raw) {
  dat <- data.frame(row_id = seq_len(nrow(raw)))
  dat$age <- clean_num(raw$A_age)
  dat$bmi <- {
    h <- clean_num(raw$A_q15)
    w <- clean_num(raw$A_q16)
    bmi <- w / (h / 100)^2
    bmi[!is.finite(bmi) | bmi < 12 | bmi > 45] <- NA_real_
    bmi
  }
  dat$work_years <- clean_num(raw$A_gongzuoshichang)
  dat$rotation_direction <- clean_num(raw$C_q5)
  dat$night_shift_years_level <- clean_num(raw$C_q6)
  dat$night_months_past_year <- {
    x <- clean_num(raw$C_q7)
    ifelse(is.na(x), NA_real_, 13 - x)
  }
  dat$night_shifts_per_month <- clean_num(raw$C_q8)
  dat$night_shift_interval <- clean_num(raw$C_q9)
  dat$pre_night_burden <- clean_num(raw$C_q11)
  dat$pre_night_sleep_hours <- clean_num(raw$C_q12)
  dat$pre_night_sleep_quality <- clean_num(raw$C_q13)
  dat$pre_night_energy <- clean_num(raw$C_q15)
  dat$night_burden <- clean_num(raw$C_q22)
  dat$night_busyness <- clean_num(raw$C_q24)
  dat$post_sleep_medication <- clean_num(raw$C_q30)
  dat$post_night_sleep_hours <- clean_num(raw$C_q31)
  dat$post_night_sleep_quality <- clean_num(raw$C_q32)
  dat$post_night_recovery <- clean_num(raw$C_q39)
  dat$shiftwork_sleep_disorder <- {
    c44 <- clean_num(raw$C_q44); c45 <- clean_num(raw$C_q45); c46 <- clean_num(raw$C_q46)
    out <- ifelse(c44 == 1 & c45 == 1 & c46 == 1, 1, 0)
    out[is.na(c44) | is.na(c45) | is.na(c46)] <- NA_real_
    out
  }
  dat$psqi_total <- clean_num(raw$D_PSQI_ALL)
  dat$fatigue_total <- clean_num(raw$G_pifa_all)
  dat$physical_fatigue <- clean_num(raw$G_qutipifa)
  g <- function(name) clean_num(raw[[name]])
  dat$circadian_fr <- g("G_q4_2") + g("G_q4_4") + g("G_q4_6") + g("G_q4_8") + g("G_q4_10")
  dat$circadian_lv <- g("G_q4_1") + g("G_q4_3") + g("G_q4_5") + g("G_q4_7") + g("G_q4_9") + g("G_q4_11")
  dat$phq9_total <- clean_num(raw$D_yiyu_all)
  dat$depression_risk <- as.integer(dat$phq9_total >= 10)
  dat$depression_risk[is.na(dat$phq9_total)] <- NA_integer_
  shift_flag <- clean_num(raw$C_q1) %in% c(2, 3, 4) | clean_num(raw$C_q2) %in% c(1, 2, 3)
  dat[shift_flag %in% TRUE & !is.na(dat$depression_risk), , drop = FALSE]
}

candidate_variables <- function() {
  c(
    "age", "bmi", "work_years",
    "rotation_direction", "night_shift_years_level", "night_months_past_year",
    "night_shifts_per_month", "night_shift_interval", "pre_night_burden",
    "pre_night_sleep_hours", "pre_night_sleep_quality", "pre_night_energy",
    "night_burden", "night_busyness", "post_sleep_medication",
    "post_night_sleep_hours", "post_night_sleep_quality", "post_night_recovery",
    "shiftwork_sleep_disorder", "psqi_total", "fatigue_total",
    "physical_fatigue", "circadian_fr", "circadian_lv"
  )
}

roc_curve <- function(y, p) {
  ord <- order(p, decreasing = TRUE)
  y <- y[ord]
  pos <- sum(y == 1)
  neg <- sum(y == 0)
  tpr <- c(0, cumsum(y == 1) / pos, 1)
  fpr <- c(0, cumsum(y == 0) / neg, 1)
  auc <- sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
  data.frame(fpr = fpr, tpr = tpr, auc = auc)
}

make_baseline_table <- function(dat, raw, output_dir) {
  bdat <- dat
  bdat$depression_group <- factor(
    ifelse(bdat$depression_risk == 1, "Depression risk", "No depression risk"),
    levels = c("No depression risk", "Depression risk")
  )
  continuous_vars <- c(
    age = "Age, years",
    bmi = "BMI, kg/m2",
    work_years = "Work duration, years",
    night_months_past_year = "Night-shift months in past year",
    psqi_total = "PSQI total score",
    fatigue_total = "Fatigue total score",
    physical_fatigue = "Physical fatigue score",
    circadian_fr = "Circadian FR score",
    circadian_lv = "Circadian LV score"
  )
  rows <- list()
  add_row <- function(variable, level, overall, low, high, p) {
    rows[[length(rows) + 1]] <<- data.frame(
      Variable = variable, Level = level, Overall = overall,
      No_depression_risk = low, Depression_risk = high, P_value = p,
      stringsAsFactors = FALSE
    )
  }
  for (v in names(continuous_vars)) {
    x <- bdat[[v]]; y <- bdat$depression_group
    p <- tryCatch(stats::t.test(x ~ y)$p.value, error = function(e) NA_real_)
    add_row(
      continuous_vars[[v]], "", fmt_mean_sd(x),
      fmt_mean_sd(x[y == "No depression risk"]),
      fmt_mean_sd(x[y == "Depression risk"]),
      fmt_p(p)
    )
  }
  # Compact categorical baseline for variables most relevant to the model.
  cat_list <- list(
    SWSD = factor(ifelse(bdat$shiftwork_sleep_disorder == 1, "Yes", "No"), levels = c("No", "Yes")),
    `Night shifts per month` = factor(bdat$night_shifts_per_month, levels = 1:3, labels = c("<=4", "5-9", ">=10")),
    `Pre-night sleep quality` = factor(bdat$pre_night_sleep_quality, levels = 1:5, labels = c("Very good", "Good", "Average", "Poor", "Very poor")),
    `Post-night recovery` = factor(bdat$post_night_recovery, levels = 1:5, labels = c("Fully recovered", "Partly recovered", "Average", "Not recovered", "Worse"))
  )
  for (nm in names(cat_list)) {
    x <- droplevels(cat_list[[nm]]); y <- bdat$depression_group
    tab <- table(x, y, useNA = "no")
    p <- tryCatch(suppressWarnings(stats::chisq.test(tab)$p.value), error = function(e) NA_real_)
    for (i in seq_len(nrow(tab))) {
      lev <- rownames(tab)[i]
      n_all <- sum(tab[lev, ]); n_low <- tab[lev, "No depression risk"]; n_high <- tab[lev, "Depression risk"]
      add_row(
        if (i == 1) nm else "", lev,
        sprintf("%d (%.1f%%)", n_all, 100 * n_all / sum(tab)),
        sprintf("%d (%.1f%%)", n_low, 100 * n_low / sum(tab[, "No depression risk"])),
        sprintf("%d (%.1f%%)", n_high, 100 * n_high / sum(tab[, "Depression risk"])),
        if (i == 1) fmt_p(p) else ""
      )
    }
  }
  baseline <- do.call(rbind, rows)
  n_row <- data.frame(
    Variable = "N", Level = "", Overall = as.character(nrow(bdat)),
    No_depression_risk = as.character(sum(bdat$depression_group == "No depression risk")),
    Depression_risk = as.character(sum(bdat$depression_group == "Depression risk")),
    P_value = "", stringsAsFactors = FALSE
  )
  baseline <- rbind(n_row, baseline)
  write.csv(baseline, file.path(output_dir, "baseline_table_complete_case.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  baseline
}

plot_nomogram <- function(fit, train, output_dir) {
  vars <- names(coef(fit))[-1]
  betas <- coef(fit)[vars]
  ranges <- lapply(vars, function(v) {
    qs <- stats::quantile(train[[v]], probs = c(0.02, 0.98), na.rm = TRUE, type = 1)
    if (all(train[[v]] %in% c(0, 1))) qs <- c(0, 1)
    if (diff(qs) == 0) qs <- range(train[[v]], na.rm = TRUE)
    qs
  })
  names(ranges) <- vars
  effects <- vapply(vars, function(v) abs(betas[v]) * diff(ranges[[v]]), numeric(1))
  max_effect <- max(effects)
  min_lp <- as.numeric(coef(fit)[1]) + sum(vapply(vars, function(v) min(betas[v] * ranges[[v]]), numeric(1)))
  max_total_points <- sum(100 * effects / max_effect)
  labels <- c(
    age = "Age", bmi = "BMI", pre_night_burden = "Pre-night burden",
    pre_night_sleep_hours = "Pre-night sleep hours", pre_night_sleep_quality = "Pre-night sleep quality",
    pre_night_energy = "Pre-night energy", night_burden = "Night-shift burden",
    night_busyness = "Night-shift busyness", post_night_recovery = "Post-night recovery",
    shiftwork_sleep_disorder = "SWSD", psqi_total = "PSQI total",
    fatigue_total = "Fatigue total", physical_fatigue = "Physical fatigue",
    circadian_fr = "Circadian FR", circadian_lv = "Circadian LV",
    night_months_past_year = "Night months/year", post_sleep_medication = "Post-night sleep meds"
  )
  png(file.path(output_dir, "nomogram_complete_case.png"), width = 3200, height = max(1700, 220 + 110 * length(vars)), res = 240)
  op <- par(mar = c(4, 17, 4, 3), xaxs = "i")
  plot(NA, xlim = c(0, 105), ylim = c(0, length(vars) + 4), axes = FALSE, xlab = "", ylab = "",
       main = "Nomogram: depression risk in shift nurses")
  axis_y <- length(vars) + 3
  axis(3, at = seq(0, 100, by = 20), labels = seq(0, 100, by = 20), line = -1)
  text(-8, axis_y, "Points", xpd = TRUE, adj = 1, font = 2)
  segments(0, axis_y, 100, axis_y)
  for (i in seq_along(vars)) {
    v <- vars[i]; y <- length(vars) + 2 - i; r <- ranges[[v]]
    var_max_points <- 100 * abs(betas[v]) * diff(r) / max_effect
    ticks <- if (all(train[[v]] %in% c(0, 1))) c(0, 1) else if (var_max_points < 16) unique(round(r, 2)) else pretty(r, n = 5)
    ticks <- ticks[ticks >= r[1] & ticks <= r[2]]
    point_vals <- (betas[v] * ticks - min(betas[v] * r)) / max_effect * 100
    segments(min(point_vals), y, max(point_vals), y, lwd = 1.6)
    segments(point_vals, y - 0.18, point_vals, y + 0.18, lwd = 1.1)
    text(point_vals, y + 0.35, labels = ticks, cex = 0.65, xpd = NA)
    text(-8, y, ifelse(v %in% names(labels), labels[v], v), xpd = TRUE, adj = 1)
  }
  tp_y <- 1.4
  segments(0, tp_y, 100, tp_y, lwd = 1.6)
  tp_ticks <- pretty(c(0, max_total_points), n = 8)
  axis(1, at = tp_ticks / max_total_points * 100, labels = round(tp_ticks), pos = tp_y, cex.axis = 0.75, tck = -0.02)
  text(-8, tp_y, "Total points", xpd = TRUE, adj = 1, font = 2)
  risk_y <- 0.3
  risk_ticks <- c(0.05, 0.10, 0.20, 0.30, 0.50, 0.70, 0.90)
  risk_points <- (qlogis(risk_ticks) - min_lp) / max_effect * 100
  keep <- risk_points >= 0 & risk_points <= max_total_points
  segments(0, risk_y, 100, risk_y, lwd = 1.6)
  axis(1, at = risk_points[keep] / max_total_points * 100, labels = risk_ticks[keep], pos = risk_y, cex.axis = 0.75, tck = -0.02)
  text(-8, risk_y, "Predicted risk", xpd = TRUE, adj = 1, font = 2)
  par(op)
  dev.off()
}

plot_validation <- function(train, valid, train_prob, valid_prob, output_dir) {
  roc_train <- roc_curve(train$depression_risk, train_prob)
  roc_valid <- roc_curve(valid$depression_risk, valid_prob)
  auc_train <- unique(roc_train$auc)[1]
  auc_valid <- unique(roc_valid$auc)[1]
  auc_out <- data.frame(
    dataset = c("Training", "Validation"),
    n = c(nrow(train), nrow(valid)),
    events = c(sum(train$depression_risk), sum(valid$depression_risk)),
    auc = c(auc_train, auc_valid)
  )
  write.csv(auc_out, file.path(output_dir, "train_validation_roc_auc_complete_case.csv"), row.names = FALSE)
  png(file.path(output_dir, "roc_complete_case.png"), width = 1800, height = 1400, res = 220)
  plot(roc_train$fpr, roc_train$tpr, type = "l", lwd = 2.5, col = "#1F6FEB",
       xlab = "1 - Specificity", ylab = "Sensitivity",
       main = "ROC curves: complete-case logistic model", xlim = c(0, 1), ylim = c(0, 1))
  lines(roc_valid$fpr, roc_valid$tpr, lwd = 2.5, col = "#0E7C66")
  abline(0, 1, lty = 2, col = "grey55")
  legend("bottomright", legend = c(sprintf("Training AUC = %.3f", auc_train), sprintf("Validation AUC = %.3f", auc_valid)),
         col = c("#1F6FEB", "#0E7C66"), lwd = 2.5, bty = "n")
  dev.off()
  cal <- function(y, p, dataset) {
    bins <- cut(p, breaks = unique(stats::quantile(p, probs = seq(0, 1, 0.1), na.rm = TRUE)), include.lowest = TRUE, labels = FALSE)
    do.call(rbind, lapply(sort(unique(bins)), function(b) {
      idx <- bins == b
      data.frame(dataset = dataset, decile = b, n = sum(idx), mean_predicted = mean(p[idx]), observed = mean(y[idx]))
    }))
  }
  cal_train <- cal(train$depression_risk, train_prob, "Training")
  cal_valid <- cal(valid$depression_risk, valid_prob, "Validation")
  write.csv(rbind(cal_train, cal_valid), file.path(output_dir, "calibration_deciles_complete_case.csv"), row.names = FALSE)
  png(file.path(output_dir, "calibration_complete_case.png"), width = 1800, height = 1400, res = 220)
  plot(0:1, 0:1, type = "n", xlab = "Mean predicted probability", ylab = "Observed event rate",
       main = "Calibration curve", xlim = c(0, 1), ylim = c(0, 1))
  abline(0, 1, lty = 2, col = "grey55")
  lines(cal_train$mean_predicted, cal_train$observed, type = "b", lwd = 2.4, pch = 16, col = "#1F6FEB")
  lines(cal_valid$mean_predicted, cal_valid$observed, type = "b", lwd = 2.4, pch = 16, col = "#0E7C66")
  legend("topleft", legend = c("Training", "Validation", "Ideal"), col = c("#1F6FEB", "#0E7C66", "grey55"),
         lwd = c(2.4, 2.4, 1.5), lty = c(1, 1, 2), pch = c(16, 16, NA), bty = "n")
  dev.off()
  thresholds <- seq(0.01, 0.80, by = 0.01)
  prevalence <- mean(valid$depression_risk)
  dca <- do.call(rbind, lapply(thresholds, function(pt) {
    pred <- valid_prob >= pt
    tp <- sum(pred & valid$depression_risk == 1)
    fp <- sum(pred & valid$depression_risk == 0)
    data.frame(
      threshold = pt,
      model = tp / nrow(valid) - fp / nrow(valid) * pt / (1 - pt),
      treat_all = prevalence - (1 - prevalence) * pt / (1 - pt),
      treat_none = 0
    )
  }))
  write.csv(dca, file.path(output_dir, "decision_curve_complete_case.csv"), row.names = FALSE)
  png(file.path(output_dir, "decision_curve_complete_case.png"), width = 1800, height = 1400, res = 220)
  plot(dca$threshold, dca$model, type = "l", lwd = 2.5, col = "#0E7C66",
       xlab = "Threshold probability", ylab = "Net benefit",
       main = "Decision curve analysis - validation set",
       ylim = c(-0.05, max(c(dca$model, dca$treat_all, 0), na.rm = TRUE) + 0.02))
  lines(dca$threshold, dca$treat_all, lwd = 2, lty = 2, col = "#555555")
  lines(dca$threshold, dca$treat_none, lwd = 2, lty = 3, col = "#111111")
  legend("topright", legend = c("Model", "Treat all", "Treat none"), col = c("#0E7C66", "#555555", "#111111"),
         lwd = c(2.5, 2, 2), lty = c(1, 2, 3), bty = "n")
  dev.off()
  impact <- do.call(rbind, lapply(thresholds, function(pt) {
    high <- valid_prob >= pt
    data.frame(
      threshold = pt,
      predicted_high_risk_per_1000 = mean(high) * 1000,
      true_positive_per_1000 = mean(high & valid$depression_risk == 1) * 1000
    )
  }))
  write.csv(impact, file.path(output_dir, "clinical_impact_complete_case.csv"), row.names = FALSE)
  png(file.path(output_dir, "clinical_impact_complete_case.png"), width = 1800, height = 1400, res = 220)
  plot(impact$threshold, impact$predicted_high_risk_per_1000, type = "l", lwd = 2.5, col = "#8B5CF6",
       xlab = "Threshold probability", ylab = "Number per 1000", main = "Clinical impact curve - validation set",
       ylim = c(0, max(impact$predicted_high_risk_per_1000, na.rm = TRUE)))
  lines(impact$threshold, impact$true_positive_per_1000, lwd = 2.5, col = "#DC2626")
  legend("topright", legend = c("Predicted high risk", "True positives"), col = c("#8B5CF6", "#DC2626"), lwd = 2.5, bty = "n")
  dev.off()
  png(file.path(output_dir, "risk_distribution_complete_case.png"), width = 1800, height = 1400, res = 220)
  d0 <- density(valid_prob[valid$depression_risk == 0], from = 0, to = 1, na.rm = TRUE)
  d1 <- density(valid_prob[valid$depression_risk == 1], from = 0, to = 1, na.rm = TRUE)
  plot(d0, lwd = 2.5, col = "#1F6FEB", xlim = c(0, 1), xlab = "Predicted probability", ylab = "Density",
       main = "Predicted risk distribution - validation set")
  lines(d1, lwd = 2.5, col = "#DC2626")
  legend("topright", legend = c("No depression risk", "Depression risk"), col = c("#1F6FEB", "#DC2626"), lwd = 2.5, bty = "n")
  dev.off()
  auc_out
}

run_shift_depression_pipeline <- function(
  data_xlsx = Sys.getenv("NURSE_DATA_XLSX", "D:/护士队列/data/data.xlsx"),
  output_dir = Sys.getenv("NURSE_OUTPUT_DIR", file.path(getwd(), "analysis_outputs", "shift_sleep_depression_model")),
  delete_missing = TRUE
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  csv_path <- extract_if_needed(data_xlsx, output_dir)
  raw <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "N/A", "NULL", "null"))
  dat <- build_dataset(raw)
  vars <- candidate_variables()
  missing_summary <- data.frame(
    variable = c("depression_risk", vars),
    missing_n = vapply(dat[c("depression_risk", vars)], function(x) sum(is.na(x)), integer(1)),
    missing_percent = vapply(dat[c("depression_risk", vars)], function(x) mean(is.na(x)) * 100, numeric(1)),
    stringsAsFactors = FALSE
  )
  missing_summary <- missing_summary[order(-missing_summary$missing_n), ]
  write.csv(missing_summary, file.path(output_dir, "missing_summary.csv"), row.names = FALSE)
  complete_idx <- stats::complete.cases(dat[, c("depression_risk", vars)])
  clean_dat <- dat[complete_idx, , drop = FALSE]
  deleted_summary <- data.frame(
    cohort_after_shift_filter = nrow(dat),
    complete_case_n = nrow(clean_dat),
    deleted_n = nrow(dat) - nrow(clean_dat),
    deleted_percent = (nrow(dat) - nrow(clean_dat)) / nrow(dat) * 100,
    event_n_before_deletion = sum(dat$depression_risk == 1, na.rm = TRUE),
    event_n_after_deletion = sum(clean_dat$depression_risk == 1),
    event_percent_after_deletion = mean(clean_dat$depression_risk == 1) * 100
  )
  write.csv(deleted_summary, file.path(output_dir, "deleted_cases_summary.csv"), row.names = FALSE)
  write.csv(clean_dat, file.path(output_dir, "complete_case_model_data.csv"), row.names = FALSE)
  make_baseline_table(clean_dat, raw, output_dir)
  vars <- vars[vapply(clean_dat[vars], function(x) length(unique(x)) > 1, logical(1))]
  hi <- which(clean_dat$depression_risk == 1)
  lo <- which(clean_dat$depression_risk == 0)
  train_idx <- c(sample(hi, floor(length(hi) * 0.70)), sample(lo, floor(length(lo) * 0.70)))
  train <- clean_dat[train_idx, , drop = FALSE]
  valid <- clean_dat[-train_idx, , drop = FALSE]
  univ <- do.call(rbind, lapply(vars, function(v) {
    fit <- try(glm(as.formula(paste("depression_risk ~", v)), data = train, family = binomial()), silent = TRUE)
    if (inherits(fit, "try-error")) return(data.frame(variable = v, beta = NA_real_, OR = NA_real_, p_value = NA_real_))
    sm <- summary(fit)$coefficients
    data.frame(variable = v, beta = sm[2, "Estimate"], OR = exp(sm[2, "Estimate"]), p_value = sm[2, "Pr(>|z|)"])
  }))
  write.csv(univ[order(univ$p_value), ], file.path(output_dir, "univariate_logistic_results.csv"), row.names = FALSE)
  selected <- univ$variable[!is.na(univ$p_value) & univ$p_value < 0.05]
  if (length(selected) < 2) selected <- vars
  full_fit <- glm(as.formula(paste("depression_risk ~", paste(selected, collapse = " + "))), data = train, family = binomial())
  final_fit <- MASS::stepAIC(full_fit, direction = "both", trace = FALSE)
  coef_tab <- data.frame(
    variable = names(coef(final_fit)),
    beta = as.numeric(coef(final_fit)),
    OR = exp(as.numeric(coef(final_fit))),
    confint_low = exp(confint.default(final_fit)[, 1]),
    confint_high = exp(confint.default(final_fit)[, 2]),
    p_value = summary(final_fit)$coefficients[, "Pr(>|z|)"],
    row.names = NULL
  )
  write.csv(coef_tab, file.path(output_dir, "final_logistic_coefficients.csv"), row.names = FALSE)
  train_prob <- as.numeric(predict(final_fit, newdata = train, type = "response"))
  valid_prob <- as.numeric(predict(final_fit, newdata = valid, type = "response"))
  auc_out <- plot_validation(train, valid, train_prob, valid_prob, output_dir)
  plot_nomogram(final_fit, train, output_dir)
  saveRDS(
    list(model = final_fit, variables = names(coef(final_fit))[-1], auc = auc_out, deleted_summary = deleted_summary),
    file.path(output_dir, "shift_depression_complete_case_model.rds")
  )
  report <- c(
    "# Shift nurse sleep-rhythm related depression risk prediction model",
    "",
    paste0("- Cohort after shift/night filter: ", nrow(dat)),
    paste0("- Complete cases retained: ", nrow(clean_dat)),
    paste0("- Deleted due to missing candidate variables: ", deleted_summary$deleted_n, " (", sprintf("%.1f", deleted_summary$deleted_percent), "%)"),
    paste0("- Events after deletion: ", deleted_summary$event_n_after_deletion, " (", sprintf("%.1f", deleted_summary$event_percent_after_deletion), "%)"),
    paste0("- Training/validation split: ", nrow(train), " / ", nrow(valid)),
    paste0("- Training AUC: ", sprintf("%.3f", auc_out$auc[auc_out$dataset == "Training"])),
    paste0("- Validation AUC: ", sprintf("%.3f", auc_out$auc[auc_out$dataset == "Validation"])),
    "",
    "## Final predictors",
    paste(names(coef(final_fit))[-1], collapse = ", "),
    "",
    "Key outputs: baseline_table_complete_case.csv, missing_summary.csv, deleted_cases_summary.csv,",
    "final_logistic_coefficients.csv, roc_complete_case.png, nomogram_complete_case.png,",
    "calibration_complete_case.png, decision_curve_complete_case.png, clinical_impact_complete_case.png."
  )
  writeLines(report, file.path(output_dir, "model_report_complete_case.md"), useBytes = TRUE)
  message("Done. Outputs written to: ", output_dir)
  invisible(list(model = final_fit, auc = auc_out, deleted_summary = deleted_summary, output_dir = output_dir))
}

if (!identical(Sys.getenv("RUN_SHIFT_DEPRESSION_PIPELINE"), "false")) {
  run_shift_depression_pipeline()
}
