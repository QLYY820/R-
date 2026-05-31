# ============================================================
# 堡垒机 / RStudio Server 版复现代码
# 题目：
# Symptom Network of Multisite Musculoskeletal Discomfort and
# Physical/Mental Fatigue Among Clinical Nurses
#
# 用法：
# 1. 在 RStudio Server 中打开本脚本。
# 2. 确认数据对象已经在环境中，优先对象名为：合并数据
#    如果没有对象，本脚本会尝试读取当前目录下的 data.xlsx。
# 3. 点击 Source，或按 Ctrl + Shift + Enter 一键运行。
# ============================================================

# Do not clear the whole workspace here. In RStudio Server/bastion workflows,
# the uploaded dataset is often already loaded in the Global Environment.
# Clearing everything would remove objects such as `合并数据`.
DATA_OBJECT_CANDIDATES <- c("合并数据", "合并数据64114_去重后", "data")
keep_data_objects <- intersect(DATA_OBJECT_CANDIDATES, ls(envir = .GlobalEnv))
rm(list = setdiff(ls(envir = .GlobalEnv), c(keep_data_objects, "DATA_OBJECT_CANDIDATES")),
   envir = .GlobalEnv)
options(stringsAsFactors = FALSE)
set.seed(20260531)

# ---------------------- 0. 用户设置 -------------------------

# Settings can be changed without editing this file by setting environment
# variables before source(). The short GitHub loaders do this automatically.
env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) return(default)
  tolower(value) %in% c("1", "true", "yes", "y")
}

env_int <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = NA_character_)))
  ifelse(is.na(value), default, value)
}

env_chr <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

# 第一次试跑建议 TRUE，只跑前 1500 行，并用 Monte Carlo NIRA。
# 正式复现改成 FALSE。
FAST_TEST <- env_flag("SOMATIC_FAST_TEST", FALSE)

# 是否安装缺失包。堡垒机如果不能联网，可改成 FALSE，
# 然后请管理员预装 data.table/readxl/dplyr/ggplot2/glmnet/igraph/scales。
INSTALL_MISSING <- env_flag("SOMATIC_INSTALL_MISSING", TRUE)

# NIRA_MODE:
# "exact" = 精确枚举 2^23 种状态，推荐正式结果使用，但耗时更久。
# "mc"    = Monte Carlo Gibbs 抽样，推荐先测试。
NIRA_MODE <- env_chr("SOMATIC_NIRA_MODE", "exact")

# Bootstrap 很耗时。投稿最终版建议 TRUE 且 BOOT_N = 1000；
# 复现主结果和模拟干预时可先 FALSE。
RUN_BOOTSTRAP <- env_flag("SOMATIC_RUN_BOOTSTRAP", FALSE)
BOOT_N <- env_int("SOMATIC_BOOT_N", 50)
RUN_CASE_STABILITY <- env_flag("SOMATIC_RUN_CASE_STABILITY", RUN_BOOTSTRAP)
BOOT_CASE_N <- env_int("SOMATIC_BOOT_CASE_N", BOOT_N)
BOOT_SEED <- env_int("SOMATIC_BOOT_SEED", 20260531)
CS_COR_THRESHOLD <- 0.70
CS_QUANTILE <- 0.05
CASE_DROP_PROPORTIONS <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70)

# 输出目录。RStudio Server 默认保存到当前工作目录下。
OUT_DIR <- file.path(getwd(), "somatic_exhaustion_network_R_outputs_main_qc")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (FAST_TEST) {
  NIRA_MODE <- "mc"
  RUN_BOOTSTRAP <- FALSE
  RUN_CASE_STABILITY <- FALSE
}

# ---------------------- 1. 安装并加载包 ----------------------

packages <- c("data.table", "readxl", "dplyr", "tidyr", "ggplot2",
              "glmnet", "igraph", "scales")

install_and_load <- function(pkgs) {
  miss <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(miss) > 0 && INSTALL_MISSING) {
    install.packages(miss, dependencies = TRUE)
  }
  for (p in pkgs) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}

install_and_load(packages)

# ---------------------- 2. 读取数据 --------------------------

get_raw_data <- function() {
  candidate_files <- c(
    "data.xlsx",
    "合并数据.xlsx",
    "护士队列_data.xlsx"
  )
  for (fp in candidate_files) {
    if (file.exists(fp)) {
      message("读取当前目录文件：", fp)
      return(as.data.frame(readxl::read_excel(fp, .name_repair = "unique")))
    }
  }

  candidate_objects <- DATA_OBJECT_CANDIDATES
  for (nm in candidate_objects) {
    if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
      obj <- get(nm, envir = .GlobalEnv)
      if (is.data.frame(obj)) {
        message("使用环境中的数据对象：", nm)
        return(as.data.frame(obj))
      }
    }
  }

  stop(
    "没有找到数据。请先在 RStudio Server 中加载对象 `合并数据`，",
    "或者把 data.xlsx 上传到当前工作目录。"
  )
}

raw <- get_raw_data()
message("原始数据维度：", nrow(raw), " 行；", ncol(raw), " 列")

if (FAST_TEST) {
  raw <- raw[seq_len(min(1500, nrow(raw))), , drop = FALSE]
  message("FAST_TEST=TRUE，仅使用前 ", nrow(raw), " 行。")
}

# ---------------------- 3. 节点定义 --------------------------

node_names <- c(paste0("Pain_", 1:9), paste0("Fatigue_", 1:14))

node_info <- data.frame(
  node = node_names,
  abbr = c(paste0("P", 1:9), paste0("PF", 1:8), paste0("MF", 1:6)),
  label_cn = c(
    "颈部", "肩部", "上背部", "肘部", "腕/手部", "下背部", "臀/股部", "膝部", "踝/足部",
    "被疲劳困扰", "需要更多休息", "犯困/昏昏欲睡", "着手做事费力", "继续做事力不从心",
    "体力不够", "肌肉力量减小", "虚弱感", "集中注意困难", "头脑不清晰/不敏捷",
    "口头不利落", "找词困难", "记忆力不如往常", "兴趣减退"
  ),
  label_en = c(
    "Neck", "Shoulder", "Upper back", "Elbow", "Wrist/hand",
    "Lower back", "Hip/thigh", "Knee", "Ankle/foot",
    "Troubled by fatigue", "Need more rest", "Sleepy/drowsy",
    "Difficulty starting things", "Difficulty sustaining activity",
    "Insufficient physical energy", "Reduced muscle strength", "Feeling weak",
    "Difficulty concentrating", "Not clear/sharp thinking", "Speech slips",
    "Word-finding difficulty", "Worse memory", "Reduced enjoyment"
  ),
  community = c(rep("Pain", 9), rep("Physical", 8), rep("Mental", 6)),
  stringsAsFactors = FALSE
)
rownames(node_info) <- node_info$node

pain12_cols <- paste0("E_q24_", 1:9, "_1")  # 12个月
pain7_cols  <- paste0("E_q24_", 1:9, "_4")  # 7天
fatigue_cols <- paste0("G_q3_", 1:14)
reverse_fatigue_items <- c(10, 13, 14)

time_cols <- c("timetaken.x", "timetaken.y", "timetaken.x.x", "timetaken.y.y")
demo_core_cols <- c(
  "id", "A_q2", "A_year", "A_q5", "work_y", "A_q9", "A_q10", "A_q11",
  "A_q12", "A_q15", "A_q16", "C_q1", "C_q8", "C_q42", "C_q44"
)
needed_cols <- unique(c("id", "A_year", "work_y", time_cols, pain12_cols, pain7_cols, fatigue_cols))
missing_cols <- setdiff(needed_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("原始数据缺少以下列：\n", paste(missing_cols, collapse = ", "))
}

to01_pain <- function(x) {
  z <- trimws(as.character(x))
  out <- rep(NA_real_, length(z))
  out[z %in% c("1", "是", "有", "yes", "Yes", "Y", "TRUE", "True", "true")] <- 1
  out[z %in% c("0", "否", "无", "no", "No", "N", "FALSE", "False", "false")] <- 0
  suppressWarnings(num <- as.numeric(z))
  out[is.na(out) & !is.na(num) & num == 1] <- 1
  out[is.na(out) & !is.na(num) & num == 0] <- 0
  out
}

to01_fatigue <- function(x, reverse = FALSE) {
  suppressWarnings(v <- as.numeric(as.character(x)))
  v[!(v %in% c(0, 1))] <- NA_real_
  if (reverse) v <- ifelse(is.na(v), NA_real_, 1 - v)
  v
}

build_nodes <- function(raw, pain_cols) {
  pain <- as.data.frame(lapply(raw[pain_cols], to01_pain))
  names(pain) <- paste0("Pain_", 1:9)

  fatigue <- as.data.frame(lapply(seq_along(fatigue_cols), function(i) {
    to01_fatigue(raw[[fatigue_cols[i]]], reverse = i %in% reverse_fatigue_items)
  }))
  names(fatigue) <- paste0("Fatigue_", 1:14)

  dat <- cbind(pain, fatigue)
  dat <- dat[, node_names, drop = FALSE]
  dat[] <- lapply(dat, as.numeric)
  dat
}

clean_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

safe_col <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))
}

apply_main_qc <- function(raw) {
  n <- nrow(raw)
  age <- 2025 - clean_num(safe_col(raw, "A_year"))
  tenure <- 2025 - clean_num(safe_col(raw, "work_y"))

  time_df <- as.data.frame(lapply(time_cols, function(nm) clean_num(safe_col(raw, nm))))
  complete_time <- stats::complete.cases(time_df)
  total_time <- rowSums(time_df, na.rm = FALSE)
  total_time[!complete_time] <- NA_real_

  id_chr <- trimws(as.character(safe_col(raw, "id")))
  duplicate_id <- id_chr != "" & !is.na(id_chr) &
    (duplicated(id_chr) | duplicated(id_chr, fromLast = TRUE))

  invalid_age <- is.na(age) | age < 18 | age > 65
  invalid_tenure <- is.na(tenure) | tenure < 1
  inconsistent_age_tenure <- !invalid_age & !invalid_tenure & tenure > (age - 16)
  short_completion_time <- is.na(total_time) | total_time < 600

  pain12_raw <- as.data.frame(lapply(raw[pain12_cols], to01_pain))
  pain7_raw <- as.data.frame(lapply(raw[pain7_cols], to01_pain))
  nmq_logic_inconsistent <- rowSums((pain7_raw == 1) & (pain12_raw == 0), na.rm = TRUE) > 0

  keep <- !(invalid_age | invalid_tenure | inconsistent_age_tenure |
              short_completion_time | duplicate_id)

  qc_records <- data.frame(
    source_row = seq_len(n),
    age = age,
    work_tenure = tenure,
    total_completion_time_seconds = total_time,
    invalid_age = invalid_age,
    invalid_tenure = invalid_tenure,
    inconsistent_age_tenure = inconsistent_age_tenure,
    short_completion_time = short_completion_time,
    nmq_logic_inconsistent = nmq_logic_inconsistent,
    duplicate_id = duplicate_id,
    keep = keep,
    stringsAsFactors = FALSE
  )

  current <- rep(TRUE, n)
  flow <- data.frame(step = "Raw records", n_excluded_at_step = 0L, n_remaining = n)
  add_step <- function(label, flag, exclude = TRUE) {
    excluded <- current & flag
    if (exclude) current <<- current & !flag
    flow <<- rbind(
      flow,
      data.frame(
        step = label,
        n_excluded_at_step = if (exclude) sum(excluded) else 0L,
        n_remaining = sum(current),
        stringsAsFactors = FALSE
      )
    )
  }
  add_step("Age outside 18-65 years or missing", invalid_age)
  add_step("Work tenure <1 year or missing", invalid_tenure)
  add_step("Work tenure greater than age minus 16 years", inconsistent_age_tenure)
  add_step("Total completion time <10 minutes or missing", short_completion_time)
  add_step("NMQ 7-day yes but 12-month no; flagged only, not excluded",
           nmq_logic_inconsistent, exclude = FALSE)
  add_step("Duplicated participant id", duplicate_id)

  component_counts <- data.frame(
    criterion = c(
      "Age outside 18-65 years or missing",
      "Work tenure <1 year or missing",
      "Work tenure greater than age minus 16 years",
      "Total completion time <10 minutes or missing",
      "NMQ 7-day yes but 12-month no",
      "Duplicated participant id"
    ),
    n_flagged_overall = c(
      sum(invalid_age), sum(invalid_tenure), sum(inconsistent_age_tenure),
      sum(short_completion_time), sum(nmq_logic_inconsistent), sum(duplicate_id)
    ),
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    metric = c("raw_n", "final_qc_keep", "final_qc_excluded"),
    value = c(n, sum(keep), n - sum(keep)),
    stringsAsFactors = FALSE
  )

  list(
    keep = keep,
    age = age,
    tenure = tenure,
    total_time = total_time,
    records = qc_records,
    flow = flow,
    component_counts = component_counts,
    summary = summary
  )
}

qc <- apply_main_qc(raw)
raw_qc <- raw[qc$keep, , drop = FALSE]

message("质控后样本量：", nrow(raw_qc), " / ", nrow(raw))
message("NMQ 7天=是且12个月=否：仅标记，不排除；保留样本中标记人数 = ",
        sum(qc$records$keep & qc$records$nmq_logic_inconsistent))

dat12 <- build_nodes(raw_qc, pain12_cols)
dat7  <- build_nodes(raw_qc, pain7_cols)

data.table::fwrite(qc$flow, file.path(OUT_DIR, "quality_control_flow.csv"))
data.table::fwrite(qc$component_counts, file.path(OUT_DIR, "quality_control_component_counts.csv"))
data.table::fwrite(qc$records, file.path(OUT_DIR, "quality_control_record_flags.csv"))
data.table::fwrite(qc$summary, file.path(OUT_DIR, "quality_control_summary.csv"))
data.table::fwrite(dat12, file.path(OUT_DIR, "selected_nodes_12m_cleaned.csv"))
data.table::fwrite(dat7,  file.path(OUT_DIR, "selected_nodes_7d_cleaned.csv"))
data.table::fwrite(dat12, file.path(OUT_DIR, "selected_nodes_12m.csv"))
data.table::fwrite(dat7,  file.path(OUT_DIR, "selected_nodes_7d.csv"))

demo_existing <- intersect(demo_core_cols, names(raw_qc))
demo_core <- raw_qc[, demo_existing, drop = FALSE]
demo_core$age <- qc$age[qc$keep]
demo_core$work_tenure <- qc$tenure[qc$keep]
demo_core$total_completion_time_seconds <- qc$total_time[qc$keep]
data.table::fwrite(demo_core, file.path(OUT_DIR, "cleaned_demographic_core.csv"))
data.table::fwrite(node_info, file.path(OUT_DIR, "node_dictionary.csv"))

# ---------------------- 4. 人口学/职业特征表 -----------------

make_demo_table <- function(raw) {
  n <- nrow(raw)
  age <- 2025 - clean_num(safe_col(raw, "A_year"))
  age[age < 18 | age > 65] <- NA
  tenure <- 2025 - clean_num(safe_col(raw, "work_y"))
  tenure[tenure < 0 | tenure > 50] <- NA
  height <- clean_num(safe_col(raw, "A_q15"))
  weight <- clean_num(safe_col(raw, "A_q16"))
  bmi <- weight / ((height / 100)^2)
  bmi[bmi < 14 | bmi > 50] <- NA

  cont_summary <- function(x, label) {
    x <- x[!is.na(x)]
    data.frame(
      characteristic = label,
      category = "",
      n = length(x),
      value = sprintf("%.1f (%.1f)", mean(x), stats::sd(x)),
      stringsAsFactors = FALSE
    )
  }

  cat_summary <- function(x, label, map = NULL) {
    z <- as.character(x)
    z[z %in% c("", "NA", "-1")] <- NA
    tab <- sort(table(z, useNA = "no"), decreasing = TRUE)
    if (!length(tab)) {
      return(data.frame(characteristic = label, category = NA, n = 0, value = NA))
    }
    out <- data.frame(
      characteristic = c(label, rep("", length(tab) - 1)),
      category = names(tab),
      n = as.integer(tab),
      value = sprintf("%d (%.1f%%)", as.integer(tab), 100 * as.integer(tab) / sum(tab)),
      stringsAsFactors = FALSE
    )
    if (!is.null(map)) {
      out$category <- ifelse(out$category %in% names(map), map[out$category], out$category)
    }
    out
  }

  sex_map <- c("1" = "Male", "2" = "Female")
  edu_map <- c("1" = "Technical secondary", "2" = "Junior college",
               "3" = "Bachelor", "4" = "Master or above")
  shift_map <- c("1" = "Day shift only", "2" = "Evening shift only",
                 "3" = "Night shift only", "4" = "Rotating day/night shifts")
  night_map <- c("1" = "<=4", "2" = "5-9", "3" = ">=10")
  yesno_map <- c("1" = "Yes", "2" = "No", "-3" = "Not applicable/skipped")

  dplyr::bind_rows(
    cont_summary(age, "Age, years"),
    cont_summary(tenure, "Work tenure, years"),
    cont_summary(bmi, "Body mass index, kg/m2"),
    cat_summary(safe_col(raw, "A_q2"), "Sex", sex_map),
    cat_summary(safe_col(raw, "A_q5"), "Highest educational level", edu_map),
    cat_summary(safe_col(raw, "A_q9"), "Department"),
    cat_summary(safe_col(raw, "A_q10"), "Employment type"),
    cat_summary(safe_col(raw, "A_q11"), "Professional title"),
    cat_summary(safe_col(raw, "A_q12"), "Administrative position"),
    cat_summary(safe_col(raw, "C_q1"), "Main schedule in previous 6 months", shift_map),
    cat_summary(safe_col(raw, "C_q8"), "Night shifts per month", night_map),
    cat_summary(safe_col(raw, "C_q42"), "Weeks >40 work hours in previous month"),
    cat_summary(safe_col(raw, "C_q44"), "Work schedule overlaps usual sleep time", yesno_map)
  )
}

demo_table <- make_demo_table(raw_qc)
data.table::fwrite(demo_table, file.path(OUT_DIR, "Table1_demographic_occupational_characteristics.csv"))

# ---------------------- 5. Ising 网络估计 --------------------

loglik_binomial <- function(y, p) {
  p <- pmin(pmax(p, 1e-8), 1 - 1e-8)
  sum(y * log(p) + (1 - y) * log(1 - p))
}

fit_node_lasso_ebic <- function(y, X, gamma = 0.25, nlambda = 100) {
  if (length(unique(y[!is.na(y)])) < 2) return(rep(0, ncol(X)))

  fit <- glmnet::glmnet(
    x = X, y = y,
    family = "binomial",
    alpha = 1,
    standardize = FALSE,
    intercept = TRUE,
    nlambda = nlambda
  )

  prob <- predict(fit, newx = X, type = "response")
  ll <- apply(prob, 2, function(p) loglik_binomial(y, p))
  beta <- as.matrix(stats::coef(fit))[-1, , drop = FALSE]
  k <- colSums(abs(beta) > 1e-8)
  p_pred <- ncol(X)
  ebic <- -2 * ll + k * log(length(y)) + 2 * gamma * lchoose(p_pred, k)
  best <- which.min(ebic)
  as.numeric(beta[, best])
}

estimate_ising_glmnet <- function(dat, gamma = 0.25, rule = "AND") {
  dat <- as.data.frame(dat)
  dat[] <- lapply(dat, as.numeric)
  nodes <- names(dat)
  p <- length(nodes)
  X_all <- as.matrix(dat)
  directed <- matrix(0, p, p, dimnames = list(nodes, nodes))

  for (i in seq_len(p)) {
    y <- X_all[, i]
    pred_idx <- setdiff(seq_len(p), i)
    X <- X_all[, pred_idx, drop = FALSE]
    directed[i, pred_idx] <- fit_node_lasso_ebic(y, X, gamma = gamma)
    message(sprintf("  fitted node %02d/%02d: %s", i, p, nodes[i]))
  }

  W <- matrix(0, p, p, dimnames = list(nodes, nodes))
  for (i in seq_len(p - 1)) {
    for (j in (i + 1):p) {
      b1 <- directed[i, j]
      b2 <- directed[j, i]
      selected <- if (rule == "AND") (b1 != 0 && b2 != 0) else (b1 != 0 || b2 != 0)
      if (selected) {
        vals <- c(b1, b2)
        vals <- vals[vals != 0]
        W[i, j] <- W[j, i] <- mean(vals)
      }
    }
  }
  list(W = W, directed = directed)
}

edge_table <- function(W) {
  nodes <- colnames(W)
  rows <- list()
  k <- 1
  for (i in seq_len(ncol(W) - 1)) {
    for (j in (i + 1):ncol(W)) {
      if (!is.na(W[i, j]) && W[i, j] != 0) {
        rows[[k]] <- data.frame(
          source = nodes[i],
          target = nodes[j],
          source_abbr = node_info[nodes[i], "abbr"],
          target_abbr = node_info[nodes[j], "abbr"],
          source_label = node_info[nodes[i], "label_en"],
          target_label = node_info[nodes[j], "label_en"],
          source_community = node_info[nodes[i], "community"],
          target_community = node_info[nodes[j], "community"],
          weight = W[i, j],
          abs_weight = abs(W[i, j]),
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
    }
  }
  if (!length(rows)) return(data.frame())
  dplyr::bind_rows(rows) |>
    dplyr::arrange(dplyr::desc(abs_weight))
}

local_clustering <- function(W) {
  A <- (abs(W) > 0) * 1
  diag(A) <- 0
  out <- rep(0, nrow(A))
  for (i in seq_len(nrow(A))) {
    nb <- which(A[i, ] == 1)
    if (length(nb) < 2) {
      out[i] <- 0
    } else {
      out[i] <- sum(A[nb, nb]) / 2 / (length(nb) * (length(nb) - 1) / 2)
    }
  }
  out
}

centrality_summary <- function(W) {
  nodes <- colnames(W)
  comm <- node_info[nodes, "community"]
  strength <- rowSums(abs(W), na.rm = TRUE)
  expected_influence <- rowSums(W, na.rm = TRUE)
  bridge_strength <- bridge_ei <- rep(0, length(nodes))

  for (i in seq_along(nodes)) {
    other <- comm != comm[i]
    bridge_strength[i] <- sum(abs(W[i, other]), na.rm = TRUE)
    bridge_ei[i] <- sum(W[i, other], na.rm = TRUE)
  }

  g <- igraph::graph_from_adjacency_matrix(abs(W), mode = "undirected",
                                           weighted = TRUE, diag = FALSE)
  if (igraph::ecount(g) > 0) {
    igraph::E(g)$distance <- 1 / (igraph::E(g)$weight + 1e-8)
    closeness <- suppressWarnings(igraph::closeness(g, weights = igraph::E(g)$distance,
                                                    normalized = TRUE))
    betweenness <- suppressWarnings(igraph::betweenness(g, weights = igraph::E(g)$distance,
                                                        normalized = TRUE))
  } else {
    closeness <- betweenness <- rep(NA_real_, length(nodes))
  }

  data.frame(
    node = nodes,
    abbr = node_info[nodes, "abbr"],
    label_en = node_info[nodes, "label_en"],
    community = comm,
    strength = strength,
    expected_influence = expected_influence,
    bridge_strength = bridge_strength,
    bridge_expected_influence = bridge_ei,
    closeness = as.numeric(closeness),
    betweenness = as.numeric(betweenness),
    clustering = local_clustering(W),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(dplyr::desc(strength))
}

node_predictability <- function(dat, W) {
  dat <- as.data.frame(dat)
  dat[] <- lapply(dat, as.numeric)
  nodes <- names(dat)
  X_all <- as.matrix(dat)
  rows <- list()

  for (i in seq_along(nodes)) {
    y <- X_all[, i]
    preds <- which(W[i, ] != 0)
    if (length(preds) == 0 || length(unique(y)) < 2) {
      phat <- rep(mean(y), length(y))
    } else {
      X <- cbind(Intercept = 1, X_all[, preds, drop = FALSE])
      fit <- suppressWarnings(glm.fit(x = X, y = y, family = binomial()))
      phat <- as.numeric(plogis(X %*% fit$coefficients))
    }

    ll_model <- loglik_binomial(y, phat)
    p0 <- mean(y)
    ll_null <- loglik_binomial(y, rep(p0, length(y)))
    cox_snell <- 1 - exp((2 / length(y)) * (ll_null - ll_model))
    max_r2 <- 1 - exp((2 / length(y)) * ll_null)
    nagelkerke <- ifelse(max_r2 > 0, cox_snell / max_r2, NA_real_)
    accuracy <- mean((phat >= 0.5) == y)

    rows[[i]] <- data.frame(
      node = nodes[i],
      prevalence = mean(y),
      nagelkerke_r2 = nagelkerke,
      accuracy = accuracy,
      stringsAsFactors = FALSE
    )
  }
  dplyr::bind_rows(rows)
}

# ---------------------- 6. Bootstrap and stability -----------------------

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman"))
}

all_edge_weights <- function(W) {
  nodes <- colnames(W)
  rows <- list()
  k <- 1
  for (i in seq_len(ncol(W) - 1)) {
    for (j in (i + 1):ncol(W)) {
      rows[[k]] <- data.frame(
        source = nodes[i],
        target = nodes[j],
        source_abbr = node_info[nodes[i], "abbr"],
        target_abbr = node_info[nodes[j], "abbr"],
        pair = paste(node_info[nodes[i], "abbr"], node_info[nodes[j], "abbr"], sep = "--"),
        weight = W[i, j],
        abs_weight = abs(W[i, j]),
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
  dplyr::bind_rows(rows)
}

metric_table_for_stability <- function(W) {
  centrality_summary(W) |>
    dplyr::select(node, strength, expected_influence,
                  bridge_strength, bridge_expected_influence)
}

bootstrap_network <- function(dat, W_original, B = 50, gamma = 0.25,
                              rule = "AND", seed = 20260531) {
  set.seed(seed)
  dat <- as.data.frame(dat)
  nodes <- names(dat)
  p <- length(nodes)
  n <- nrow(dat)

  original_edges <- all_edge_weights(W_original) |>
    dplyr::rename(original_weight = weight, original_abs_weight = abs_weight)

  edge_store <- matrix(NA_real_, nrow = B, ncol = nrow(original_edges))
  colnames(edge_store) <- original_edges$pair

  metric_names <- c("strength", "expected_influence",
                    "bridge_strength", "bridge_expected_influence")
  metric_store <- array(
    NA_real_,
    dim = c(B, p, length(metric_names)),
    dimnames = list(paste0("boot_", seq_len(B)), nodes, metric_names)
  )

  for (b in seq_len(B)) {
    message(sprintf("Bootstrap %d/%d", b, B))
    idx <- sample.int(n, n, replace = TRUE)
    Wb <- estimate_ising_glmnet(dat[idx, , drop = FALSE], gamma = gamma, rule = rule)$W
    edge_store[b, ] <- all_edge_weights(Wb)$weight

    cb <- metric_table_for_stability(Wb)
    cb <- cb[match(nodes, cb$node), , drop = FALSE]
    for (m in metric_names) metric_store[b, , m] <- cb[[m]]
  }

  edge_ci <- cbind(
    original_edges,
    boot_mean = colMeans(edge_store, na.rm = TRUE),
    boot_lcl = apply(edge_store, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
    boot_ucl = apply(edge_store, 2, stats::quantile, probs = 0.975, na.rm = TRUE)
  )

  centrality_rows <- list()
  k <- 1
  for (m in metric_names) {
    vals <- metric_store[, , m, drop = FALSE][, , 1]
    centrality_rows[[k]] <- data.frame(
      metric = m,
      node = nodes,
      abbr = node_info[nodes, "abbr"],
      label_en = node_info[nodes, "label_en"],
      community = node_info[nodes, "community"],
      boot_mean = colMeans(vals, na.rm = TRUE),
      boot_lcl = apply(vals, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
      boot_ucl = apply(vals, 2, stats::quantile, probs = 0.975, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }

  list(
    edge_ci = edge_ci,
    centrality_ci = dplyr::bind_rows(centrality_rows)
  )
}

case_drop_stability <- function(dat, W_original, B = 50, gamma = 0.25,
                                rule = "AND", seed = 20260531) {
  set.seed(seed + 101)
  dat <- as.data.frame(dat)
  nodes <- names(dat)
  n <- nrow(dat)
  metric_names <- c("strength", "expected_influence",
                    "bridge_strength", "bridge_expected_influence")
  original <- metric_table_for_stability(W_original)
  original <- original[match(nodes, original$node), , drop = FALSE]

  rows <- list()
  k <- 1
  for (drop_prop in CASE_DROP_PROPORTIONS) {
    keep_n <- max(50, floor(n * (1 - drop_prop)))
    if (keep_n >= n) next
    for (b in seq_len(B)) {
      message(sprintf("Case-drop stability drop=%.0f%% %d/%d",
                      100 * drop_prop, b, B))
      idx <- sample.int(n, keep_n, replace = FALSE)
      Wb <- estimate_ising_glmnet(dat[idx, , drop = FALSE], gamma = gamma, rule = rule)$W
      cb <- metric_table_for_stability(Wb)
      cb <- cb[match(nodes, cb$node), , drop = FALSE]

      for (m in metric_names) {
        rows[[k]] <- data.frame(
          drop_proportion = drop_prop,
          retained_n = keep_n,
          replicate = b,
          metric = m,
          correlation = safe_cor(original[[m]], cb[[m]]),
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
    }
  }

  raw <- dplyr::bind_rows(rows)
  summary <- raw |>
    dplyr::group_by(metric, drop_proportion, retained_n) |>
    dplyr::summarise(
      mean_correlation = mean(correlation, na.rm = TRUE),
      q05_correlation = stats::quantile(correlation, CS_QUANTILE, na.rm = TRUE),
      q95_correlation = stats::quantile(correlation, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  cs <- summary |>
    dplyr::group_by(metric) |>
    dplyr::summarise(
      cs_coefficient = ifelse(
        any(q05_correlation >= CS_COR_THRESHOLD, na.rm = TRUE),
        max(drop_proportion[q05_correlation >= CS_COR_THRESHOLD], na.rm = TRUE),
        0
      ),
      threshold = CS_COR_THRESHOLD,
      quantile = CS_QUANTILE,
      boot_case_n = B,
      .groups = "drop"
    )

  list(raw = raw, summary = summary, cs = cs)
}

# ---------------------- 7. NIRA 模拟干预 ---------------------

sigmoid <- function(x) plogis(pmin(pmax(x, -35), 35))

solve_thresholds_fixed_slope <- function(dat, W) {
  X <- as.matrix(dat)
  prev <- colMeans(X)
  theta <- numeric(ncol(X))
  names(theta) <- colnames(X)
  for (i in seq_len(ncol(X))) {
    linear <- as.numeric(X %*% W[i, ])
    f <- function(a) mean(sigmoid(a + linear)) - prev[i]
    theta[i] <- stats::uniroot(f, lower = -60, upper = 60, tol = 1e-10)$root
  }
  theta
}

bits_from_indices <- function(idx, p) {
  idx <- as.integer(idx)
  powers <- bitwShiftL(1L, 0:(p - 1))
  bits <- sapply(powers, function(pow) as.integer(bitwAnd(idx, pow) != 0L))
  if (p == 1) bits <- matrix(bits, ncol = 1)
  colnames(bits) <- node_names[seq_len(p)]
  bits
}

base_energy <- function(bits, W, theta) {
  as.numeric(bits %*% theta + 0.5 * rowSums((bits %*% W) * bits))
}

run_nira_exact <- function(dat, W, amount_sd = 2, reference_n = 5000,
                           chunk_size = 150000) {
  nodes <- colnames(W)
  p <- length(nodes)
  theta <- solve_thresholds_fixed_slope(dat, W)
  theta_sd <- stats::sd(theta)

  scenarios <- data.frame(
    direction = c("baseline", rep("aggravating", p), rep("alleviating", p)),
    target_index = c(NA_integer_, seq_len(p), seq_len(p)),
    target = c(NA_character_, nodes, nodes),
    shift = c(0, rep(amount_sd * theta_sd, p), rep(-amount_sd * theta_sd, p)),
    stringsAsFactors = FALSE
  )
  nsc <- nrow(scenarios)
  n_states <- 2^p
  starts <- seq(0, n_states - 1, by = chunk_size)
  max_energy <- rep(-Inf, nsc)

  message("NIRA exact pass 1/2")
  for (start in starts) {
    end <- min(start + chunk_size - 1, n_states - 1)
    bits <- bits_from_indices(start:end, p)
    e0 <- base_energy(bits, W, theta)
    for (s in seq_len(nsc)) {
      e <- if (scenarios$direction[s] == "baseline") {
        e0
      } else {
        e0 + scenarios$shift[s] * bits[, scenarios$target_index[s]]
      }
      max_energy[s] <- max(max_energy[s], max(e))
    }
  }

  z <- total_sum <- total_sum2 <- pain_sum <- physical_sum <- mental_sum <- rep(0, nsc)
  pain_idx <- which(node_info[nodes, "community"] == "Pain")
  physical_idx <- which(node_info[nodes, "community"] == "Physical")
  mental_idx <- which(node_info[nodes, "community"] == "Mental")

  message("NIRA exact pass 2/2")
  for (start in starts) {
    end <- min(start + chunk_size - 1, n_states - 1)
    bits <- bits_from_indices(start:end, p)
    e0 <- base_energy(bits, W, theta)
    total_score <- rowSums(bits)
    pain_score <- rowSums(bits[, pain_idx, drop = FALSE])
    physical_score <- rowSums(bits[, physical_idx, drop = FALSE])
    mental_score <- rowSums(bits[, mental_idx, drop = FALSE])

    for (s in seq_len(nsc)) {
      e <- if (scenarios$direction[s] == "baseline") {
        e0
      } else {
        e0 + scenarios$shift[s] * bits[, scenarios$target_index[s]]
      }
      w <- exp(e - max_energy[s])
      z[s] <- z[s] + sum(w)
      total_sum[s] <- total_sum[s] + sum(w * total_score)
      total_sum2[s] <- total_sum2[s] + sum(w * total_score^2)
      pain_sum[s] <- pain_sum[s] + sum(w * pain_score)
      physical_sum[s] <- physical_sum[s] + sum(w * physical_score)
      mental_sum[s] <- mental_sum[s] + sum(w * mental_score)
    }
  }

  mean_total <- total_sum / z
  sd_total <- sqrt(pmax(total_sum2 / z - mean_total^2, 0))
  mean_pain <- pain_sum / z
  mean_physical <- physical_sum / z
  mean_mental <- mental_sum / z

  summary <- cbind(
    scenarios,
    mean_sumscore = mean_total,
    ci95_low = mean_total - 1.96 * sd_total / sqrt(reference_n),
    ci95_high = mean_total + 1.96 * sd_total / sqrt(reference_n),
    pain_sumscore = mean_pain,
    physical_sumscore = mean_physical,
    mental_sumscore = mean_mental,
    fatigue_sumscore = mean_physical + mean_mental
  )

  baseline <- summary[summary$direction == "baseline", ]
  rows <- summary[summary$direction != "baseline", ]
  rows$abbr <- node_info[rows$target, "abbr"]
  rows$label_en <- node_info[rows$target, "label_en"]
  rows$community <- node_info[rows$target, "community"]
  rows$threshold_original <- theta[rows$target]
  rows$threshold_shift <- rows$shift
  rows$threshold_perturbed <- rows$threshold_original + rows$shift
  rows$baseline_mean_sumscore <- baseline$mean_sumscore
  rows$baseline_pain_sumscore <- baseline$pain_sumscore
  rows$baseline_physical_sumscore <- baseline$physical_sumscore
  rows$baseline_mental_sumscore <- baseline$mental_sumscore
  rows$baseline_fatigue_sumscore <- baseline$fatigue_sumscore
  rows$intervention_mean_sumscore <- rows$mean_sumscore
  rows$intervention_ci95_low <- rows$ci95_low
  rows$intervention_ci95_high <- rows$ci95_high
  rows$intervention_pain_sumscore <- rows$pain_sumscore
  rows$intervention_physical_sumscore <- rows$physical_sumscore
  rows$intervention_mental_sumscore <- rows$mental_sumscore
  rows$intervention_fatigue_sumscore <- rows$fatigue_sumscore

  rows$nira_effect <- ifelse(
    rows$direction == "alleviating",
    baseline$mean_sumscore - rows$intervention_mean_sumscore,
    rows$intervention_mean_sumscore - baseline$mean_sumscore
  )
  rows$pain_effect <- ifelse(
    rows$direction == "alleviating",
    baseline$pain_sumscore - rows$intervention_pain_sumscore,
    rows$intervention_pain_sumscore - baseline$pain_sumscore
  )
  rows$physical_fatigue_effect <- ifelse(
    rows$direction == "alleviating",
    baseline$physical_sumscore - rows$intervention_physical_sumscore,
    rows$intervention_physical_sumscore - baseline$physical_sumscore
  )
  rows$mental_fatigue_effect <- ifelse(
    rows$direction == "alleviating",
    baseline$mental_sumscore - rows$intervention_mental_sumscore,
    rows$intervention_mental_sumscore - baseline$mental_sumscore
  )
  rows$total_fatigue_effect <- rows$physical_fatigue_effect + rows$mental_fatigue_effect

  keep <- c(
    "target", "abbr", "label_en", "community", "direction",
    "baseline_mean_sumscore", "intervention_mean_sumscore",
    "intervention_ci95_low", "intervention_ci95_high", "nira_effect",
    "threshold_original", "threshold_perturbed", "threshold_shift",
    "baseline_pain_sumscore", "baseline_physical_sumscore",
    "baseline_mental_sumscore", "baseline_fatigue_sumscore",
    "intervention_pain_sumscore", "intervention_physical_sumscore",
    "intervention_mental_sumscore", "intervention_fatigue_sumscore",
    "pain_effect", "physical_fatigue_effect", "mental_fatigue_effect",
    "total_fatigue_effect"
  )
  result <- rows[, keep]
  list(
    baseline = baseline,
    thresholds = data.frame(node = nodes, abbr = node_info[nodes, "abbr"], threshold = theta),
    aggravating = result[result$direction == "aggravating", ] |>
      dplyr::arrange(dplyr::desc(nira_effect)),
    alleviating = result[result$direction == "alleviating", ] |>
      dplyr::arrange(dplyr::desc(nira_effect))
  )
}

gibbs_sample_ising <- function(W, theta, n = 5000, burnin = 1000, thin = 5) {
  p <- length(theta)
  state <- rbinom(p, 1, 0.5)
  out <- matrix(0, nrow = n, ncol = p)
  colnames(out) <- names(theta)
  for (b in seq_len(burnin)) {
    for (i in sample.int(p)) {
      state[i] <- rbinom(1, 1, sigmoid(theta[i] + sum(W[i, ] * state)))
    }
  }
  for (r in seq_len(n)) {
    for (t in seq_len(thin)) {
      for (i in sample.int(p)) {
        state[i] <- rbinom(1, 1, sigmoid(theta[i] + sum(W[i, ] * state)))
      }
    }
    out[r, ] <- state
  }
  out
}

run_nira_mc <- function(dat, W, amount_sd = 2, n = 5000) {
  nodes <- colnames(W)
  theta <- solve_thresholds_fixed_slope(dat, W)
  theta_sd <- stats::sd(theta)
  comm <- node_info[nodes, "community"]
  baseline_samp <- gibbs_sample_ising(W, theta, n = n, burnin = 500, thin = 3)
  baseline_total <- rowSums(baseline_samp)
  baseline <- data.frame(
    mean_sumscore = mean(baseline_total),
    pain_sumscore = mean(rowSums(baseline_samp[, comm == "Pain", drop = FALSE])),
    physical_sumscore = mean(rowSums(baseline_samp[, comm == "Physical", drop = FALSE])),
    mental_sumscore = mean(rowSums(baseline_samp[, comm == "Mental", drop = FALSE]))
  )
  baseline$fatigue_sumscore <- baseline$physical_sumscore + baseline$mental_sumscore

  one <- function(direction, i) {
    shift <- ifelse(direction == "alleviating", -amount_sd * theta_sd, amount_sd * theta_sd)
    theta2 <- theta
    theta2[i] <- theta2[i] + shift
    samp <- gibbs_sample_ising(W, theta2, n = n, burnin = 500, thin = 3)
    total <- rowSums(samp)
    pain_m <- mean(rowSums(samp[, comm == "Pain", drop = FALSE]))
    phy_m <- mean(rowSums(samp[, comm == "Physical", drop = FALSE]))
    men_m <- mean(rowSums(samp[, comm == "Mental", drop = FALSE]))
    mean_total <- mean(total)

    data.frame(
      target = nodes[i],
      abbr = node_info[nodes[i], "abbr"],
      label_en = node_info[nodes[i], "label_en"],
      community = node_info[nodes[i], "community"],
      direction = direction,
      baseline_mean_sumscore = baseline$mean_sumscore,
      intervention_mean_sumscore = mean_total,
      intervention_ci95_low = mean_total - 1.96 * sd(total) / sqrt(n),
      intervention_ci95_high = mean_total + 1.96 * sd(total) / sqrt(n),
      nira_effect = ifelse(direction == "alleviating",
                           baseline$mean_sumscore - mean_total,
                           mean_total - baseline$mean_sumscore),
      threshold_original = theta[i],
      threshold_perturbed = theta2[i],
      threshold_shift = shift,
      baseline_pain_sumscore = baseline$pain_sumscore,
      baseline_physical_sumscore = baseline$physical_sumscore,
      baseline_mental_sumscore = baseline$mental_sumscore,
      baseline_fatigue_sumscore = baseline$fatigue_sumscore,
      intervention_pain_sumscore = pain_m,
      intervention_physical_sumscore = phy_m,
      intervention_mental_sumscore = men_m,
      intervention_fatigue_sumscore = phy_m + men_m,
      pain_effect = ifelse(direction == "alleviating", baseline$pain_sumscore - pain_m, pain_m - baseline$pain_sumscore),
      physical_fatigue_effect = ifelse(direction == "alleviating", baseline$physical_sumscore - phy_m, phy_m - baseline$physical_sumscore),
      mental_fatigue_effect = ifelse(direction == "alleviating", baseline$mental_sumscore - men_m, men_m - baseline$mental_sumscore),
      total_fatigue_effect = ifelse(direction == "alleviating", baseline$fatigue_sumscore - (phy_m + men_m), (phy_m + men_m) - baseline$fatigue_sumscore),
      stringsAsFactors = FALSE
    )
  }

  rows <- dplyr::bind_rows(
    lapply(seq_along(nodes), function(i) one("aggravating", i)),
    lapply(seq_along(nodes), function(i) one("alleviating", i))
  )
  list(
    baseline = baseline,
    thresholds = data.frame(node = nodes, abbr = node_info[nodes, "abbr"], threshold = theta),
    aggravating = rows[rows$direction == "aggravating", ] |>
      dplyr::arrange(dplyr::desc(nira_effect)),
    alleviating = rows[rows$direction == "alleviating", ] |>
      dplyr::arrange(dplyr::desc(nira_effect))
  )
}

# ---------------------- 7. 图形 ------------------------------

manual_layout <- data.frame(
  node = node_names,
  x = c(0.15, 0.10, 0.12, 0.18, 0.27, 0.34, 0.40, 0.43, 0.36,
        0.68, 0.76, 0.84, 0.64, 0.74, 0.86, 0.92, 0.90,
        0.68, 0.58, 0.54, 0.67, 0.82, 0.92),
  y = c(0.78, 0.63, 0.47, 0.30, 0.16, 0.18, 0.30, 0.47, 0.63,
        0.37, 0.27, 0.16, 0.13, 0.07, 0.09, 0.20, 0.32,
        0.84, 0.74, 0.60, 0.58, 0.64, 0.80),
  stringsAsFactors = FALSE
)

plot_network <- function(W, prefix) {
  edges <- edge_table(W)
  pos <- merge(manual_layout, node_info, by = "node", all.x = TRUE)
  edge_df <- edges |>
    dplyr::left_join(pos[, c("node", "x", "y")], by = c("source" = "node")) |>
    dplyr::rename(x1 = x, y1 = y) |>
    dplyr::left_join(pos[, c("node", "x", "y")], by = c("target" = "node")) |>
    dplyr::rename(x2 = x, y2 = y)

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_df,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2,
                   linewidth = abs_weight, alpha = abs_weight,
                   colour = weight > 0),
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = pos,
      ggplot2::aes(x = x, y = y, fill = community),
      shape = 21, size = 10, colour = "white", stroke = 1.1
    ) +
    ggplot2::geom_text(
      data = pos,
      ggplot2::aes(x = x, y = y, label = abbr),
      size = 3.5, fontface = "bold", colour = "white"
    ) +
    ggplot2::scale_fill_manual(values = c(Pain = "#E76F51", Physical = "#2A9D8F", Mental = "#457B9D")) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#2563EB", `FALSE` = "#DC2626"),
                                 labels = c("Negative", "Positive")) +
    ggplot2::scale_linewidth(range = c(0.15, 2.2), guide = "none") +
    ggplot2::scale_alpha(range = c(0.15, 0.80), guide = "none") +
    ggplot2::coord_equal(xlim = c(0.02, 0.98), ylim = c(0.02, 0.95), expand = FALSE) +
    ggplot2::labs(title = paste0(prefix, ": symptom network"),
                  fill = "Community", colour = "Edge sign") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                   legend.position = "bottom")

  ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_network.png")),
                  p, width = 9.5, height = 6.5, dpi = 300)
}

plot_centrality_and_bridge <- function(centrality, prefix) {
  top_strength <- centrality |>
    dplyr::arrange(dplyr::desc(strength)) |>
    dplyr::slice_head(n = 12)
  p_strength <- ggplot2::ggplot(
    top_strength,
    ggplot2::aes(x = stats::reorder(abbr, strength), y = strength, fill = community)
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(Pain = "#E76F51", Physical = "#2A9D8F", Mental = "#457B9D")) +
    ggplot2::labs(x = NULL, y = "Strength", title = "Top strength centrality nodes") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_centrality_strength.png")),
                  p_strength, width = 7.2, height = 5.2, dpi = 300)

  pain_clust <- centrality |>
    dplyr::filter(community == "Pain") |>
    dplyr::arrange(dplyr::desc(clustering))
  p_clust <- ggplot2::ggplot(
    pain_clust,
    ggplot2::aes(x = stats::reorder(abbr, clustering), y = clustering)
  ) +
    ggplot2::geom_col(width = 0.72, fill = "#E76F51") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Local clustering", title = "Pain-node local clustering") +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_pain_clustering.png")),
                  p_clust, width = 7.2, height = 4.4, dpi = 300)

  bridge_strength <- centrality |>
    dplyr::arrange(dplyr::desc(bridge_strength))
  p_bridge_strength <- ggplot2::ggplot(
    bridge_strength,
    ggplot2::aes(x = stats::reorder(abbr, bridge_strength), y = bridge_strength, fill = community)
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(Pain = "#E76F51", Physical = "#2A9D8F", Mental = "#457B9D")) +
    ggplot2::labs(x = NULL, y = "Bridge strength", title = "Bridge centrality: strength") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_bridge_strength.png")),
                  p_bridge_strength, width = 7.2, height = 5.2, dpi = 300)

  bridge_ei <- centrality |>
    dplyr::mutate(abs_bridge_ei = abs(bridge_expected_influence)) |>
    dplyr::arrange(dplyr::desc(abs_bridge_ei))
  p_bridge_ei <- ggplot2::ggplot(
    bridge_ei,
    ggplot2::aes(x = stats::reorder(abbr, bridge_expected_influence),
                 y = bridge_expected_influence, fill = community)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(Pain = "#E76F51", Physical = "#2A9D8F", Mental = "#457B9D")) +
    ggplot2::labs(x = NULL, y = "Bridge expected influence",
                  title = "Bridge centrality: expected influence") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_bridge_expected_influence.png")),
                  p_bridge_ei, width = 7.2, height = 5.2, dpi = 300)
}

plot_bootstrap_outputs <- function(boot, prefix) {
  edge_plot_data <- boot$edge_ci |>
    dplyr::filter(original_abs_weight > 0) |>
    dplyr::arrange(dplyr::desc(original_abs_weight)) |>
    dplyr::slice_head(n = 30)
  if (nrow(edge_plot_data) > 0) {
    p_edge <- ggplot2::ggplot(
      edge_plot_data,
      ggplot2::aes(x = stats::reorder(pair, original_abs_weight), y = original_weight)
    ) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = boot_lcl, ymax = boot_ucl),
                             width = 0.18, colour = "grey35") +
      ggplot2::geom_point(size = 2.1, colour = "#2563EB") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Edge weight with 95% bootstrap CI",
                    title = "Bootstrap accuracy: strongest edges") +
      ggplot2::theme_minimal(base_size = 12)
    ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_bootstrap_edge_accuracy.png")),
                    p_edge, width = 8.4, height = 7.2, dpi = 300)
  }

  strength_plot_data <- boot$centrality_ci |>
    dplyr::filter(metric == "strength")
  p_cent <- ggplot2::ggplot(
    strength_plot_data,
    ggplot2::aes(x = stats::reorder(abbr, boot_mean), y = boot_mean, fill = community)
  ) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = boot_lcl, ymax = boot_ucl),
                           width = 0.18, colour = "grey35") +
    ggplot2::geom_col(width = 0.72, alpha = 0.82) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(Pain = "#E76F51", Physical = "#2A9D8F", Mental = "#457B9D")) +
    ggplot2::labs(x = NULL, y = "Strength with 95% bootstrap CI",
                  title = "Bootstrap accuracy: strength centrality") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_bootstrap_strength_accuracy.png")),
                  p_cent, width = 7.2, height = 6.4, dpi = 300)
}

plot_case_drop_stability <- function(stability, prefix) {
  metric_labels <- c(
    strength = "Strength",
    expected_influence = "Expected influence",
    bridge_strength = "Bridge strength",
    bridge_expected_influence = "Bridge expected influence"
  )
  plot_df <- stability$summary
  plot_df$metric_label <- metric_labels[plot_df$metric]

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = drop_proportion, y = mean_correlation,
                 ymin = q05_correlation, ymax = q95_correlation,
                 colour = metric_label, fill = metric_label)
  ) +
    ggplot2::geom_hline(yintercept = CS_COR_THRESHOLD, linetype = "dashed",
                        colour = "grey35", linewidth = 0.35) +
    ggplot2::geom_ribbon(alpha = 0.12, colour = NA) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                                breaks = CASE_DROP_PROPORTIONS) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(x = "Proportion of cases dropped",
                  y = "Correlation with original centrality",
                  colour = NULL, fill = NULL,
                  title = "Case-dropping bootstrap stability") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_case_drop_stability.png")),
                  p, width = 8.2, height = 5.4, dpi = 300)
}

plot_nira <- function(nira, prefix, n_top = 12) {
  make_panel <- function(dat, label) {
    dat <- dat |>
      dplyr::arrange(dplyr::desc(nira_effect)) |>
      dplyr::slice_head(n = n_top)
    rbind(
      data.frame(panel = label, order = 0, x_lab = "original",
                 mean = dat$baseline_mean_sumscore[1],
                 low = dat$baseline_mean_sumscore[1],
                 high = dat$baseline_mean_sumscore[1]),
      data.frame(panel = label, order = seq_len(nrow(dat)), x_lab = dat$abbr,
                 mean = dat$intervention_mean_sumscore,
                 low = dat$intervention_ci95_low,
                 high = dat$intervention_ci95_high)
    )
  }

  plot_df <- rbind(
    make_panel(nira$aggravating, "A. Aggravating"),
    make_panel(nira$alleviating, "B. Alleviating")
  )
  plot_df$x_id <- factor(paste(plot_df$panel, plot_df$order, sep = "_"),
                         levels = unique(paste(plot_df$panel, plot_df$order, sep = "_")))
  label_map <- setNames(plot_df$x_lab, plot_df$x_id)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x_id, y = mean, group = 1)) +
    ggplot2::geom_line(linetype = "dashed", colour = "grey25", linewidth = 0.45) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = high), width = 0.18, colour = "grey30") +
    ggplot2::geom_point(ggplot2::aes(colour = panel), size = 2.6) +
    ggplot2::facet_wrap(~panel, scales = "free_x", nrow = 1) +
    ggplot2::scale_x_discrete(labels = label_map) +
    ggplot2::scale_colour_manual(values = c("A. Aggravating" = "#B23A2E",
                                            "B. Alleviating" = "#2B83BA")) +
    ggplot2::labs(x = "Symptom of which the threshold is altered",
                  y = "Expected sum score") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none",
                   strip.text = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  ggplot2::ggsave(file.path(OUT_DIR, paste0(prefix, "_figure_nira_interventions.png")),
                  p, width = 11, height = 4.8, dpi = 300)
}

# ---------------------- 8. 主分析函数 ------------------------

analyze_one <- function(dat, prefix) {
  message("\n==============================")
  message("开始分析：", prefix)
  message("==============================")

  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  data.table::fwrite(dat, file.path(OUT_DIR, paste0(prefix, "_selected_nodes.csv")))

  prevalence <- data.frame(
    node = names(dat),
    abbr = node_info[names(dat), "abbr"],
    label_en = node_info[names(dat), "label_en"],
    label_cn = node_info[names(dat), "label_cn"],
    community = node_info[names(dat), "community"],
    n = colSums(dat == 1),
    denominator = nrow(dat),
    prevalence = colMeans(dat),
    stringsAsFactors = FALSE
  )
  data.table::fwrite(prevalence, file.path(OUT_DIR, paste0(prefix, "_node_prevalence.csv")))

  message("估计 Ising-type 网络...")
  fit <- estimate_ising_glmnet(dat, gamma = 0.25, rule = "AND")
  W <- fit$W
  data.table::fwrite(as.data.frame(W, check.names = FALSE),
                     file.path(OUT_DIR, paste0(prefix, "_adjacency_matrix.csv")))

  edges <- edge_table(W)
  data.table::fwrite(edges, file.path(OUT_DIR, paste0(prefix, "_edge_weights.csv")))
  data.table::fwrite(head(edges, 30), file.path(OUT_DIR, paste0(prefix, "_top_edges.csv")))

  centrality <- centrality_summary(W)
  pred <- node_predictability(dat, W)
  centrality <- dplyr::left_join(centrality, pred, by = "node")
  data.table::fwrite(centrality, file.path(OUT_DIR, paste0(prefix, "_centrality_predictability.csv")))
  plot_centrality_and_bridge(centrality, prefix)

  bridge_edges <- edges |>
    dplyr::filter(source_community != target_community) |>
    dplyr::arrange(dplyr::desc(abs_weight))
  data.table::fwrite(bridge_edges, file.path(OUT_DIR, paste0(prefix, "_bridge_edges.csv")))

  network_summary <- data.frame(
    analysis = prefix,
    n_complete = nrow(dat),
    nodes = ncol(dat),
    edges = nrow(edges),
    density = nrow(edges) / (ncol(dat) * (ncol(dat) - 1) / 2),
    strongest_node = centrality$node[1],
    strongest_node_abbr = centrality$abbr[1],
    strongest_node_strength = centrality$strength[1],
    stringsAsFactors = FALSE
  )
  data.table::fwrite(network_summary, file.path(OUT_DIR, paste0(prefix, "_network_summary.csv")))

  plot_network(W, prefix)

  boot <- NULL
  stability <- NULL
  if (RUN_BOOTSTRAP) {
    message("Running nonparametric bootstrap for edge and centrality accuracy...")
    boot <- bootstrap_network(dat, W, B = BOOT_N, gamma = 0.25, rule = "AND", seed = BOOT_SEED)
    data.table::fwrite(boot$edge_ci, file.path(OUT_DIR, paste0(prefix, "_bootstrap_edge_ci.csv")))
    data.table::fwrite(boot$centrality_ci, file.path(OUT_DIR, paste0(prefix, "_bootstrap_centrality_ci.csv")))
    plot_bootstrap_outputs(boot, prefix)
  }

  if (RUN_CASE_STABILITY) {
    message("Running case-dropping bootstrap for centrality stability and CS coefficients...")
    stability <- case_drop_stability(dat, W, B = BOOT_CASE_N, gamma = 0.25,
                                     rule = "AND", seed = BOOT_SEED)
    data.table::fwrite(stability$raw, file.path(OUT_DIR, paste0(prefix, "_case_drop_stability_raw.csv")))
    data.table::fwrite(stability$summary, file.path(OUT_DIR, paste0(prefix, "_case_drop_stability_summary.csv")))
    data.table::fwrite(stability$cs, file.path(OUT_DIR, paste0(prefix, "_cs_coefficients.csv")))
    plot_case_drop_stability(stability, prefix)
  }

  message("运行 NIRA 模拟干预...")
  if (NIRA_MODE == "exact") {
    nira <- run_nira_exact(dat, W, amount_sd = 2, reference_n = 5000, chunk_size = 150000)
  } else {
    nira <- run_nira_mc(dat, W, amount_sd = 2, n = 5000)
  }

  data.table::fwrite(nira$thresholds, file.path(OUT_DIR, paste0(prefix, "_nira_thresholds.csv")))
  data.table::fwrite(nira$aggravating, file.path(OUT_DIR, paste0(prefix, "_nira_aggravating.csv")))
  data.table::fwrite(nira$alleviating, file.path(OUT_DIR, paste0(prefix, "_nira_alleviating.csv")))
  plot_nira(nira, prefix)

  pain_targets <- nira$alleviating |>
    dplyr::filter(community == "Pain") |>
    dplyr::arrange(dplyr::desc(total_fatigue_effect))
  data.table::fwrite(pain_targets, file.path(OUT_DIR, paste0(prefix, "_pain_targets_by_fatigue_reduction.csv")))

  list(
    dat = dat,
    W = W,
    edges = edges,
    centrality = centrality,
    boot = boot,
    stability = stability,
    nira = nira,
    summary = network_summary
  )
}

# ---------------------- 9. 开始运行 --------------------------

primary_12m <- analyze_one(dat12, "primary_12m")
sensitivity_7d <- analyze_one(dat7, "sensitivity_7d")

combined_summary <- dplyr::bind_rows(primary_12m$summary, sensitivity_7d$summary)
data.table::fwrite(combined_summary, file.path(OUT_DIR, "combined_network_summary.csv"))

if (!is.null(primary_12m$stability) || !is.null(sensitivity_7d$stability)) {
  cs_all <- dplyr::bind_rows(
    if (!is.null(primary_12m$stability)) {
      cbind(analysis = "primary_12m", primary_12m$stability$cs)
    },
    if (!is.null(sensitivity_7d$stability)) {
      cbind(analysis = "sensitivity_7d", sensitivity_7d$stability$cs)
    }
  )
  data.table::fwrite(cs_all, file.path(OUT_DIR, "combined_cs_coefficients.csv"))
}

message("\n全部完成。输出目录：")
message(OUT_DIR)

message("\n12个月网络：NIRA 缓解靶点前10名")
print(primary_12m$nira$alleviating |>
        dplyr::select(abbr, label_en, community, nira_effect, total_fatigue_effect) |>
        head(10))

message("\n12个月网络：疼痛部位按 projected fatigue reduction 排序")
print(primary_12m$nira$alleviating |>
        dplyr::filter(community == "Pain") |>
        dplyr::arrange(dplyr::desc(total_fatigue_effect)) |>
        dplyr::select(abbr, label_en, nira_effect,
                      physical_fatigue_effect, mental_fatigue_effect,
                      total_fatigue_effect))
