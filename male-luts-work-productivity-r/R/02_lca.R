source(file.path(PIPELINE_ROOT, "R", "00_utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages({library(data.table); library(poLCA)})

dt <- as.data.table(readRDS(out_path("01_data", "analysis_module_completers_deidentified.rds")))
items <- CFG$variables$luts
labels <- CFG$luts_labels_zh

entropy_norm <- function(post) {
  k <- ncol(post)
  if (k <= 1L) return(1)
  1 - sum(-rowSums(post * log(pmax(post, 1e-15)))) / (nrow(post) * log(k))
}
best_repeat <- function(m, tol = 1e-4) {
  # The one-class solution is unique by construction; random starts are still
  # executed for a uniform audit trail, but it should not trigger costly retries.
  if (ncol(m$posterior) == 1L) return(CFG$lca_best_ll_min_repetitions)
  attempts <- m$attempts
  if (!length(attempts) || !any(is.finite(attempts))) return(0L)
  sum(abs(attempts - max(attempts, na.rm = TRUE)) <= tol, na.rm = TRUE)
}

fit_lca_grid <- function(data_x, tag = "ordinal", response_levels = 5L) {
  cache_model_path <- out_path("_work", paste0(tag, "_lca_models_1_to_8.rds"))
  cache_signature_path <- out_path("_work", paste0(tag, "_lca_cache_signature.rds"))
  current_signature <- make_lca_cache_signature(tag)
  if (file.exists(cache_model_path) && file.exists(cache_signature_path)) {
    cached_signature <- tryCatch(readRDS(cache_signature_path), error = function(e) NULL)
    cached_models <- tryCatch(readRDS(cache_model_path), error = function(e) NULL)
    starts_ok <- is.list(cached_models) && length(cached_models) == 8L &&
      all(vapply(seq_along(cached_models), function(k) {
        k == 1L || length(cached_models[[k]]$attempts) >= CFG$lca_random_starts[["initial"]]
      }, logical(1)))
    if (identical(cached_signature, current_signature) && starts_ok) {
      log_msg("INFO", "Using validated ", tag, " LCA cache; input hash, seed, settings and script hash match.")
      return(cached_models)
    }
  }
  f <- as.formula(paste0("cbind(", paste(items, collapse = ","), ") ~ 1"))
  fit_one <- function(k, nrep, seed_suffix = 0L) {
    set.seed(CFG$random_seed + ifelse(tag == "binary", 500000L, 0L) +
               k * 10000L + nrep * 10L + seed_suffix)
    poLCA::poLCA(f, data = data_x, nclass = k, nrep = nrep,
                 maxiter = CFG$max_iterations, tol = 1e-8,
                 verbose = FALSE, graphs = FALSE, calc.se = FALSE)
  }
  # PSOCK workers start fresh R sessions.  On managed/older servers the project
  # renv autoloader can replace the library paths inherited from the parent
  # session, so packages that are available to this process (notably poLCA)
  # become invisible inside the workers.  Pass the verified parent paths to
  # every worker before loading poLCA.
  parent_libpaths <- .libPaths()
  cl <- parallel::makeCluster(min(4L, max(1L, parallel::detectCores(logical = FALSE) - 1L)))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(
    cl,
    c("data_x", "items", "CFG", "tag", "fit_one", "f", "parent_libpaths"),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, {
    .libPaths(unique(c(parent_libpaths, .libPaths())))
    suppressPackageStartupMessages(library(poLCA))
    invisible(.libPaths())
  })

  run_start_grid <- function(k_values, total_starts, chunks) {
    chunks <- max(1L, min(as.integer(chunks), as.integer(total_starts)))
    base_n <- total_starts %/% chunks
    extras <- total_starts %% chunks
    chunk_sizes <- rep.int(base_n, chunks)
    if (extras > 0L) chunk_sizes[seq_len(extras)] <- chunk_sizes[seq_len(extras)] + 1L
    tasks <- do.call(rbind, lapply(k_values, function(k) data.frame(
      k = k, chunk = seq_len(chunks), nrep = chunk_sizes
    )))
    parallel::clusterExport(cl, c("tasks", "fit_one", "total_starts"), envir = environment())
    fitted <- parallel::parLapplyLB(cl, seq_len(nrow(tasks)), function(i) {
      fit_one(tasks$k[i], tasks$nrep[i], seed_suffix = total_starts * 100L + tasks$chunk[i])
    })
    combined <- vector("list", length(k_values))
    names(combined) <- paste0("k", k_values)
    for (i in seq_along(k_values)) {
      k <- k_values[i]
      parts <- fitted[tasks$k == k]
      best <- parts[[which.max(vapply(parts, function(x) x$llik, numeric(1)))]]
      best$attempts <- unlist(lapply(parts, function(x) x$attempts), use.names = FALSE)
      combined[[i]] <- best
    }
    combined
  }

  initial_starts <- CFG$lca_random_starts[["initial"]]
  models <- run_start_grid(1:8, initial_starts, chunks = 2L)
  for (target_starts in CFG$lca_random_starts[c("retry", "final")]) {
    unstable <- which(vapply(models, best_repeat, integer(1)) < CFG$lca_best_ll_min_repetitions)
    if (!length(unstable)) break
    log_msg("WARNING", tag, " LCA models with unstable optimum will be refitted at ", target_starts,
            " starts: K=", paste(unstable, collapse = ","))
    completed_starts <- vapply(unstable, function(k) length(models[[k]]$attempts), integer(1))
    additional_starts <- target_starts - min(completed_starts)
    if (additional_starts <= 0L) next
    refit <- run_start_grid(unstable, additional_starts, chunks = 4L)
    for (j in seq_along(unstable)) {
      k <- unstable[j]
      old <- models[[k]]
      new <- refit[[j]]
      best <- if (old$llik >= new$llik) old else new
      best$attempts <- c(old$attempts, new$attempts)
      models[[k]] <- best
    }
  }
  parallel::stopCluster(cl); on.exit(NULL, add = FALSE)
  names(models) <- paste0("k", 1:8)
  saveRDS(models, cache_model_path, compress = "xz")
  saveRDS(current_signature, cache_signature_path)
  models
}

make_fit_table <- function(models) {
  rbindlist(lapply(seq_along(models), function(k) {
    m <- models[[k]]
    modal <- m$predclass
    avepp <- vapply(seq_len(k), function(j) {
      if (!sum(modal == j)) NA_real_ else mean(m$posterior[modal == j, j])
    }, numeric(1))
    p_all <- unlist(m$probs, use.names = FALSE)
    attempts <- m$attempts
    data.table(
      classes = k, log_likelihood = m$llik, parameters = m$npar, G2 = m$Gsq, df = m$resid.df,
      AIC = m$aic, BIC = m$bic, aBIC = -2 * m$llik + m$npar * log((m$Nobs + 2) / 24),
      entropy = entropy_norm(m$posterior), min_estimated_class_prop = min(m$P),
      min_estimated_class_n = min(m$P) * m$Nobs, min_modal_class_n = min(tabulate(modal, nbins = k)),
      min_AvePP = min(avepp, na.rm = TRUE), mean_AvePP = mean(avepp, na.rm = TRUE),
      AvePP_by_class = paste(sprintf("%.3f", avepp), collapse = ";"),
      class_proportions = paste(sprintf("%.4f", m$P), collapse = ";"),
      random_starts = if (k == 1L) CFG$lca_random_starts[["initial"]] else length(attempts),
      best_ll_repetitions = best_repeat(m),
      converged_start_fraction = mean(is.finite(attempts)), final_iterations = m$numiter,
      converged_final = m$numiter < CFG$max_iterations,
      boundary_probability_count = sum(p_all <= 1e-6 | p_all >= 1 - 1e-6),
      empty_modal_class = any(tabulate(modal, nbins = k) == 0),
      small_class_warning = min(m$P) < CFG$class_warn_fraction,
      small_class_high_risk = min(m$P) < CFG$class_high_risk_fraction || min(m$P) * m$Nobs < CFG$minimum_class_n_high_risk
    )
  }))
}

local_dependence_all <- function(models, observed_x, response_levels) {
  rows <- vector("list", length(models) * choose(length(items), 2))
  idx <- 0L
  for (k in seq_along(models)) {
    m <- models[[k]]
    for (j in seq_len(length(items) - 1L)) for (h in (j + 1L):length(items)) {
      obs <- table(factor(observed_x[[j]], levels = seq_len(response_levels)),
                   factor(observed_x[[h]], levels = seq_len(response_levels)))
      expected_prob <- matrix(0, response_levels, response_levels)
      for (cl in seq_len(k)) expected_prob <- expected_prob + m$P[cl] * outer(m$probs[[j]][cl, ], m$probs[[h]][cl, ])
      exp_n <- m$Nobs * expected_prob
      keep <- exp_n > 1e-10
      bvr <- sum((obs[keep] - exp_n[keep])^2 / exp_n[keep])
      df_pair <- (response_levels - 1)^2
      idx <- idx + 1L
      rows[[idx]] <- data.table(classes = k, item1 = items[j], symptom1 = unname(labels[items[j]]),
        item2 = items[h], symptom2 = unname(labels[items[h]]), BVR = bvr, df = df_pair,
        p_value = pchisq(bvr, df_pair, lower.tail = FALSE))
    }
  }
  ans <- rbindlist(rows[seq_len(idx)])
  ans[, p_BH := p.adjust(p_value, method = "BH"), by = classes]
  ans[, `:=`(BH_significant = p_BH < CFG$local_dependence_bh_alpha,
             material_BVR = BVR >= CFG$local_dependence_bvr_material)]
  ans
}

ordinal_x <- as.data.frame(dt[, ..items])
ordinal_x[] <- lapply(ordinal_x, function(z) as.integer(z) + 1L)
log_msg("INFO", "Fitting ordinal 1-8 class LCA models with adaptive random starts.")
models <- fit_lca_grid(ordinal_x, "ordinal", 5L)
fit <- make_fit_table(models)
local_dep <- local_dependence_all(models, ordinal_x, 5L)
ld_summary <- local_dep[, .(
  BVR_min = min(BVR), BVR_q25 = quantile(BVR, .25), BVR_median = median(BVR),
  BVR_q75 = quantile(BVR, .75), BVR_max = max(BVR), BVR_mean = mean(BVR),
  BH_significant_pairs = sum(BH_significant), BH_significant_fraction = mean(BH_significant),
  material_BVR_pairs = sum(material_BVR), top_pair = paste(symptom1[which.max(BVR)], symptom2[which.max(BVR)], sep = " × ")
), by = classes]
fit <- merge(fit, ld_summary, by = "classes", all.x = TRUE, sort = TRUE)

fit[, strict_eligible := classes >= 2 & entropy >= .80 & min_AvePP >= .80 &
      min_estimated_class_prop >= CFG$class_high_risk_fraction & best_ll_repetitions >= CFG$lca_best_ll_min_repetitions &
      converged_final & !empty_modal_class]
fit[, relaxed_eligible := classes >= 2 & min_AvePP >= .70 &
      min_estimated_class_prop >= CFG$class_high_risk_fraction & best_ll_repetitions >= CFG$lca_best_ll_min_repetitions &
      converged_final & !empty_modal_class]
fit[, selection_score := rank(BIC, ties.method = "min") + rank(aBIC, ties.method = "min") +
      2 * small_class_warning + 3 * small_class_high_risk +
      1 * (boundary_probability_count > 0) + 2 * (BH_significant_fraction >= CFG$local_dependence_broad_fraction)]
eligible <- fit[strict_eligible == TRUE]
selection_rule <- "strict_multicriterion"
if (!nrow(eligible)) { eligible <- fit[relaxed_eligible == TRUE]; selection_rule <- "relaxed_multicriterion" }
if (!nrow(eligible)) { eligible <- fit[classes >= 2 & min_estimated_class_prop >= CFG$class_high_risk_fraction]; selection_rule <- "minimum_size_fallback" }
if (!nrow(eligible)) { eligible <- fit[classes >= 2]; selection_rule <- "statistical_fallback" }
selected_k <- eligible[which.min(selection_score), classes]
fit[, selected := classes == selected_k]

selected_model <- models[[selected_k]]
modal <- selected_model$predclass
expected <- matrix(NA_real_, selected_k, length(items), dimnames = list(seq_len(selected_k), items))
profile_rows <- list(); idx <- 0L
for (j in seq_along(items)) {
  p <- selected_model$probs[[j]]
  expected[, j] <- p %*% (0:4)
  for (cl in seq_len(selected_k)) for (r in 0:4) {
    idx <- idx + 1L
    profile_rows[[idx]] <- data.table(class_original = cl, item = items[j], symptom = unname(labels[items[j]]),
                                      response = r, conditional_probability = p[cl, r + 1L])
  }
}
burden <- rowSums(expected)
order_class <- order(burden)
class_rank <- match(seq_len(selected_k), order_class)

dimension_mean <- function(cl, dim_items) mean(expected[cl, dim_items, drop = TRUE])
class_names <- character(selected_k)
for (cl in seq_len(selected_k)) {
  r <- class_rank[cl]
  dvals <- vapply(CFG$luts_dimensions, function(v) dimension_mean(cl, v), numeric(1))
  if (r == 1L) class_names[cl] <- "低症状负担型"
  else if (r == selected_k) class_names[cl] <- "广泛高症状负担型"
  else {
    ord <- order(dvals, decreasing = TRUE)
    dim_zh <- c(storage = "储尿症状", voiding = "排尿症状", post_micturition = "排尿后症状")
    if (dvals[ord[1]] - dvals[ord[2]] >= .30) class_names[cl] <- paste0(dim_zh[names(dvals)[ord[1]]], "突出型")
    else class_names[cl] <- paste0("混合症状负担", r, "级")
  }
}
class_levels <- class_names[order_class]

expected_long <- as.data.table(as.table(expected))
setnames(expected_long, c("class_original", "item", "expected_score"))
expected_long[, class_original := as.integer(class_original)]
expected_long[, `:=`(symptom = unname(labels[item]), class_rank = class_rank[class_original],
                     class_label = class_names[class_original])]
expected_long[, dimension := fifelse(item %in% CFG$luts_dimensions$storage, "储尿症状",
                              fifelse(item %in% CFG$luts_dimensions$voiding, "排尿症状", "排尿后症状"))]
expected_long[, item_order := match(item, items)]
setorder(expected_long, class_rank, item_order)
expected_long[, item_order := NULL]

profiles <- rbindlist(profile_rows)
profiles[, `:=`(class_rank = class_rank[class_original], class_label = class_names[class_original])]
profiles[, positive_probability := sum(conditional_probability[response > 0]), by = .(class_original, item)]
profiles[, item_order := match(item, items)]
setorder(profiles, class_rank, item_order, response)
profiles[, item_order := NULL]

assignments <- data.table(
  class_original = seq_len(selected_k), class_rank = class_rank, class_label = class_names,
  estimated_proportion = selected_model$P, estimated_n = selected_model$P * selected_model$Nobs,
  modal_n = tabulate(modal, nbins = selected_k), modal_proportion = tabulate(modal, nbins = selected_k) / selected_model$Nobs,
  AvePP = vapply(seq_len(selected_k), function(cl) mean(selected_model$posterior[modal == cl, cl]), numeric(1)),
  expected_total_score = burden
)
setorder(assignments, class_rank)

post <- data.table(analysis_key = dt$analysis_key, modal_class_original = modal,
                   class_rank = class_rank[modal], class_label = class_names[modal])
for (r in seq_len(selected_k)) {
  original <- order_class[r]
  post[[paste0("posterior_rank_", r)]] <- selected_model$posterior[, original]
}

selected_ld <- local_dep[classes == selected_k]
model_not_ready <- selected_ld[, mean(BH_significant) >= CFG$local_dependence_broad_fraction ||
                                max(BVR) >= CFG$local_dependence_bvr_material]
model_status <- if (model_not_ready) "MODEL_NOT_READY_FOR_FINAL_INFERENCE" else "MODEL_READY_PENDING_PRODUCTION_GATES"

write_csv_utf8(fit, out_path("outputs", "LCA_model_selection_full.csv"))
write_csv_utf8(local_dep, out_path("outputs", "LCA_local_dependence_full.csv"))
file.copy(out_path("outputs", "LCA_model_selection_full.csv"), out_path("03_lca", "model_fit_1_to_8.csv"), overwrite = TRUE)
file.copy(out_path("outputs", "LCA_local_dependence_full.csv"), out_path("03_lca", "local_dependence_full.csv"), overwrite = TRUE)
write_csv_utf8(expected_long, out_path("03_lca", "class_expected_scores.csv"))
write_csv_utf8(profiles, out_path("03_lca", "class_profiles_full.csv"))
write_csv_utf8(assignments, out_path("03_lca", "class_assignment_summary.csv"))
write_csv_utf8(post, out_path("03_lca", "posterior_probabilities.csv"))
write_lines_utf8(model_status, out_path("03_lca", "MODEL_STATUS.txt"))
saveRDS(models, out_path("_work", "ordinal_lca_models_1_to_8.rds"), compress = "xz")
saveRDS(list(model = selected_model, selected_k = selected_k, class_rank = class_rank,
             class_names = class_names, class_levels = class_levels, order_class = order_class,
             selection_rule = selection_rule, model_status = model_status),
        out_path("03_lca", "final_model_parameters.rds"), compress = "xz")

top_ld <- selected_ld[order(-BVR)][1:min(10, .N)]
class_report <- c(
  "# LCA模型选择报告", "",
  if (CFG$run_mode == "test") "> **测试数据结果，禁止投稿。正式数据重跑后全部数值将自动更新。**" else "",
  paste0("- 选择规则：", selection_rule, "；最终类别数由代码选择为", selected_k, "类，未固定为4类。"),
  paste0("- 模型状态：`", model_status, "`。"),
  paste0("- 最小类别比例：", sprintf("%.1f%%", 100 * fit[selected == TRUE, min_estimated_class_prop]),
         "；最小AvePP：", sprintf("%.3f", fit[selected == TRUE, min_AvePP]),
         "；熵：", sprintf("%.3f", fit[selected == TRUE, entropy]), "。"),
  paste0("- 经BH校正的异常局部依赖条目对：", fit[selected == TRUE, BH_significant_pairs], "/", nrow(selected_ld),
         "；最大BVR=", sprintf("%.2f", max(selected_ld$BVR)), "。"),
  "- 类别名称由条件期望得分和储尿/排尿/排尿后维度模式自动生成，需在正式数据结果出来后由临床专家复核。", "",
  "## 最严重的局部依赖条目对", "",
  "|条目对|BVR|BH校正P|", "|---|---:|---:|",
  vapply(seq_len(nrow(top_ld)), function(i) sprintf("|%s × %s|%.2f|%s|",
    top_ld$symptom1[i], top_ld$symptom2[i], top_ld$BVR[i], fmt_p(top_ld$p_BH[i])), character(1))
)
write_lines_utf8(class_report, out_path("03_lca", "class_selection_report.md"))

# Binary-coded LCA sensitivity, using the same adaptive start policy.
binary_x <- as.data.frame(dt[, ..items])
binary_x[] <- lapply(binary_x, function(z) as.integer(z > 0) + 1L)
log_msg("INFO", "Fitting binary 1-8 class LCA sensitivity models with adaptive random starts.")
binary_models <- fit_lca_grid(binary_x, "binary", 2L)
binary_fit <- make_fit_table(binary_models)
binary_fit[, strict_eligible := classes >= 2 & entropy >= .80 & min_AvePP >= .80 &
      min_estimated_class_prop >= CFG$class_high_risk_fraction & best_ll_repetitions >= CFG$lca_best_ll_min_repetitions & converged_final]
binary_fit[, relaxed_eligible := classes >= 2 & min_AvePP >= .70 &
      min_estimated_class_prop >= CFG$class_high_risk_fraction & best_ll_repetitions >= CFG$lca_best_ll_min_repetitions & converged_final]
binary_fit[, selection_score := rank(BIC) + rank(aBIC) + 2 * small_class_warning + 3 * small_class_high_risk +
             (boundary_probability_count > 0)]
be <- binary_fit[strict_eligible == TRUE]
if (!nrow(be)) be <- binary_fit[relaxed_eligible == TRUE]
if (!nrow(be)) be <- binary_fit[classes >= 2]
binary_k <- be[which.min(selection_score), classes]
binary_fit[, selected := classes == binary_k]
binary_model <- binary_models[[binary_k]]
binary_burden <- vapply(seq_len(binary_k), function(cl) sum(vapply(binary_model$probs, function(p) p[cl, 2], numeric(1))), numeric(1))
binary_rank <- match(seq_len(binary_k), order(binary_burden))
binary_modal_rank <- binary_rank[binary_model$predclass]
ordinal_modal_rank <- class_rank[selected_model$predclass]
ari <- mclust::adjustedRandIndex(ordinal_modal_rank, binary_modal_rank)

binary_profiles <- rbindlist(lapply(seq_along(items), function(j) data.table(
  class_original = seq_len(binary_k), class_rank = binary_rank, item = items[j], symptom = unname(labels[items[j]]),
  positive_probability = binary_model$probs[[j]][, 2]
)))
binary_profiles[, item_order := match(item, items)]
setorder(binary_profiles, class_rank, item_order)
binary_profiles[, item_order := NULL]
binary_assignments <- data.table(
  class_original = seq_len(binary_k), class_rank = binary_rank,
  estimated_proportion = binary_model$P, estimated_n = binary_model$P * binary_model$Nobs,
  modal_n = tabulate(binary_model$predclass, nbins = binary_k),
  AvePP = vapply(seq_len(binary_k), function(cl) mean(binary_model$posterior[binary_model$predclass == cl, cl]), numeric(1)),
  expected_positive_symptoms = binary_burden
)
setorder(binary_assignments, class_rank)
binary_compare <- data.table(
  ordinal_classes = selected_k, binary_classes = binary_k, n = nrow(dt), adjusted_rand_index = ari,
  interpretation = ifelse(ari < .50, "类别结构对编码方式敏感，一致性有限",
                          ifelse(ari < .80, "类别结构中等一致", "类别结构较高一致"))
)
confusion <- as.data.table(table(ordinal_class_rank = ordinal_modal_rank, binary_class_rank = binary_modal_rank))

write_csv_utf8(binary_fit, out_path("05_sensitivity", "binary_lca_fit_1_to_8.csv"))
write_csv_utf8(binary_profiles, out_path("05_sensitivity", "binary_lca_profiles_full.csv"))
write_csv_utf8(binary_assignments, out_path("05_sensitivity", "binary_lca_class_summary.csv"))
write_csv_utf8(binary_compare, out_path("05_sensitivity", "binary_lca_comparison.csv"))
write_csv_utf8(confusion, out_path("05_sensitivity", "binary_lca_confusion.csv"))
saveRDS(binary_models, out_path("_work", "binary_lca_models_1_to_8.rds"), compress = "xz")
log_msg("INFO", "LCA complete: ordinal K=", selected_k, "; binary K=", binary_k,
        "; ARI=", sprintf("%.3f", ari), "; status=", model_status)
