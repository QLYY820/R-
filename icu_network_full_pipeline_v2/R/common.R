suppressPackageStartupMessages({
  library(data.table)
  library(qgraph)
  library(networktools)
  library(Matrix)
})

NODE_IDS <- c(paste0("ANX", 1:7), paste0("DEP", 1:9), paste0("STR", 1:10),
              paste0("BO", 1:22), paste0("FAT", 1:14))
NODE_DOMAINS <- setNames(c(rep("Anxiety", 7), rep("Depression", 9), rep("Stress", 10),
                           rep("Burnout", 22), rep("Fatigue", 14)), NODE_IDS)
COMMUNITIES <- setNames(as.integer(factor(NODE_DOMAINS,
  levels = c("Anxiety", "Depression", "Stress", "Burnout", "Fatigue"))), NODE_IDS)
PSS_REVERSE <- c(4, 5, 7, 8)
MBI_REVERSE <- c(4, 7, 9, 12, 17, 18, 19, 21)
FAT_REVERSE <- c(10, 13, 14)
LEGACY22 <- c("ANX4", "ANX6", "ANX7", "BO10", "BO13", "BO15", "BO18", "BO19",
              "BO2", "BO3", "BO8", "DEP1", "DEP2", "DEP4", "DEP8", "FAT6", "FAT8",
              "STR1", "STR10", "STR2", "STR3", "STR8")

node_range <- function(node) {
  if (grepl("^(ANX|DEP)", node)) return(c(0, 3))
  if (grepl("^STR", node)) return(c(0, 4))
  if (grepl("^BO", node)) return(c(0, 6))
  c(0, 1)
}

ensure_dir <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)

append_log <- function(file, ...) {
  line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste(..., collapse = " "))
  cat(line, "\n", file = file, append = TRUE)
  message(line)
}

assert_scored_input <- function(d, label = "analysis input", expected_group_counts = NULL) {
  required <- c("analysis_id", "participant_hash", "A_q9", "eligible_primary_network", NODE_IDS)
  missing_vars <- setdiff(required, names(d))
  if (length(missing_vars)) stop(label, ": missing required variables: ", paste(missing_vars, collapse = ", "))
  if (!is.logical(d$eligible_primary_network)) stop(label, ": eligible_primary_network must be logical")
  e <- d[eligible_primary_network == TRUE]
  if (!nrow(e)) stop(label, ": eligible sample is empty")
  if (anyDuplicated(e$participant_hash)) stop(label, ": duplicate participant_hash remains in eligible sample")
  if (anyDuplicated(e$analysis_id)) stop(label, ": duplicate analysis_id remains in eligible sample")
  if (anyNA(e[, ..NODE_IDS])) stop(label, ": eligible nodes contain missing values")
  for (node in NODE_IDS) {
    r <- node_range(node)
    x <- e[[node]]
    if (any(x < r[[1]] | x > r[[2]])) stop(label, ": node out of range: ", node)
  }
  if (!is.null(expected_group_counts)) {
    observed <- c(ICU = sum(e$A_q9 == 8), nonICU = sum(e$A_q9 != 8))
    if (!identical(as.integer(observed), as.integer(expected_group_counts))) {
      stop(label, ": group counts do not match sample flow. observed=", paste(observed, collapse = ","),
           "; expected=", paste(expected_group_counts, collapse = ","))
    }
  }
  invisible(e)
}

assert_group_matrix <- function(d, nodes = NODE_IDS, label = "network group", expected_n = NULL) {
  if (!all(nodes %in% names(d))) stop(label, ": required nodes missing")
  if (anyNA(d[, ..nodes])) stop(label, ": node missingness detected")
  if (anyDuplicated(d$participant_hash)) stop(label, ": duplicate participant ID")
  if (!is.null(expected_n) && nrow(d) != expected_n) stop(label, ": n mismatch")
  for (node in nodes) {
    r <- node_range(node)
    if (any(d[[node]] < r[[1]] | d[[node]] > r[[2]])) stop(label, ": out-of-range node ", node)
  }
  invisible(TRUE)
}

safe_pd <- function(S) {
  if (all(eigen(S, symmetric = TRUE, only.values = TRUE)$values > 1e-8)) return(S)
  as.matrix(Matrix::nearPD(S, corr = TRUE, keepDiag = TRUE)$mat)
}

estimate_network <- function(dat, nodes = colnames(dat), correlation = "spearman", gamma = 0.5,
                             threshold = FALSE) {
  dat <- as.data.frame(dat[, nodes, drop = FALSE])
  if (anyNA(dat)) stop("Network data contain missing values")
  if (correlation == "spearman") {
    S <- cor(dat, method = "spearman", use = "complete.obs")
    S <- safe_pd(S)
  } else if (correlation == "cor_auto") {
    S <- qgraph::cor_auto(dat, detectOrdinal = TRUE, ordinalLevelMax = 7,
                          forcePD = TRUE, missing = "listwise", verbose = FALSE)
    S <- safe_pd(S)
  } else stop("Unsupported correlation: ", correlation)
  W <- qgraph::EBICglasso(S, n = nrow(dat), gamma = gamma, threshold = threshold,
                          penalize.diagonal = FALSE, checkPD = TRUE, verbose = FALSE)
  dimnames(W) <- list(nodes, nodes)
  list(correlation = S, weights = W, n = nrow(dat), nodes = nodes,
       correlation_method = correlation, gamma = gamma, threshold = threshold)
}

edge_long <- function(W, network, analysis) {
  ix <- which(upper.tri(W), arr.ind = TRUE)
  data.table(analysis = analysis, network = network,
             node1 = rownames(W)[ix[,1]], node2 = colnames(W)[ix[,2]],
             weight = W[ix], absolute_weight = abs(W[ix]),
             sign = fifelse(W[ix] > 0, "positive", fifelse(W[ix] < 0, "negative", "zero")),
             retained = W[ix] != 0)
}

centrality_table <- function(W, network, analysis) {
  nodes <- colnames(W)
  b <- networktools::bridge(W, communities = COMMUNITIES[nodes], normalize = FALSE)
  bn <- networktools::bridge(W, communities = COMMUNITIES[nodes], normalize = TRUE)
  out <- data.table(
    analysis = analysis, network = network, node_id = nodes, domain = NODE_DOMAINS[nodes],
    strength = rowSums(abs(W)), expected_influence = rowSums(W),
    bridge_strength = as.numeric(b[["Bridge Strength"]][nodes]),
    bridge_expected_influence = as.numeric(b[["Bridge Expected Influence (1-step)"]][nodes]),
    normalized_bridge_strength = as.numeric(bn[["Bridge Strength"]][nodes]),
    normalized_bridge_expected_influence = as.numeric(bn[["Bridge Expected Influence (1-step)"]][nodes])
  )
  for (v in c("strength", "expected_influence", "bridge_strength", "bridge_expected_influence",
              "normalized_bridge_strength", "normalized_bridge_expected_influence")) {
    out[, paste0(v, "_z") := as.numeric(scale(get(v)))]
  }
  out
}

network_summary <- function(W, network, analysis, n) {
  v <- W[upper.tri(W)]
  data.table(analysis = analysis, network = network, n = n, n_nodes = nrow(W),
             possible_edges = length(v), nonzero_edges = sum(v != 0),
             positive_edges = sum(v > 0), negative_edges = sum(v < 0),
             density = sum(v != 0) / length(v), global_strength = sum(abs(v)),
             mean_absolute_nonzero_edge = ifelse(any(v != 0), mean(abs(v[v != 0])), 0),
             maximum_absolute_edge = max(abs(v)))
}

write_matrix <- function(M, file) {
  ensure_dir(dirname(file))
  data.table::fwrite(as.data.table(M, keep.rownames = "node_id"), file)
}

rank_correlation <- function(x, y) suppressWarnings(cor(x, y, method = "spearman", use = "complete.obs"))

top_nodes <- function(values, n = 5L) names(sort(values, decreasing = TRUE))[seq_len(min(n, length(values)))]
