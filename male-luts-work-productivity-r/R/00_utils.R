options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("data.table", "digest", "openxlsx", "poLCA", "psych",
                       "sandwich", "lmtest", "mclust", "ggplot2", "jsonlite",
                       "splines", "car", "officer", "flextable", "readxl")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                               logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "))

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x[1])) y else x
norm_char <- function(x) {
  z <- trimws(as.character(x))
  z[z %in% c("", "NA", "N/A", "NULL", "null", "NaN")] <- NA_character_
  z
}
num <- function(x) suppressWarnings(as.numeric(norm_char(x)))
safe_pct <- function(n, d) ifelse(is.na(d) | d <= 0, NA_real_, 100 * n / d)
fmt_p <- function(p) ifelse(is.na(p), "NA", ifelse(p < .001, "<0.001", sprintf("%.3f", p)))
fmt_ci <- function(est, lo, hi, digits = 2) {
  sprintf(paste0("%.", digits, "f (95%%CI %.", digits, "f to %.", digits, "f)"), est, lo, hi)
}

write_csv_utf8 <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(x, path, bom = TRUE, na = "")
}
write_lines_utf8 <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(x), con, useBytes = TRUE)
}
hash_file <- function(path) {
  if (is.null(path) || !length(path) || is.na(path[1]) || !nzchar(path[1]) ||
      !file.exists(path) || isTRUE(file.info(path)$isdir)) return(NA_character_)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

make_lca_cache_signature <- function(tag) {
  list(
    pipeline_version = CFG$pipeline_version,
    data_hash = hash_file(out_path("01_data", "analysis_module_completers_deidentified.rds")),
    lca_script_hash = hash_file(out_path("R", "02_lca.R")),
    tag = tag,
    items = CFG$variables$luts,
    random_seed = CFG$random_seed,
    random_starts = unname(CFG$lca_random_starts),
    max_iterations = CFG$max_iterations
  )
}
resolve_path <- function(path, root = PIPELINE_ROOT) {
  if (is.null(path) || !length(path) || is.na(path[1]) || !nzchar(path[1])) return(NA_character_)
  if (grepl("^[A-Za-z]:[/\\\\]", path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  normalizePath(file.path(root, path), winslash = "/", mustWork = FALSE)
}
ensure_dirs <- function() {
  dirs <- c("00_audit", "01_data", "02_scales", "03_lca", "04_association",
            "05_sensitivity", "06_tables", "07_figures", "08_manuscript",
            "09_logs", "outputs", "_work", "_rendered")
  invisible(lapply(file.path(PIPELINE_ROOT, dirs), dir.create, recursive = TRUE,
                   showWarnings = FALSE))
}
out_path <- function(...) file.path(PIPELINE_ROOT, ...)

log_msg <- function(level = "INFO", ...) {
  msg <- paste0(..., collapse = "")
  line <- sprintf("%s [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), level, msg)
  cat(line, "\n")
  cat(line, "\n", file = out_path("outputs", "analysis_run_log.txt"), append = TRUE)
  invisible(line)
}

gate <- function(condition, code, message, production_block = TRUE) {
  if (isTRUE(condition)) return(invisible(TRUE))
  row <- data.table::data.table(code = code, mode = CFG$run_mode,
                                status = if (CFG$run_mode == "production" && production_block) "BLOCK" else "WARNING",
                                message = message)
  write_csv_utf8(row, out_path("00_audit", paste0("gate_", code, ".csv")))
  if (CFG$run_mode == "production" && production_block) {
    log_msg("ERROR", code, ": ", message)
    stop(code, ": ", message, call. = FALSE)
  }
  log_msg("WARNING_TEST_MODE", code, ": ", message, " Test mode continues.")
  invisible(FALSE)
}

alpha_safe <- function(x) {
  x <- as.data.frame(x)
  if (ncol(x) < 2 || nrow(x) < 3) return(NA_real_)
  suppressWarnings(psych::alpha(x, check.keys = FALSE, warnings = FALSE)$total$raw_alpha)
}
omega_safe <- function(x) {
  x <- as.data.frame(x)
  if (ncol(x) < 3 || nrow(x) < 10) return(NA_real_)
  tryCatch(suppressMessages(suppressWarnings(
    psych::omega(x, nfactors = 1, plot = FALSE, warnings = FALSE,
                 flip = FALSE, key = rep(1, ncol(x)))$omega.tot)),
    error = function(e) NA_real_)
}

write_workbook <- function(path, sheets, warning_banner = NULL) {
  wb <- openxlsx::createWorkbook(creator = "Reproducible LUTS pipeline")
  header_style <- openxlsx::createStyle(fontName = "Microsoft YaHei", fontSize = 10,
    textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom", valign = "center")
  warn_style <- openxlsx::createStyle(fontName = "Microsoft YaHei", fontSize = 11,
    textDecoration = "bold", fontColour = "#9C0006", fgFill = "#FFC7CE", wrapText = TRUE)
  body_style <- openxlsx::createStyle(fontName = "Microsoft YaHei", fontSize = 9, valign = "top")
  for (nm in names(sheets)) {
    sn <- substr(gsub("[:\\\\/?*\\[\\]]", "_", nm), 1, 31)
    openxlsx::addWorksheet(wb, sn, gridLines = FALSE)
    start <- 1L
    if (!is.null(warning_banner)) {
      openxlsx::writeData(wb, sn, warning_banner, startRow = 1, startCol = 1)
      openxlsx::mergeCells(wb, sn, cols = 1:max(2, ncol(sheets[[nm]])), rows = 1)
      openxlsx::addStyle(wb, sn, warn_style, rows = 1, cols = 1:max(2, ncol(sheets[[nm]])), gridExpand = TRUE)
      openxlsx::setRowHeights(wb, sn, 1, 32)
      start <- 3L
    }
    openxlsx::writeData(wb, sn, sheets[[nm]], startRow = start, headerStyle = header_style, withFilter = FALSE)
    if (nrow(sheets[[nm]]) > 0 && ncol(sheets[[nm]]) > 0) {
      openxlsx::addStyle(wb, sn, body_style, rows = (start + 1):(start + nrow(sheets[[nm]])),
                         cols = seq_len(ncol(sheets[[nm]])), gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::freezePane(wb, sn, firstActiveRow = start + 1)
    openxlsx::setColWidths(wb, sn, cols = seq_len(max(1, ncol(sheets[[nm]]))), widths = "auto")
    widths <- pmin(45, pmax(10, vapply(seq_len(ncol(sheets[[nm]])), function(j) {
      max(nchar(c(names(sheets[[nm]])[j], as.character(utils::head(sheets[[nm]][[j]], 200)))), na.rm = TRUE) + 2
    }, numeric(1))))
    if (length(widths)) openxlsx::setColWidths(wb, sn, cols = seq_along(widths), widths = widths)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

hc3_tidy <- function(fit) {
  V <- sandwich::vcovHC(fit, type = "HC3")
  b <- stats::coef(fit)
  se <- sqrt(diag(V))
  data.table::data.table(term = names(b), estimate = as.numeric(b), se = as.numeric(se),
    lower = as.numeric(b - stats::qnorm(.975) * se), upper = as.numeric(b + stats::qnorm(.975) * se),
    p_value = as.numeric(2 * stats::pnorm(-abs(b / se))))
}

partial_r2 <- function(full, reduced) {
  sse_full <- sum(stats::residuals(full)^2)
  sse_reduced <- sum(stats::residuals(reduced)^2)
  if (!is.finite(sse_reduced) || sse_reduced <= 0) return(NA_real_)
  (sse_reduced - sse_full) / sse_reduced
}

model_metrics <- function(fit, reduced = NULL, model_name = "model") {
  s <- summary(fit)
  data.table::data.table(model = model_name, n = stats::nobs(fit), parameters = length(stats::coef(fit)),
    R2 = s$r.squared, adjusted_R2 = s$adj.r.squared, AIC = stats::AIC(fit), BIC = stats::BIC(fit),
    delta_R2 = if (is.null(reduced)) NA_real_ else s$r.squared - summary(reduced)$r.squared,
    partial_R2 = if (is.null(reduced)) NA_real_ else partial_r2(fit, reduced))
}

save_plot_set <- function(plot, stem, width, height) {
  ggplot2::ggsave(out_path("07_figures", paste0(stem, ".png")), plot, width = width, height = height, dpi = 300, bg = "white")
  ggplot2::ggsave(out_path("07_figures", paste0(stem, ".tiff")), plot, width = width, height = height, dpi = 600,
                  compression = "lzw", bg = "white")
  ggplot2::ggsave(out_path("07_figures", paste0(stem, ".pdf")), plot, width = width, height = height,
                  device = grDevices::cairo_pdf, bg = "white")
}
