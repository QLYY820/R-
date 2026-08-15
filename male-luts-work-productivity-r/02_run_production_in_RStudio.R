# After reviewing the preflight outputs and confirming the four production
# gates in config/config.R, source this file in RStudio.

Sys.setenv(LUTS_RUN_MODE = "production",
           LUTS_PIPELINE_ROOT = normalizePath(getwd(), winslash = "/", mustWork = TRUE))
source("run_all.R", encoding = "UTF-8")
