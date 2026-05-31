# Full GitHub loader for the bastion/RStudio Server environment.
# Run this single line in RStudio Server:
# source("https://raw.githubusercontent.com/QLYY820/R-/net/nn_full.R")

# This final-mode loader keeps NIRA exact, adds bridge plots, and enables:
# 1) nonparametric bootstrap edge/centrality accuracy
# 2) case-dropping bootstrap centrality stability
# 3) CS coefficients
#
# Increase SOMATIC_BOOT_N and SOMATIC_BOOT_CASE_N if the bastion can run longer.
Sys.setenv(
  SOMATIC_FAST_TEST = "0",
  SOMATIC_NIRA_MODE = "exact",
  SOMATIC_RUN_BOOTSTRAP = "1",
  SOMATIC_BOOT_N = "200",
  SOMATIC_RUN_CASE_STABILITY = "1",
  SOMATIC_BOOT_CASE_N = "100",
  SOMATIC_BOOT_SEED = "20260601"
)

u <- "https://raw.githubusercontent.com/QLYY820/R-/net/outputs/somatic_exhaustion_network/bastion_reproduce_somatic_exhaustion_network.R?v=20260601_bridge_boot_cs"
f <- "nurse_network_full_bootstrap_cs.R"
download.file(u, f, mode = "wb")
source(f, encoding = "UTF-8")
