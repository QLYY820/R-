# Download and open the full bastion/RStudio Server R script.
# Run this line in RStudio Server:
# source("https://raw.githubusercontent.com/QLYY820/R-/net/open.R")

u <- "https://raw.githubusercontent.com/QLYY820/R-/net/outputs/somatic_exhaustion_network/bastion_reproduce_somatic_exhaustion_network.R?v=20260531_prefer_dataxlsx"
f <- "nurse_network.R"
download.file(u, f, mode = "wb")
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  rstudioapi::navigateToFile(f)
} else {
  file.edit(f)
}
