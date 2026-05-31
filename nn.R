# Short GitHub loader for the bastion/RStudio Server environment.
# Run this line in RStudio Server:
# source("https://raw.githubusercontent.com/QLYY820/R-/net/nn.R")

u <- "https://raw.githubusercontent.com/QLYY820/R-/net/outputs/somatic_exhaustion_network/bastion_reproduce_somatic_exhaustion_network.R"
f <- tempfile(fileext = ".R")
download.file(u, f, mode = "wb")
source(f, encoding = "UTF-8")

