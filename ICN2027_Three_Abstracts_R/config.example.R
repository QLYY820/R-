# Copy this file to config.R, then edit DATA_FILE only.
# config.R is ignored by Git and must never contain credentials.

# Relative to the RStudio project, or use an absolute path on the bastion host.
DATA_FILE <- file.path("data", "scored_data.rds")

# Analysis outputs. Keep this outside data/.
OUTPUT_DIR <- "results"

# TRUE stops immediately if the abstract sample sizes or key corrected results differ.
STRICT_REPRODUCTION <- TRUE

EXPECTED_N_ABSTRACT_1 <- 60836L
EXPECTED_N_ABSTRACT_2 <- 8540L
EXPECTED_N_ABSTRACT_3 <- 60836L
EXPECTED_FULL_N <- 60838L
