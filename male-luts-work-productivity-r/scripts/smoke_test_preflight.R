# Synthetic smoke test for repository code only. No research data are used.
suppressPackageStartupMessages(library(data.table))
set.seed(20260814)
n <- 64114L
n_module <- 800L

data <- data.table(
  id = sprintf("SYNTH_%06d", seq_len(n)),
  A_q2 = c(rep("1", n_module), rep("2", n - n_module)),
  A_age = sample(20:60, n, replace = TRUE),
  A_BMI = round(rnorm(n, 22, 3), 1),
  A_q5 = sample(1:2, n, replace = TRUE),
  A_q6 = sample(1:2, n, replace = TRUE),
  A_q9 = sample(1:3, n, replace = TRUE),
  A_q11 = sample(1:3, n, replace = TRUE),
  A_q12 = sample(0:1, n, replace = TRUE),
  A_q24 = sample(0:2, n, replace = TRUE),
  A_q30 = sample(0:2, n, replace = TRUE),
  A_q19_1_1 = sample(c("无", "有"), n, replace = TRUE, prob = c(.8, .2)),
  C_q1 = sample(0:1, n, replace = TRUE),
  C_q42 = pmax(0, round(rnorm(n, 2, 1.5))),
  A_q38 = round(runif(n, 800, 3000)),
  work_y = sample(1985:2025, n, replace = TRUE),
  A_year_sub = 2026,
  A_year = sample(1965:2005, n, replace = TRUE),
  timetaken.x = sample(650:1800, n, replace = TRUE),
  timetaken.y = sample(650:1800, n, replace = TRUE),
  timetaken.x.x = sample(650:1800, n, replace = TRUE),
  timetaken.y.y = sample(650:1800, n, replace = TRUE)
)

luts <- c("I_q1", "I_q3", "I_q5", "I_q7", "I_q11", "I_q13", "I_q15",
          "I_q19", "I_q23", "I_q27", "I_q31", "I_q35", "I_q37")
for (v in luts) data[, (v) := c(sample(0:4, n_module, replace = TRUE,
                                       prob = c(.65, .18, .10, .05, .02)),
                                  rep(NA_integer_, n - n_module))]
for (v in paste0("D_q21_", 1:6)) data[, (v) := sample(1:5, n, replace = TRUE)]
data[, I_niaolu_score_all_M := rowSums(.SD), .SDcols = luts]
data[, D_shengchanlishousun_all := D_q21_1 + D_q21_2 + D_q21_3 + D_q21_4 +
       (6 - D_q21_5) + (6 - D_q21_6)]

source("01_preflight_in_RStudio.R", encoding = "UTF-8")
audit <- fread("00_audit/id_linkage_summary.csv")
stopifnot(audit[metric == "total_records", n] == 64114L)
stopifnot(file.exists("outputs/sex_module_linkage_audit.xlsx"))
stopifnot(file.exists("outputs/SPS6_scoring_audit.xlsx"))
message("Synthetic 64,114-record preflight smoke test passed.")
