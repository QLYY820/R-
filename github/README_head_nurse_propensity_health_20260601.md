# Head Nurse Propensity Occupational-Health Analysis

This is the non-overwriting GitHub note for:

`scripts/run_head_nurse_propensity_health_analysis_20260601.R`

## Bastion/RStudio use

After this branch is pushed, run in RStudio Server:

```r
source("https://raw.githubusercontent.com/QLYY820/R-/codex/head-nurse-propensity-health-20260601/scripts/run_head_nurse_propensity_health_analysis_20260601.R", encoding = "UTF-8")
```

The script first looks for loaded data objects named `合并数据64114_去重后`, `合并数据`, `data`, `raw_data`, or `dat`. If none exists, set a selected-column CSV path before sourcing:

```r
Sys.setenv(NURSE_DATA_CSV = "analysis_outputs/late_career_turnover_cleaned/01_selected_columns.csv")
Sys.setenv(NURSE_OUTPUT_DIR = "analysis_outputs/head_nurse_propensity_health_20260601")
source("https://raw.githubusercontent.com/QLYY820/R-/codex/head-nurse-propensity-health-20260601/scripts/run_head_nurse_propensity_health_analysis_20260601.R", encoding = "UTF-8")
```

## Cleaning basis

The main exposure is `A_q12=3` head nurse versus `A_q12=1` staff nurse without an administrative role. Other management levels are excluded from the primary contrast. Duplicate IDs after the first record, implausible age, implausible work tenure, work tenure exceeding age minus 16 years, and records with all main outcomes missing are excluded.

Night shift (`C_q1`) is not included in the propensity-score model because it may lie on the pathway from head-nurse role to occupational health. The script uses a three-model strategy: total role difference, night-shift-adjusted outcome model, and day-shift-only restriction.

The local trial run used ATT propensity-score weighting. Maximum absolute SMD decreased from 1.784 before weighting to 0.090 after weighting.
