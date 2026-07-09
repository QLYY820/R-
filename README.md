# Nurse HRPL Reviewer-Response Statistics

R pipeline for rerunning the reviewer-requested analyses on the full nurse HRPL dataset.
The repository intentionally contains code only. Do not commit raw data or output tables.

## What It Runs

- Data and missingness overview.
- Scale reliability for HRPL, fatigue, social support, and depression items.
- Common method bias checks:
  - Harman single-factor PCA.
  - One-factor CFA vs four-factor CFA when sample size is sufficient.
- Core Spearman correlations with Benjamini-Hochberg adjusted p values.
- Linear mediation model: fatigue -> depression -> HRPL, with bootstrap CI.
- Moderated mediation checks using total social support.
- Conditional indirect effects at low, mean, and high social support.
- Social support dimension models: family, friend, significant-other support.
- Beverage frequency or dose correlations and adjusted linear models.
- Subgroup feasibility and subgroup interaction checks.

## Required R Packages

Install once on the server:

```r
install.packages(c(
  "readxl", "psych", "lavaan", "lm.beta", "ggplot2",
  "dplyr", "broom", "readr"
))
```

If the bastion host has no internet access, install these packages from the local
CRAN mirror or from your institution's offline package repository.

## Basic Run

From the repository directory:

```bash
Rscript R/reviewer_response_stats.R --data /path/to/full_data.xlsx --out analysis_outputs_full --bootstrap 5000
```

For a CSV file:

```bash
Rscript R/reviewer_response_stats.R --data /path/to/full_data.csv --out analysis_outputs_full --bootstrap 5000
```

If the Excel workbook has multiple sheets, the script reads the first sheet by
default. To choose a sheet by name or index:

```bash
Rscript R/reviewer_response_stats.R --data /path/to/full_data.xlsx --sheet 1 --out analysis_outputs_full
```

## Expected Variable Names

The defaults match the current manuscript/simulated-data dictionary:

- HRPL total score: `s`
- Fatigue total score: `pifa`
- Depression total score: `yiyu`
- Social support total score: `shehui`
- HRPL items: `q21_1` to `q21_6`
- Fatigue items: `q3_1` to `q3_14`
- Social support items: `q4_1` to `q4_12`
- Depression items: `q23_1` to `q23_9`
- Social support dimensions:
  - `F_jiatingneizhichi`
  - `F_pengyouzhichi`
  - `F_qitazhichi`
- Coffee/tea and milk variables:
  - `tea`, `milk`
  - `q34_1`, `q34_2`, `q34_3`, `q34_4`
  - `q36_1`, `q36_2`, `q36_3`, `q36_4`, `q36_6`, `q36_7`
- Default covariates, if present:
  - `gender`, `age`, `type`, `zhic`, `p`, `income`, `tea`, `milk`,
    `education`, `EDU`, `HUNYIN`, `kesi`, `BMI`, `A_gongzuoshichang`,
    `nianx`

If the real dataset uses different column names, edit the variable mapping block
near the top of `R/reviewer_response_stats.R`.

## Main Outputs

The output folder will contain:

- `_analysis_outputs.md`
- `data_shape.csv`
- `missing_values.csv`
- `core_variable_overview.csv`
- `reliability_alpha.csv`
- `common_method_bias_harman.csv`
- `common_method_bias_cfa.csv`
- `correlation_core_spearman.csv`
- `mediation_results.csv`
- `moderated_mediation_interactions.csv`
- `moderated_mediation_conditional_indirect.csv`
- `support_dimension_moderation.csv`
- `beverage_spearman_analysis.csv`
- `beverage_adjusted_models.csv`
- `subgroup_feasibility.csv`
- `subgroup_interactions.csv`
- `analysis_console_summary.txt`
- optional PNG/PDF plots

## Reporting Notes

- For the manuscript, describe cross-sectional mediation as association-level
  evidence and include a temporal-order caveat.
- For reviewer response, emphasize standardized beta, 95% CI, exact p value,
  and delta R-squared for interaction terms.
- Use full-sample CFA results only when the model converges and sample size is
  adequate.
