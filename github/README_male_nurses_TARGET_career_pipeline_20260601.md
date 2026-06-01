# TARGET Male Nurses Career-Development Pipeline

This folder contains a new, standalone RStudio script for the male nurses manuscript:

`male_nurses_TARGET_career_pipeline_20260601.R`

It does not overwrite the earlier uploaded R script:

`shift_sleep_depression_complete_pipeline.R`

## What the script does

- Reads the original TARGET baseline data.
- Builds the analytic dataset for male nurses.
- Constructs career-development and turnover-intention variables.
- Runs the main multivariable logistic regression models.
- Runs sensitivity analyses:
  - age group replacing work years;
  - turnover-intention item average >=3;
  - social-support direction check;
  - sparse-category event counts;
  - optional Firth logistic regression for administrative position;
  - collapsed-education administrative-position model.
- Exports tables as CSV files.
- Exports a high-resolution forest plot if `ggplot2` is installed.

## How to run in RStudio

### Option 1: the original data are already loaded as `data`

```r
source("male_nurses_TARGET_career_pipeline_20260601.R")
```

### Option 2: read from a local file

```r
Sys.setenv(TARGET_DATA_PATH = "D:/path/to/original_data.sav")
source("male_nurses_TARGET_career_pipeline_20260601.R")
```

Supported formats: `.rds`, `.RData`, `.rda`, `.sav`, `.xlsx`, `.xls`, `.csv`, `.tsv`.

### Option 3: source directly from GitHub

```r
source("https://raw.githubusercontent.com/QLYY820/R-/refs/heads/codex/shift-sleep-depression-code/github/male_nurses_TARGET_career_pipeline_20260601.R")
```

## Optional settings

```r
Sys.setenv(TARGET_OUTPUT_DIR = "E:/TARGET_male_nurses_outputs")
Sys.setenv(TARGET_TURNOVER_CUTOFF = "16")
Sys.setenv(TARGET_INSTALL_PACKAGES = "1")
```

`TARGET_INSTALL_PACKAGES=1` allows the script to install missing optional packages. If this is not set, missing optional packages are reported and the related steps are skipped or use base R fallbacks.

## Main outputs

By default, outputs are saved under:

```text
analysis_outputs/male_nurses_TARGET_career_<timestamp>/
```

Important files:

- `tables_csv/Table1_male_characteristics_blank_repeated_labels.csv`
- `tables_csv/Table2_career_turnover_indicators.csv`
- `tables_csv/S2_full_model_high_turnover.csv`
- `tables_csv/S3_full_model_professional_title.csv`
- `tables_csv/S4_full_model_administrative_position.csv`
- `tables_csv/S5_full_model_high_income.csv`
- `tables_csv/S9_social_support_direction_check.csv`
- `tables_csv/S10_sparse_category_event_counts.csv`
- `figures/forest_high_turnover.png`

## Notes

- The script does not save or upload individual-level data.
- Continuous psychosocial variables are standardized before modeling.
- Logistic regression results are exported as odds ratios with 95% confidence intervals.
- Firth penalized logistic regression is run only when the `logistf` package is installed.

