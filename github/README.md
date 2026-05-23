# Shift nurse sleep-rhythm depression risk prediction model

This folder contains a single-file R pipeline for a traditional logistic regression prediction model.

## RStudio one-line use

After this repository is on GitHub, run this in RStudio:

```r
source("https://raw.githubusercontent.com/<owner>/<repo>/<branch>/github/shift_sleep_depression_complete_pipeline.R")
```

Default data path used by the script:

```r
D:/护士队列/data/data.xlsx
```

Recommended RStudio use:

```r
Sys.setenv(NURSE_DATA_XLSX = "D:/护士队列/data/data.xlsx")
source("https://raw.githubusercontent.com/<owner>/<repo>/<branch>/github/shift_sleep_depression_complete_pipeline.R")
```

If the data file is elsewhere, change `NURSE_DATA_XLSX` to the actual local path before sourcing.

## What the script does

- Extracts needed fields from the large XLSX file.
- Selects shift/night nurses.
- Defines depression risk as `D_yiyu_all >= 10`.
- Treats negative skip codes as missing.
- Deletes complete-case missing records.
- Builds a traditional logistic regression model using univariate screening plus stepwise logistic regression.
- Outputs baseline table, missing summary, ROC, nomogram, calibration curve, DCA, clinical impact curve, and risk distribution.

## Main output files

- `baseline_table_complete_case.csv`
- `missing_summary.csv`
- `deleted_cases_summary.csv`
- `final_logistic_coefficients.csv`
- `train_validation_roc_auc_complete_case.csv`
- `roc_complete_case.png`
- `nomogram_complete_case.png`
- `calibration_complete_case.png`
- `decision_curve_complete_case.png`
- `clinical_impact_complete_case.png`
- `risk_distribution_complete_case.png`
