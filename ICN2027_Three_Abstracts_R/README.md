# ICN 2027: reproducible R code for three abstracts

This repository contains the complete analysis code for the three ICN 2027
abstracts. It contains no participant-level data and no previously generated
results.

The code is compatible with R 4.1 or later. Restricted cubic spline bases and
robust joint tests are implemented within the project, so `rms`, `Hmisc`, `car`,
`MatrixModels`, and `quantreg` are not required.

## Run directly in RStudio

1. Clone or download this repository to the bastion host.
2. Open `ICN2027_Three_Abstracts_R.Rproj` in RStudio.
3. Copy `config.example.R` to `config.R`.
4. In `config.R`, set `DATA_FILE` to the protected real-data RDS or CSV file.
5. If required, run `00_install_packages.R` once.
6. Run `00_run_all.R` to reproduce all three analyses, or run an individual numbered script.
7. Review outputs under `results/` and retain the generated `analysis_manifest.txt` and `sessionInfo.txt` files.

Environment variables `ICN_DATA_FILE` and `ICN_OUTPUT_DIR` can be used instead
of `config.R`, which is useful on a Linux bastion host.

## Reproduction gates

With `STRICT_REPRODUCTION <- TRUE`, the code stops if the final sample sizes do
not reproduce 60,836, 8,540, and 60,836 for Abstracts 1-3, respectively. The
corrected Abstract 2 analysis also verifies:

- direct summation of the six already-scored SPS-6 items;
- total range 6-30 and no imputation;
- full-data Cronbach alpha approximately 0.915;
- 8,540-sample Cronbach alpha approximately 0.880;
- all corrected item-total correlations are positive;
- 20 fixed manual recalculations agree exactly;
- knots reproduce 0, 4, 11, and 24;
- adjusted differences reproduce approximately 2.49, 5.70, and 8.46.

Set strict reproduction to `FALSE` only when intentionally analysing a different
data release. All validation tables are still produced.

## Privacy and GitHub

Never commit `config.R`, real data, model objects, or generated results. The
included `.gitignore` excludes these files by default. Before sharing code,
confirm that the dataset is de-identified and that local governance permits the
analysis on the bastion host.

## Expected headline results for comparison

| Abstract | Analysis N | Key result |
|---|---:|---|
| 1 | 60,836 | Any negative acts: +13.04 exhaustion points; per 10 points among exposed: +3.76 |
| 2 | 8,540 | Kupperman 6/16/31 versus 0: +2.49 / +5.70 / +8.46 productivity-loss points |
| 3 | 60,836 | Balance 44/50/57 versus 40: 1.59 / 3.72 / 4.73 lower turnover-intention points |
