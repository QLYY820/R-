# Head-nurse final analysis on bastion RStudio

## Assumption

The final questionnaire data have already been loaded in RStudio as an object named `data`.
The column names and scale structure should match the local trial dataset.

## Recommended RStudio command

Run this from a local clone or downloaded copy of the GitHub repository:

```r
source("scripts/run_head_nurse_final_bastion_analysis.R", encoding = "UTF-8")
```

The default output folder is:

```text
analysis_outputs/head_nurse_occupational_health_final
```

## Direct GitHub source command

If the bastion RStudio can access GitHub, the same entry script can be run directly:

```r
source("https://raw.githubusercontent.com/QLYY820/R-/codex/msk-lca-rstudio-bastion-20260602/scripts/run_head_nurse_final_bastion_analysis.R", encoding = "UTF-8")
```

The entry script automatically tries to install missing core R packages from CRAN. Word manuscript generation is skipped by default on the bastion server (`HEAD_NURSE_SKIP_WORD=TRUE`). The script will complete the core analysis and save CSV tables, PNG figures, cleaning rules, and the run log.

After moving the statistical output folder back to a local computer, Word files can be regenerated locally. If the server can compile document packages and you explicitly want Word output on the bastion server, run:

```r
Sys.setenv(HEAD_NURSE_SKIP_WORD = "FALSE")
install.packages(c("ragg", "officer", "flextable"), repos = "https://cloud.r-project.org")
```

## Custom output folder

```r
Sys.setenv(HEAD_NURSE_BASE_DIR = "analysis_outputs/head_nurse_occupational_health_final_20260610")
source("scripts/run_head_nurse_final_bastion_analysis.R", encoding = "UTF-8")
```

## Main outputs

- `head_nurse_occupational_health_baseline_cross_sectional_manuscript.docx`
- `head_nurse_occupational_health_supplementary_material.docx`
- `00_cleaning_rules.md`
- `analysis_run_log.txt`
- CSV tables and PNG figures used by the manuscript and supplementary material
