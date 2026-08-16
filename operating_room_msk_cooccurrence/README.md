# Operating-room nurse multisite musculoskeletal symptom analysis

## Project

- Project name: `operating_room_msk_cooccurrence`
- Research question: identify latent multisite musculoskeletal symptom profiles and the core symptom network among operating-room nurses, then estimate cross-sectional associations of work-time factors with profile membership.
- Statistical analyses: descriptive prevalence with Wilson 95% CIs; 1-7 class latent class analysis; EBIC-regularized Ising network with AND rule and bootstrap stability; multinomial logistic regression with 20-dataset MICE, survey-year adjustment, multiplicity control, sensitivity analyses, diagnostics, and model-based adjusted probabilities.

## Fixed data source

The formal bastion-host run uses only:

- Source workbook: `/home/sunxuexia/我的数据/data.xlsx`
- All-text cache: `/home/sunxuexia/我的数据/data_text.rds`

The import step forces every XLSX column to text before creating the RDS. If a valid nonempty all-text RDS already exists, it is reused and never overwritten automatically. Paths can be supplied through `OR_MSK_RAW_XLSX` and `OR_MSK_DATA_TEXT_RDS` only for controlled testing.

## One-command entry point

Formal foreground run:

```bash
Rscript run_all.R --mode=formal
```

Formal background run:

```bash
bash run_bastion.sh
```

Local test mode using a deidentified prepared analysis RDS:

```bash
Rscript run_all.R --mode=test \
  --analysis-rds=/path/to/operating_room_analysis_data.rds \
  --output-root=/new/nonexistent/output/directory
```

Test mode reduces LCA starts, network bootstraps, MICE datasets/iterations, and posterior draws. It must never be used for manuscript estimates.

## Output safety

Every run creates a new output directory and aborts if the target already exists. Raw data are read-only. Git ignores data, RDS/XLSX files, participant-level outputs, logs, PIDs, and run results. Only code, README, tests, and configuration are intended for GitHub.

Success requires all of the following:

1. `Rscript` exit code 0.
2. Every step log contains its `STEP_COMPLETE=` marker.
3. `RUN_COMPLETE.ok` exists and is nonempty.
4. `validation_summary.csv` reports PASS for required files, sample flow, LCA assignments, the Ising matrix, and regression confidence intervals.
5. `sessionInfo.txt` and `_analysis_outputs.md` exist.
