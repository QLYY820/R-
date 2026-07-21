# Bastion Run Checklist

1. Confirm the object `data` is a `data.frame` or `data.table`.
2. Confirm the supplied dictionary matches the formal variable names.
3. Confirm at least 20 GB of free private storage is available.
4. Confirm all packages using `source("install_packages.R")`.
5. Set cores conservatively for the bastion allocation.
6. Run into a new empty output directory with `mode = "real"`.
7. Monitor `logs/pipeline_master.log` and per-step console logs.
8. Do not interpret results until `statistical_coverage_audit_v2.csv` has no FAIL rows.
9. Keep the original data and all result directories outside Git.
