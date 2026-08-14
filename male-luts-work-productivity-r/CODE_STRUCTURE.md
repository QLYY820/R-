# Code structure

| File | Purpose |
|---|---|
| `config/config.R` | All paths, variable mappings, production gates, seeds and thresholds |
| `01_preflight_in_RStudio.R` | Fast data/identity/scoring audit without LCA |
| `02_run_production_in_RStudio.R` | One-click RStudio production entry point |
| `run_all.R` | Command-line and RStudio orchestration |
| `R/00_utils.R` | Shared I/O, logging, robust-SE, workbook and plotting helpers |
| `R/01_audit_scales.R` | ID, sex/module, LUTS quality and SPS-6 scoring audits |
| `R/02_lca.R` | Ordinal and binary LCA, class selection and local dependence |
| `R/03_association_continuous.R` | Pseudo-class association, sensitivities and continuous burden models |
| `R/04_tables_figures.R` | Main/supplement tables, figures and manuscript payload |
| `R/05_documents_and_manifest.R` | Word manuscript and Chinese audit report generation |
| `R/06_validate.R` | Syntax, artifacts, workbook and prohibited-hardcoding checks |
| `metadata/variable_dictionary.csv` | Non-identifying variable and scoring metadata |
| `install_packages.R` | Installs missing CRAN packages |
| `check_environment.R` | Checks R, packages and configured data source |
