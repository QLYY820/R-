# Expected behavior: this script exits nonzero because the repository ships
# with all production-confirmation switches set to FALSE.
source("scripts/smoke_test_preflight.R", encoding = "UTF-8")
source("02_run_production_in_RStudio.R", encoding = "UTF-8")
