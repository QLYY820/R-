# ICU Nurse Symptom Network Analysis Pipeline V2

This repository contains the R pipeline for the individual-level, unweighted,
cross-sectional comparison of ICU and non-ICU nurses. Data are not included.

## Bastion R: run an object named `data`

Clone the dedicated branch and enter the project directory:

```bash
git clone --branch codex/icu-network-full-pipeline-v2 --single-branch https://github.com/QLYY820/R-.git
cd R-/icu_network_full_pipeline_v2
```

Install or verify packages once:

```r
source("install_packages.R")
```

After the formal dataset has been loaded into R as an object named `data`:

```r
stopifnot(exists("data"), is.data.frame(data))
source("preflight_data_object.R")
preflight <- preflight_icu_data(data)

# Continue only when this is TRUE:
preflight$ready

source("run_from_R_object.R")

run_icu_network_pipeline(
  data = data,
  output_dir = "/your/private/path/ICU_network_REAL_RUN",
  mode = "real",
  cores = 12,
  bootstrap_cores = 8
)
```

The output directory must be empty for a new run. Calling the same command with
the same output directory resumes completed steps using `.pipeline_state` markers.

## Required input structure

The supplied dictionary expects the original item names used by the nurse cohort:

- `D_q22_1` to `D_q22_7`
- `D_q23_1` to `D_q23_9`
- `D_q24_1` to `D_q24_10`
- `F_q3_1` to `F_q3_22`
- `G_q3_1` to `G_q3_14`

It also expects participant ID, department, existing scale totals, demographics,
work variables, and response-time variables used by the scoring and QC scripts.
Run the preflight commands below before launching the several-hour pipeline:

```r
dictionary <- data.table::fread("config/source_dictionary.csv")
setdiff(dictionary$variable_name, names(data))
anyDuplicated(names(data))
dim(data)
```

`setdiff(...)` must return `character(0)`. The scoring module stops if required
items are missing, item values are outside documented ranges, or recomputed and
stored scale totals differ.

## Main analyses

- Descriptive statistics, Cronbach alpha, unadjusted comparisons, and HC3 models
- PSM-A, PSM-B, and ICU versus ordinary inpatient PSM-A
- Full 62-node Spearman EBICglasso networks with predefined domains
- Eleven network sensitivity specifications
- Unpaired NCT: 5,000 main permutations and at least 1,000 sensitivity permutations
- Two 200-iteration downsampling scenarios and forty fixed-seed NCT runs
- 1,000 edge-accuracy and case-dropping bootstraps
- Custom bridge-centrality stability analysis
- Five-fold mixed categorical MGM predictability analysis
- Content audit, logs, seeds, package versions, matrices, tables, and figures

Hospital clustering, multilevel hospital models, region analyses, and survey
weighting are not performed because those variables do not exist in the study data.

## Security

Do not place formal data or generated results inside the Git repository. The wrapper
uses a temporary CSV and deletes it when the pipeline exits. Keep the output directory
on the bastion host in an access-controlled location.
