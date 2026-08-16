# Variable operationalization

Study design: cross-sectional secondary analysis of a nationwide multicenter nurse survey.

## Population

- Start from the fixed 64,114-row all-text TARGET source database.
- Reproduce the prespecified parent-cohort cleaning before department selection: review coded-ID duplicates and core-scale missingness; sequentially exclude work experience below 1 year, estimated nursing-entry age below 16 years, and total response time below 600 seconds.
- The expected cleaned parent cohort is 60,838 nurses. Then include respondents whose department variable `A_q9` equals `7` (operating room), yielding the fixed analysis sample of 3,744 nurses.
- Submission year is parsed from `submittime`. The similarly named suffixed fields are merged-module timestamps and are not substituted for the final submission time.

## Derived demographic and quality-control variables

- Submission year: year parsed from `submittime`.
- Age: submission year minus birth year (`A_year`); values outside 16-65 years are set to missing.
- Nursing work duration: submission year minus nursing start year (`work_y`).
- Estimated nursing-entry age: `work_y - A_year`.
- Total response time: row sum of all four `timetaken*` module-duration fields, retaining missing only when all four fields are missing.
- BMI: weight (`A_q16`) divided by squared height (`A_q15`) in metres after setting height outside 140-210 cm and weight outside 35-120 kg to missing.

## Primary latent-class and network indicators

Nine binary Nordic Musculoskeletal Questionnaire indicators for symptoms during the previous 12 months:

- Neck: `E_q24_1_1`
- Shoulder: `E_q24_2_1`
- Upper back: `E_q24_3_1`
- Elbow: `E_q24_4_1`
- Wrist/hand: `E_q24_5_1`
- Lower back: `E_q24_6_1`
- Hip/thigh: `E_q24_7_1`
- Knee: `E_q24_8_1`
- Ankle/foot: `E_q24_9_1`

Source coding is `1 = no`, `2 = yes`; analysis coding is `0 = no`, `1 = yes`.
The scale dictionary labels these items as `B_q24_*`, whereas the data workbook stores them as `E_q24_*`; this audited prefix crosswalk is applied explicitly.

## Secondary symptom definitions

- Previous-12-month activity limitation: suffix `_2` for each site.
- Previous-12-month health-care use: suffix `_3` for each site.
- Previous-7-day symptom: suffix `_4` for each site.
- Multisite symptom burden: sum of the nine primary indicators (range 0-9).

## Work-related factors

- Shift pattern: `C_q1`.
- Years of night-shift work: `C_q6`.
- Months with night shifts during the previous year: `C_q7`.
- Average night shifts per month: `C_q8`.
- Weeks exceeding 40 work hours during the previous month: `C_q42`.
- Work time overlapping usual sleep time: `C_q44`.
- Perceived probability/severity or derived risk score for posture/lifting (`E_q23_6_*`), musculoskeletal load and poor facility design (`E_q23_19_*`), repetitive work (`E_q23_20_*`), prolonged standing (`E_q23_21_*`), shift/night/overtime work (`E_q23_22_*`), and understaffing/work overload (`E_q23_24_*`).

The `E_q23_*` variables are treated as perceived occupational-hazard ratings, not objective exposure measurements. Cross-sectional associations are not interpreted causally.

## Covariates

Derived age, sex (`A_q2`), derived body mass index, derived years worked, education (`A_q5`), marital status (`A_q6`), employment type (`A_q10`), professional title (`A_q11`), administrative role (`A_q12`), and income (`A_q13`). Covariate inclusion is based on design and subject-matter relevance rather than significance screening alone.

## Missing data

- Report missingness for every analysis variable.
- Primary latent class analysis uses complete symptom-indicator records because the indicators are binary and the expected missingness is audited before modeling.
- Regression uses complete cases if total model-variable missingness is at most 5%; otherwise multiple imputation is considered and reported.
