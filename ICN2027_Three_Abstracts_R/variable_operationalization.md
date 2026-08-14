# Variable operationalization

## Abstract 1: Workplace negative acts and emotional exhaustion

- Exposure: `F_fuxingxingwei_all`, the sum of `F_q1_1` to `F_q1_22` (22-110; higher means more frequent negative acts).
- Binary exposure: 22 means no reported negative act; greater than 22 means at least one negative act.
- Outcome: `F_qingganshuaijie`, the sum of nine emotional-exhaustion items (0-54; higher means greater exhaustion).
- Covariates: age per 10 years, sex, education, department, and employment type.
- Models: HC3-robust linear regression for any versus no negative acts, plus a dose model among exposed nurses per 10-point higher negative-acts score.

## Abstract 2: Menopause at Work

- Population: female nurses passing the reproductive-health module routing/exclusions with valid reproductive stage and all 13 modified Kupperman items.
- Exposure: `B_weijuejingqizonghezheng_all`, the weighted modified Kupperman Index (0-63).
- Outcome: corrected SPS-6 productivity-loss score, calculated as the direct sum of `D_q21_1` to `D_q21_6`.
- Important scoring rule: `D_q21_5` and `D_q21_6` already contain the data-dictionary-assigned reverse scores. Do not apply `6-x` again.
- Outcome range: 6-30; higher scores indicate greater health-related productivity loss. All six items are required; there is no imputation or prorating.
- Covariates: age per 10 years, marital status, department, employment type, and night-shift frequency.
- Model: four-knot restricted cubic spline at the 5th, 35th, 65th, and 95th percentiles with HC3 robust covariance; contrasts at Kupperman 6, 16, and 31 versus 0.

## Abstract 3: Work-family balance and turnover intention

- Exposure: `C_gzjt_all`, a direction-harmonised sum of 14 work-family items (14-70; higher means better balance).
- Direction harmonisation: apply `6-x` to `C_q50_1`-`C_q50_4` and `C_q50_8`-`C_q50_11`; retain the other six items as stored.
- Outcome: `C_lizhiyiyuan_all`, turnover-intention total (6-24; higher means stronger intention to leave).
- Covariates: age per 10 years, sex, marital status, department, and employment type.
- Model: four-knot restricted cubic spline at the 5th, 35th, 65th, and 95th percentiles with HC3 robust covariance; contrasts at balance scores 44, 50, and 57 versus 40.
