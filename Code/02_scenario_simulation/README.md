# 02_scenario_simulation

Scenario simulation and recharge metrics calculation, contributed by
Jason Wiener (Environmental Science Associates). These files are provided
as delivered and have not been modified.

## Before running

Each script contains a hardcoded `setwd()` path pointing to Jason's own
local directory. Update this to your local project directory before running
any script in this folder.

## Known gaps

- **The three `*_Available.csv` files (Elder_Available.csv, Scott_Available.csv,
  Stony_Available.csv) were manually post-processed in Excel from the output
  of Jessi Ayers's FF scenario definition code** (see `01_ff_scenario_definition/`),
  to prepare it for reading into these scripts. The specific steps taken during
  this manual post-processing were not documented and are not captured in any
  script. Reproducing this step directly from Jessi's raw output may require
  reconstructing this process independently.

## Files

- `FloodMAR_Diversion_Functions_V2_TMH.R` — functions to run 90/20 and
  Ecohydraulic scenarios
- `Scott_Div_Analysis_TMH.R` — runs 90/20 and Ecohydraulic scenarios for
  Scott River
- `Stony_Elder_90_20.R` — runs the 90/20 scenario for Elder Creek and Stony
  Creek
- `Elder_Scott_Stony_80_50.R` — runs the 80/50 scenario for Elder Creek,
  Stony Creek, and Scott River
- `Scott_recharge_metrics.R` — calculates recharge metrics for Scott River
- `Stony_Elder_recharge_metrics.R` — calculates recharge metrics for Elder
  Creek and Stony Creek