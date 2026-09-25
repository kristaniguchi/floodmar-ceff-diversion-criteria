####### Scott River Recharge Metric Analysis ######
### Developed by Environmental Science Associates
### Developed for DWR under TOS-ESA-4600015545
### Code and supporting files provided "as is"
### Dated: 8/2026

library(dplyr)
library(lubridate)

scott_wide <- read.csv("Scott_Available.csv") |>
  mutate(date = mdy(date))

# reshape to long format using tidyr package
scott <- tidyr::pivot_longer(
  scott_wide,
  cols = !c(date, discharge_cfs, water_year),
  names_to = "scenario",
  values_to = "divert_cfs"
) |>
  mutate(
    divert_af = divert_cfs * 1.983,
    # replace the X's at the start of the column names (added by read.csv) with the empty string
    scenario = sub("^X", "", scenario),
    # finish cleaning up scenario names
    scenario = case_when(
      scenario == "90.20" ~ "90/20",
      scenario == "80.50" ~ "80/50",
      scenario == "362..Ecohydraulic." ~ "362 (Ecohydraulic)",
      .default = scenario
    )
  ) |>
  arrange(scenario, date)

# ---- 1. Annual volume (AF) per water year ----

scott_annual <- scott |>
  group_by(scenario, water_year) |>
  summarise(divert_af = sum(divert_af, na.rm = TRUE), .groups = "drop")

# ---- 2. Run-length stats per scenario and water year ----

scott_runs <- scott |>
  group_by(scenario) |>
  mutate(
    is_div = divert_cfs > 0,
    run_id = cumsum(is_div & !lag(is_div, default = FALSE)),
    run_id = ifelse(is_div, run_id, NA_integer_)
  ) |>
  filter(is_div) |>
  group_by(scenario, run_id) |>
  summarise(
    water_year = first(water_year),
    run_duration = n(),
    .groups = "drop"
  ) |>
  group_by(scenario, water_year) |>
  summarise(
    n_runs = n(),
    avg_run_dur = mean(run_duration),
    .groups = "drop"
  )

scott_div_days <- scott |>
  group_by(scenario, water_year) |>
  summarise(n_div_days = sum(divert_cfs > 0), .groups = "drop")

# ---- 3. Metrics table ----

n_years <- n_distinct(scott$water_year)

scott_div_rate <- scott |>
  filter(divert_cfs > 0) |>
  group_by(scenario) |>
  summarise(avg_div_rate_cfs = mean(divert_cfs), .groups = "drop")

scott_events <- scott_div_days |>
  left_join(scott_runs, by = c("scenario", "water_year")) |>
  # years with no diversion days will have missing values for n_runs and avg_run_dur that we replace with zero
  mutate(across(c(n_runs, avg_run_dur), ~ ifelse(is.na(.x), 0, .x))) |>
  group_by(scenario) |>
  summarise(
    avg_div_days_div_yrs = mean(n_div_days[n_div_days > 0]),
    avg_events_div_yrs = mean(n_runs[n_runs > 0]),
    avg_event_dur_div_yrs = mean(avg_run_dur[n_runs > 0]),
    .groups = "drop"
  )

metrics <- scott_annual |>
  group_by(scenario) |>
  summarise(
    avg_annual_taf = mean(divert_af) / 1000,
    median_annual_taf = median(divert_af) / 1000,
    max_annual_taf = max(divert_af) / 1000,
    avg_taf_div_yrs = mean(divert_af[divert_af > 0]) / 1000,
    sim_years = n_years,
    n_zero_yrs = sum(divert_af == 0),
    n_div_yrs = sum(divert_af > 0),
    aep_poisson = 1 - exp(-sum(divert_af == 0) / n_years),
    empirical_rp = n_years / sum(divert_af == 0),
    poisson_rp = 1 / (1 - exp(-sum(divert_af == 0) / n_years)),
    .groups = "drop"
  ) |>
  left_join(scott_div_rate, by = "scenario") |>
  left_join(scott_events, by = "scenario") |>
  mutate(
    normalized_avg = avg_annual_taf / avg_annual_taf[scenario == "90/20"],
    pct_chg_from_ref = (normalized_avg - 1) * 100,
  ) |>
  # change orientation to match Excel version (but columns and rows not in same order)
  tidyr::pivot_longer(
    cols = !scenario,
    names_to = "recharge_metric",
    values_to = "value"
  ) |>
  tidyr::pivot_wider(names_from = scenario, values_from = value)
