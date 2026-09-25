# ==============================================================================
# Cross-Site Comparison — Peak 2-Year Duration & Frequency
#
# Combines Elder Creek, Scott River, and Stony Creek into a single figure,
# faceted by site (columns) and metric (rows), to show that 90/20 and 80/50
# compress Peak 2-yr duration/frequency relative to natural variability at all
# three sites, while FF scenarios track Baseline and the natural range more
# closely. This is the cross-site version of the standalone
# "boxplot_Peak_2yr_Dur_Fre_slide.png" figure produced by each site's own
# FFM plots script.
#
# SCENARIO LABEL HARMONIZATION
# Elder and Stony use "FF-BF50"/"FF-BF75" (baseflow-threshold naming); Scott
# uses "FF-WS50"/"FF-WS75" (wet-season-threshold naming). These represent the
# same conceptual scenario (wet-season baseflow percentile threshold, 20%
# daily cap) at each site, so they are relabeled here as "FF-50"/"FF-75" for
# a consistent x-axis across facets. Harmonization is done by scenario_code
# (unambiguous per site), not by the original label text. Ecohydraulic exists
# only at Scott River and will simply be absent from the Elder/Stony facets.
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "scales"))
#   Requires outputs from each site's own *_FFC_analysis.R:
#     ./Data/outputs/Elder_Creek/FFM/results/elder_ffm_patched.csv
#     ./Data/outputs/Elder_Creek/FFM/results/elder_percentiles.csv
#     ./Data/outputs/Scott_River/FFM/results/scott_ffm_patched.csv
#     ./Data/outputs/Scott_River/FFM/results/scott_percentiles.csv
#     ./Data/outputs/Stony_Creek/FFM/results/stony_ffm_patched.csv
#     ./Data/outputs/Stony_Creek/FFM/results/stony_percentiles.csv
# ==============================================================================

library(tidyverse)
library(scales)

#setwd("C:/Users/adrianal/SCCWRP/CEFF - FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
#setwd("C:/Users/kristinet/SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
setwd("/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

site_config <- tribble(
  ~site,           ~results_dir,                                  ~ffm_file,               ~pct_file,               ~period_start,  ~period_end,
  "Scott River",   "./Data/outputs/Scott_River/FFM/results",     "scott_ffm_patched.csv",  "scott_percentiles.csv", 1950L,          2014L,
  "Stony Creek",   "./Data/outputs/Stony_Creek/FFM/results",     "stony_ffm_patched.csv",  "stony_percentiles.csv", NA_integer_,    NA_integer_,
  "Elder Creek",   "./Data/outputs/Elder_Creek/FFM/results",     "elder_ffm_patched.csv",  "elder_percentiles.csv", 1950L,          2014L
)

site_levels <- c("Scott River", "Stony Creek", "Elder Creek")

output_dir <- "./Data/outputs/Cross_Site_Comparisons"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Harmonized scenario labels — mapped by scenario_code, not original label text
scenario_code_map <- c(
  "Baseline"     = "Baseline",
  "90_20"        = "90/20",
  "80_50"        = "80/50",
  "Ecohydraulic" = "Ecohydraulic",
  "FF_BF50"      = "FF-50",
  "FF_WS50"      = "FF-50",
  "FF_BF75"      = "FF-75",
  "FF_WS75"      = "FF-75",
  "FF_Pk2"       = "FF-Pk2",
  "FF_PEAK2"     = "FF-Pk2"
)

scenario_levels <- c("Baseline", "90/20", "80/50", "Ecohydraulic", "FF-50", "FF-75", "FF-Pk2")

scenario_colors <- c(
  "Baseline"     = "grey40",
  "90/20"        = "#E41A1C",
  "80/50"        = "#D4A017",
  "Ecohydraulic" = "#29B6F6",
  "FF-50"        = "#00E676",
  "FF-75"        = "#CE93D8",
  "FF-Pk2"       = "#F06292"
)

metric_levels <- c("Peak_Dur_2", "Peak_Fre_2")
metric_labels <- c(
  Peak_Dur_2 = "Peak 2-yr Duration (days)",
  Peak_Fre_2 = "Peak 2-yr Frequency"
)

# ==============================================================================
# LOAD & COMBINE — annual FFM values across all three sites
# ==============================================================================

message("Loading FFM results for all three sites...")

ffm_all <- pmap_dfr(site_config, function(site, results_dir, ffm_file, pct_file, period_start, period_end) {
  df <- read_csv(file.path(results_dir, ffm_file), show_col_types = FALSE) %>%
    filter(!Year %in% c("Avg", "Std", "CV")) %>%
    mutate(
      Year = as.integer(Year),
      Peak_Dur_2 = suppressWarnings(as.numeric(Peak_Dur_2)),
      Peak_Fre_2 = suppressWarnings(as.numeric(Peak_Fre_2)),
      site = site
    ) %>%
    select(site, Year, WYT, scenario_code, Peak_Dur_2, Peak_Fre_2)

  # Per-site period-of-record filter — matches each site's own FFM plots
  # script. Elder Creek and Scott River are constrained to 1950-2014 to match
  # the NFD calibration period. Stony Creek's own script uses NO year filter
  # (full available period of record, since its record doesn't extend back to
  # 1950) — period_start/period_end are NA for Stony Creek in site_config,
  # so no filter is applied here either, keeping this consistent with
  # Stony_Creek_FFM_plots.R rather than silently truncating Stony's data.
  if (!is.na(period_start) && !is.na(period_end)) {
    df <- df %>% filter(Year >= period_start, Year <= period_end)
  }

  df
})

message("  Combined FFM rows: ", nrow(ffm_all))

# Diagnostic: confirm each site's actual loaded year range — Stony Creek
# should show its full available record (not truncated to 2014), Scott River
# and Elder Creek should show 1950-2014.
ffm_all %>%
  group_by(site) %>%
  summarise(min_year = min(Year, na.rm = TRUE),
            max_year = max(Year, na.rm = TRUE),
            n_years  = n_distinct(Year), .groups = "drop") %>%
  pwalk(function(site, min_year, max_year, n_years) {
    message("  ", site, ": years ", min_year, "-", max_year, " (", n_years, " unique years)")
  })

ffm_long <- ffm_all %>%
  filter(WYT %in% c("wet", "moderate", "dry")) %>%
  mutate(across(c(Peak_Dur_2, Peak_Fre_2), ~ replace_na(.x, 0))) %>%
  pivot_longer(cols = c(Peak_Dur_2, Peak_Fre_2),
               names_to = "metric", values_to = "value") %>%
  mutate(
    scenario_label = recode(scenario_code, !!!scenario_code_map),
    scenario_label = factor(scenario_label, levels = scenario_levels),
    metric         = factor(metric, levels = metric_levels),
    metric_label   = factor(metric_labels[as.character(metric)], levels = metric_labels[metric_levels]),
    site           = factor(site, levels = site_levels)
  ) %>%
  filter(!is.na(scenario_label))

# ==============================================================================
# LOAD & COMBINE — natural flow reference range (per site)
# ==============================================================================

message("Loading natural flow percentiles for all three sites...")

pct_all <- pmap_dfr(site_config, function(site, results_dir, ffm_file, pct_file, ...) {
  read_csv(file.path(results_dir, pct_file), show_col_types = FALSE) %>%
    mutate(site = site)
})

pct_cols <- c("p10_predicted", "p25_predicted", "p50_predicted",
              "p75_predicted", "p90_predicted")
pct_cols_present <- intersect(pct_cols, names(pct_all))

natural_range <- pct_all %>%
  filter(scenario_code == "Baseline",
         WYT     == "any",
         metric  %in% metric_levels) %>%
  select(site, metric, all_of(pct_cols_present)) %>%
  rename_with(~ str_remove(.x, "_predicted"), all_of(pct_cols_present)) %>%
  mutate(
    across(any_of(c("p10","p25","p50","p75","p90")), as.numeric),
    metric_label = factor(metric_labels[metric], levels = metric_labels[metric_levels]),
    site         = factor(site, levels = site_levels)
  ) %>%
  filter(!is.na(p10), !is.na(p90))

has_nat_range <- nrow(natural_range) > 0

# ==============================================================================
# PLOT THEME
# ==============================================================================

theme_ffm <- theme_minimal(base_size = 24) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey92", linewidth = 0.3),
    axis.title        = element_text(size = 24, face = "bold"),
    axis.text.x       = element_text(size = 20, angle = 35, hjust = 1),
    axis.text.y       = element_text(size = 20),
    strip.text        = element_text(size = 21, face = "bold", margin = margin(t = 8, b = 8)),
    strip.background  = element_rect(fill = "grey95", color = NA),
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold", size = 22),
    legend.text       = element_text(size = 21),
    legend.key.size   = unit(1.4, "cm"),
    plot.margin       = margin(10, 10, 10, 10)
  )

# ==============================================================================
# PLOT — faceted grid: metric (rows) x site (columns)
# ==============================================================================

message("Generating cross-site Peak 2-yr Duration & Frequency comparison...")

p <- ggplot(ffm_long, aes(x = scenario_label, y = value, fill = scenario_label))

if (has_nat_range) {
  p <- p +
    geom_rect(data        = natural_range,
              aes(ymin = p10, ymax = p90, group = interaction(metric_label, site)),
              xmin        = -Inf, xmax = Inf,
              fill        = "#B3E5FC", alpha = 0.35,
              inherit.aes = FALSE) +
    geom_rect(data        = natural_range,
              aes(ymin = p25, ymax = p75, group = interaction(metric_label, site)),
              xmin        = -Inf, xmax = Inf,
              fill        = "#0288D1", alpha = 0.20,
              inherit.aes = FALSE) +
    geom_hline(data        = natural_range,
               aes(yintercept = p50, group = interaction(metric_label, site)),
               color          = "#0288D1",
               linewidth      = 0.7,
               linetype       = "dashed",
               inherit.aes    = FALSE)
}

p <- p +
  geom_boxplot(width = 0.6, outlier.size = 0.8,
               outlier.alpha = 0.6,
               color = "grey20", linewidth = 0.4) +

  scale_fill_manual(name   = "Scenario",
                    values = scenario_colors,
                    drop   = FALSE) +

  facet_wrap(vars(metric_label, site), nrow = 2, scales = "free_y") +

  labs(x = NULL, y = "Metric Value") +

  theme_ffm

fname <- file.path(output_dir, "Cross_Site_Peak2yr_Dur_Fre_comparison.png")
ggsave(fname, p, width = 22, height = 11, dpi = 300)
message("  Saved: ", fname)

message("\nDone.")
