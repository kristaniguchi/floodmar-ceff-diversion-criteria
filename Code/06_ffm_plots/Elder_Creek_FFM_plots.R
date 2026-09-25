# ==============================================================================
# Elder Creek — FFM Visualization (standalone)
# COMID: 12068798
#
# TWO plot types:
#
#   1. BOXPLOTS by flow component
#      Per-year scenario distributions overlaid on natural flow predicted range
#      (p10-p90 ribbon, p25-p75 shading, p50 line from API percentiles).
#      One PNG per flow component, faceted by metric.
#
#   2. RANGE COMPRESSION
#      IQR (25th-75th percentile) width per scenario as % of natural range IQR.
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "lubridate", "scales"))
#   Requires outputs from Elder_Creek_FFC_analysis.R:
#     ./Data/outputs/Elder_Creek/FFM/results/elder_ffm_patched.csv
#     ./Data/outputs/Elder_Creek/FFM/results/elder_percentiles.csv
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)

#setwd("C:/Users/adrianal/SCCWRP/CEFF - FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
#setwd("C:/Users/kristinet/SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
setwd("/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

results_dir <- "./Data/outputs/Elder_Creek/FFM/results"
boxplot_dir <- "./Data/outputs/Elder_Creek/boxplots"

dir.create(boxplot_dir, showWarnings = FALSE, recursive = TRUE)

# Scenario factor order and display labels for x-axis
scenario_levels <- c("Baseline", "90_20", "80_50", "FF_BF50", "FF_BF75", "FF_Pk2")
scenario_labels <- c("Baseline", "90/20", "80/50", "FF-WS50", "FF-WS75", "FF-Pk2")

# Scenario colors
scenario_colors <- c(
  "Baseline" = "grey40",
  "90_20"    = "#E41A1C",
  "80_50"    = "#D4A017",
  "FF_BF50"  = "#00E676",
  "FF_BF75"  = "#CE93D8",
  "FF_Pk2"   = "#F06292"
)

# Metrics to include, grouped by flow component
metric_groups <- list(
  "Fall Pulse Flow" = c("FA_Mag", "FA_Tim", "FA_Dur"),
  "Wet-Season Baseflow" = c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50",
                             "Wet_BFL_Dur",    "Wet_Tim"),
  "Peak Flow" = c("Peak_2",     "Peak_Dur_2", "Peak_Fre_2",
                  "Peak_5",     "Peak_Dur_5", "Peak_Fre_5",
                  "Peak_10",    "Peak_Dur_10","Peak_Fre_10"),
  "Spring Recession" = c("SP_Mag", "SP_Tim", "SP_Dur", "SP_ROC"),
  "Dry-Season Baseflow" = c("DS_Mag_50", "DS_Mag_90", "DS_Dur_WS", "DS_Tim")
)

# Metric display labels
metric_labels <- c(
  FA_Mag         = "Fall Pulse\nMagnitude (cfs)",
  FA_Tim         = "Fall Pulse\nTiming (dowy)",
  FA_Dur         = "Fall Pulse\nDuration (days)",
  Wet_BFL_Mag_10 = "Wet-Season BF\nMag 10th pct (cfs)",
  Wet_BFL_Mag_50 = "Wet-Season BF\nMag 50th pct (cfs)",
  Wet_BFL_Dur    = "Wet-Season BF\nDuration (days)",
  Wet_Tim        = "Wet-Season\nTiming (dowy)",
  Peak_2         = "Peak 2-yr\nMagnitude (cfs)",
  Peak_5         = "Peak 5-yr\nMagnitude (cfs)",
  Peak_10        = "Peak 10-yr\nMagnitude (cfs)",
  Peak_Dur_2     = "Peak 2-yr\nDuration (days)",
  Peak_Dur_5     = "Peak 5-yr\nDuration (days)",
  Peak_Dur_10    = "Peak 10-yr\nDuration (days)",
  Peak_Fre_2     = "Peak 2-yr\nFrequency",
  Peak_Fre_5     = "Peak 5-yr\nFrequency",
  Peak_Fre_10    = "Peak 10-yr\nFrequency",
  SP_Mag         = "Spring Recession\nMagnitude (cfs)",
  SP_Tim         = "Spring Recession\nTiming (dowy)",
  SP_Dur         = "Spring Recession\nDuration (days)",
  SP_ROC         = "Spring Recession\nRate of Change",
  DS_Mag_50      = "Dry-Season BF\nMag 50th pct (cfs)",
  DS_Mag_90      = "Dry-Season BF\nMag 90th pct (cfs)",
  DS_Dur_WS      = "Dry-Season BF\nDuration (days)",
  DS_Tim         = "Dry-Season BF\nTiming (dowy)"
)

# ==============================================================================
# LOAD DATA
# ==============================================================================

message("Loading FFM results...")

ffm_raw <- read_csv(file.path(results_dir, "elder_ffm_patched.csv"),
                    show_col_types = FALSE) %>%
  filter(!Year %in% c("Avg", "Std", "CV")) %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(across(-c(Year, WYT, Overall_Int_Class, Int_Class, scenario_code, scenario_label),
                ~ suppressWarnings(as.numeric(.x)))) %>%
  mutate(across(matches("^Peak_Dur_|^Peak_Fre_"), ~ replace_na(.x, 0)))

pct_raw <- read_csv(file.path(results_dir, "elder_percentiles.csv"),
                    show_col_types = FALSE)

message("  FFM rows: ", nrow(ffm_raw), "  |  Percentile rows: ", nrow(pct_raw))

# ==============================================================================
# WRANGLE
# ==============================================================================

all_metrics <- unlist(metric_groups, use.names = FALSE)

nfd_start <- 1950L
nfd_end   <- 2014L

ffm_long <- ffm_raw %>%
  filter(WYT %in% c("wet","moderate","dry"),
         Year >= nfd_start, Year <= nfd_end,
         scenario_code %in% scenario_levels) %>%
  select(Year, WYT, scenario_code, scenario_label,
         all_of(intersect(all_metrics, names(ffm_raw)))) %>%
  pivot_longer(cols      = -c(Year, WYT, scenario_code, scenario_label),
               names_to  = "metric",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    scenario_code = factor(scenario_code, levels = scenario_levels, labels = scenario_labels),
    metric        = factor(metric, levels = all_metrics),
    metric_label  = factor(metric_labels[as.character(metric)],
                           levels = metric_labels[all_metrics])
  )

message("  Years with valid WYT: ", length(unique(ffm_long$Year)),
        " (", min(ffm_long$Year), "-", max(ffm_long$Year), ")")

# Natural flow reference range from API percentiles
pct_cols <- c("p10_predicted", "p25_predicted", "p50_predicted",
              "p75_predicted", "p90_predicted")
pct_cols_present <- intersect(pct_cols, names(pct_raw))

if (length(pct_cols_present) == 0)
  stop("No predicted percentile columns found in elder_percentiles.csv. ",
       "Expected: ", paste(pct_cols, collapse = ", "),
       "\nFound: ", paste(names(pct_raw), collapse = ", "))

natural_range <- pct_raw %>%
  filter(scenario_code == "Baseline",
         WYT           %in% c("any", "wet", "moderate", "dry"),
         metric        %in% all_metrics) %>%
  select(metric, WYT, all_of(pct_cols_present)) %>%
  rename_with(~ str_remove(.x, "_predicted"), all_of(pct_cols_present)) %>%
  mutate(across(any_of(c("p10","p25","p50","p75","p90")), as.numeric))

# ==============================================================================
# PLOT THEME
# ==============================================================================

theme_ffm <- theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey92", linewidth = 0.3),
    axis.title        = element_text(size = 12, face = "bold"),
    axis.text.x       = element_text(size = 10, angle = 35, hjust = 1),
    axis.text.y       = element_text(size = 10),
    strip.text        = element_text(size = 11, face = "bold"),
    strip.background  = element_rect(fill = "grey95", color = NA),
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 10),
    plot.title        = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5, color = "grey40"),
    plot.margin       = margin(10, 10, 10, 10)
  )

# ==============================================================================
# PLOT 1 — BOXPLOTS by flow component
# ==============================================================================

message("\nGenerating boxplots...")

for (component in names(metric_groups)) {

  metrics_in_comp <- metric_groups[[component]]

  metrics_avail <- metrics_in_comp[
    metrics_in_comp %in% unique(ffm_long$metric) &
    sapply(metrics_in_comp, function(m) {
      sum(!is.na(ffm_long$value[ffm_long$metric == m])) > 0
    })
  ]

  if (length(metrics_avail) == 0) {
    message("  Skipping ", component, " — no data")
    next
  }

  df_comp <- ffm_long %>%
    filter(metric %in% metrics_avail) %>%
    mutate(metric_label = factor(metric_labels[as.character(metric)],
                                 levels = metric_labels[metrics_avail]))

  nat_comp <- natural_range %>%
    filter(metric %in% metrics_avail, WYT == "any") %>%
    mutate(
      metric_label = factor(metric_labels[metric], levels = metric_labels[metrics_avail]),
      p10 = as.numeric(p10), p25 = as.numeric(p25),
      p50 = as.numeric(p50), p75 = as.numeric(p75),
      p90 = as.numeric(p90)
    ) %>%
    filter(!is.na(p10), !is.na(p90))

  has_nat_range <- nrow(nat_comp) > 0

  p <- ggplot(df_comp, aes(x = scenario_code, y = value, fill = scenario_code))

  if (has_nat_range) {
    p <- p +
      geom_rect(data        = nat_comp,
                aes(ymin    = p10, ymax = p90, group = metric_label),
                xmin        = -Inf, xmax = Inf,
                fill        = "#B3E5FC", alpha = 0.35,
                inherit.aes = FALSE) +
      geom_rect(data        = nat_comp,
                aes(ymin    = p25, ymax = p75, group = metric_label),
                xmin        = -Inf, xmax = Inf,
                fill        = "#0288D1", alpha = 0.20,
                inherit.aes = FALSE) +
      geom_hline(data        = nat_comp,
                 aes(yintercept = p50, group = metric_label),
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
                      values = setNames(scenario_colors, scenario_labels),
                      drop   = FALSE) +

    facet_wrap(~ metric_label, scales = "free_y", ncol = 3) +

    labs(
      title    = paste0("Elder Creek — ", component),
      subtitle = if (has_nat_range)
        "Boxplots: per-year scenario values  |  Blue bands: natural flow p10-p90 (light) and p25-p75 (dark)  |  Blue dashed: natural p50"
      else
        "Boxplots: per-year scenario values  |  No natural flow predictions available for this component",
      x = NULL, y = "Metric Value"
    ) +

    theme_ffm

  comp_safe <- str_replace_all(component, "[^A-Za-z0-9]", "_")
  n_cols    <- min(3L, length(metrics_avail))
  n_rows    <- ceiling(length(metrics_avail) / n_cols)
  fig_h     <- max(5, n_rows * 3.5 + 2)

  fname <- file.path(boxplot_dir, paste0("boxplot_", comp_safe, ".png"))
  ggsave(fname, p, width = 14, height = fig_h, dpi = 300)
  message("  Saved: ", basename(fname),
          if (!has_nat_range) " (no natural range bands)" else "")
}

# ==============================================================================
# PLOT 1B — Peak 2-year Duration and Frequency — standalone slide figure
# ==============================================================================

message("\nGenerating Peak 2-yr Duration & Frequency slide figure...")

peak2_metrics <- c("Peak_Dur_2", "Peak_Fre_2")

df_peak2  <- ffm_long %>%
  filter(metric %in% peak2_metrics) %>%
  mutate(metric_label = factor(metric_labels[as.character(metric)],
                               levels = metric_labels[peak2_metrics]))

nat_peak2 <- natural_range %>%
  filter(metric %in% peak2_metrics, WYT == "any") %>%
  mutate(
    metric_label = factor(metric_labels[metric], levels = metric_labels[peak2_metrics]),
    p10 = as.numeric(p10), p25 = as.numeric(p25),
    p50 = as.numeric(p50), p75 = as.numeric(p75),
    p90 = as.numeric(p90)
  ) %>%
  filter(!is.na(p10), !is.na(p90))

has_nat_peak2 <- nrow(nat_peak2) > 0

p_peak2 <- ggplot(df_peak2, aes(x = scenario_code, y = value, fill = scenario_code))

if (has_nat_peak2) {
  p_peak2 <- p_peak2 +
    geom_rect(data        = nat_peak2,
              aes(ymin = p10, ymax = p90, group = metric_label),
              xmin        = -Inf, xmax = Inf,
              fill        = "#B3E5FC", alpha = 0.35,
              inherit.aes = FALSE) +
    geom_rect(data        = nat_peak2,
              aes(ymin = p25, ymax = p75, group = metric_label),
              xmin        = -Inf, xmax = Inf,
              fill        = "#0288D1", alpha = 0.20,
              inherit.aes = FALSE) +
    geom_hline(data        = nat_peak2,
               aes(yintercept = p50, group = metric_label),
               color          = "#0288D1", linewidth = 0.7,
               linetype       = "dashed",
               inherit.aes    = FALSE)
}

p_peak2 <- p_peak2 +
  geom_boxplot(width = 0.65, outlier.size = 1.0,
               outlier.alpha = 0.6,
               color = "grey20", linewidth = 0.4) +
  scale_fill_manual(name   = "Scenario",
                    values = setNames(scenario_colors, scenario_labels),
                    drop   = FALSE) +
  facet_wrap(~ metric_label, scales = "free_y", ncol = 2) +
  labs(
    title    = "Elder Creek — Peak 2-Year: Duration and Frequency",
    subtitle = "Blue bands: natural flow p10-p90 (light) and p25-p75 (dark)  |  Blue dashed: natural p50",
    x = NULL, y = "Metric Value"
  ) +
  theme_ffm +
  theme(axis.text.x = element_text(size = 11, angle = 35, hjust = 1))

ggsave(file.path(boxplot_dir, "boxplot_Peak_2yr_Dur_Fre_slide.png"),
       p_peak2, width = 12, height = 6, dpi = 300)
message("  Saved: boxplot_Peak_2yr_Dur_Fre_slide.png")

# ==============================================================================
# PLOT 1C — Wet-Season Baseflow Median — standalone slide figure
# ==============================================================================

message("\nGenerating Wet-Season Baseflow Median slide figure...")

wsmedian_metric <- "Wet_BFL_Mag_50"

df_wsmedian <- ffm_long %>%
  filter(metric == wsmedian_metric)

nat_wsmedian <- natural_range %>%
  filter(metric == wsmedian_metric, WYT == "any") %>%
  mutate(
    p10 = as.numeric(p10), p25 = as.numeric(p25),
    p50 = as.numeric(p50), p75 = as.numeric(p75),
    p90 = as.numeric(p90)
  ) %>%
  filter(!is.na(p10), !is.na(p90))

has_nat_wsmedian <- nrow(nat_wsmedian) > 0

p_wsmedian <- ggplot(df_wsmedian, aes(x = scenario_code, y = value, fill = scenario_code))

if (has_nat_wsmedian) {
  p_wsmedian <- p_wsmedian +
    geom_rect(data        = nat_wsmedian,
              aes(ymin = p10, ymax = p90),
              xmin        = -Inf, xmax = Inf,
              fill        = "#B3E5FC", alpha = 0.35,
              inherit.aes = FALSE) +
    geom_rect(data        = nat_wsmedian,
              aes(ymin = p25, ymax = p75),
              xmin        = -Inf, xmax = Inf,
              fill        = "#0288D1", alpha = 0.20,
              inherit.aes = FALSE) +
    geom_hline(data        = nat_wsmedian,
               aes(yintercept = p50),
               color          = "#0288D1", linewidth = 0.7,
               linetype       = "dashed",
               inherit.aes    = FALSE)
}

p_wsmedian <- p_wsmedian +
  geom_boxplot(width = 0.55, outlier.size = 1.0,
               outlier.alpha = 0.6,
               color = "grey20", linewidth = 0.4) +
  scale_fill_manual(name   = "Scenario",
                    values = setNames(scenario_colors, scenario_labels),
                    drop   = FALSE) +
  labs(
    title    = "Elder Creek — Wet-Season Baseflow Median",
    subtitle = "Blue bands: natural flow p10-p90 (light) and p25-p75 (dark)  |  Blue dashed: natural p50",
    x = NULL, y = "Wet-Season BF Mag 50th pct (cfs)"
  ) +
  theme_ffm +
  theme(axis.text.x = element_text(size = 11, angle = 35, hjust = 1))

ggsave(file.path(boxplot_dir, "boxplot_Wet_Season_BF_Median_slide.png"),
       p_wsmedian, width = 9, height = 6, dpi = 300)
message("  Saved: boxplot_Wet_Season_BF_Median_slide.png")

# ==============================================================================
# PLOT 2 — RANGE COMPRESSION
# ==============================================================================

message("\nGenerating range compression plot...")

key_metrics <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Wet_BFL_Dur",
                 "Peak_2", "Peak_5", "Peak_10",
                 "Peak_Dur_2", "Peak_Dur_5", "Peak_Dur_10",
                 "SP_Mag", "SP_Tim")

iqr_scenario <- ffm_long %>%
  filter(metric %in% key_metrics) %>%
  group_by(scenario_code, metric) %>%
  summarise(q25 = quantile(value, 0.25, na.rm = TRUE),
            q75 = quantile(value, 0.75, na.rm = TRUE),
            iqr  = q75 - q25,
            .groups = "drop")

nat_iqr <- natural_range %>%
  filter(metric %in% key_metrics, WYT == "any") %>%
  mutate(nat_iqr = p75 - p25) %>%
  select(metric, nat_iqr, nat_p50 = p50)

compression <- iqr_scenario %>%
  left_join(nat_iqr, by = "metric") %>%
  mutate(
    iqr_pct_of_natural = iqr / nat_iqr * 100,
    metric_label       = metric_labels[metric],
    component = case_when(
      str_starts(metric, "Wet")  ~ "Wet-Season Baseflow",
      str_starts(metric, "Peak") ~ "Peak Flow",
      str_starts(metric, "SP")   ~ "Spring Recession",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(iqr_pct_of_natural), is.finite(iqr_pct_of_natural))

write_csv(compression %>% arrange(component, metric, scenario_code),
          file.path(results_dir, "elder_range_compression_values.csv"))
message("  Saved: elder_range_compression_values.csv (exact IQR % of natural, by scenario and metric)")

p_compress <- ggplot(compression,
                     aes(y = scenario_code,
                         x = iqr_pct_of_natural,
                         fill = scenario_code)) +

  geom_vline(xintercept = 100, color = "#0288D1",
             linewidth = 0.8, linetype = "dashed") +

  geom_col(width = 0.65, color = "grey20", linewidth = 0.3,
           orientation = "y") +

  scale_fill_manual(name   = "Scenario",
                    values = setNames(scenario_colors, scenario_labels),
                    guide  = "none") +

  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, NA)) +

  facet_grid(metric_label ~ component, scales = "free",
             space = "free_y") +

  labs(title    = "Elder Creek — Range Compression",
       subtitle = "Scenario IQR as % of natural flow IQR  |  Blue dashed = natural variability (100%)\nValues < 100% indicate compressed variability even if median stays within natural range",
       x = "IQR as % of Natural Flow IQR", y = NULL) +

  theme_ffm +
  theme(strip.text.y = element_text(size = 8, angle = 0))

ggsave(file.path(boxplot_dir, "range_compression_key_metrics.png"),
       p_compress, width = 14, height = 10, dpi = 300)
message("  Saved: range_compression_key_metrics.png")

message("\nDone.")
message("  Boxplots + range compression: ", normalizePath(boxplot_dir))
