# Code

R scripts for the Flood-MAR diversion criteria analysis, organized by
workflow stage. For project background, data availability, and citation
information, see the [repository root README](../README.md).

## Folder Guide

Scripts run in the following order:

| Folder | Contents | Contributor |
|---|---|---|
| `01_ff_scenario_definition/` | Defines functional flow-informed diversion criteria and thresholds for each site | Jessi Ayers (TNC) |
| `02_scenario_simulation/` | Simulates daily diversion/instream flow per scenario; calculates recharge opportunity metrics | Jason Wiener (ESA) |
| `03_ffc_analysis/` | Calculates functional flow metrics via the CEFF Functional Flows Calculator; includes the peak duration/frequency correction | Kris Taniguchi-Quan (SCCWRP), building on initial code by Adriana Le Compte-Santiago (SCCWRP) |
| `04_diagnostics/` | Diagnostic checks for spring recession timing sensitivity; documents the wet-season baseflow correction | Kris Taniguchi-Quan (SCCWRP) |
| `05_hydrographs/` | Comparative hydrograph figures for representative wet/dry years, by scenario | Kris Taniguchi-Quan (SCCWRP) |
| `06_ffm_plots/` | Boxplot comparisons of functional flow metrics across scenarios; range compression screening | Kris Taniguchi-Quan (SCCWRP), building on initial code by Adriana Le Compte-Santiago (SCCWRP) |
| `07_response_curves/` | Deviation-from-baseline response curves; correlation screening for featured metric combinations | Kris Taniguchi-Quan (SCCWRP) |
| `08_cross_site/` | Cross-site comparison of peak 2-year duration/frequency patterns | Kris Taniguchi-Quan (SCCWRP) |

Folders `01_` and `02_` are provided as delivered by their original authors
and have not been modified; each has its own README noting any
site-specific caveats. Folders `03_`–`08_` are internally documented with
section-level comments tying each part of the code to the specific figure,
table, or finding it produces in the technical memo.

## Before running

Each script contains a hardcoded `setwd()` — update this to your local
project directory (see each script's header) before running. Scripts
within `03_`–`08_` expect input data at `../Data/inputs/` and write output
to `../Data/outputs/`, relative to the working directory set at the top of
each script.

## Data

Raw input flow data and full simulation outputs are not included in this
repository due to file size. Scripts expect this data at `../Data/inputs/`
and `../Data/outputs/`, relative to each script's working directory (see
"Before running," above). Data is available upon request — see the
[repository root README](../README.md#data-availability) for contact
information.

## Questions

For questions about this code, contact Kris Taniguchi-Quan (SCCWRP) at
kristinetq@sccwrp.org.