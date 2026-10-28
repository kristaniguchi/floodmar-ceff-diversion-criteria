# Operationalizing Functional Flows to Inform and Evaluate Flood-Managed Aquifer Recharge (Flood-MAR) Diversion Approaches

This repository contains the analysis code supporting a DWR-funded Flood-MAR
pilot study evaluating alternative streamflow diversion criteria (90/20,
cumulative wet-season/80-50, functional flow-informed, and ecohydraulic
approaches) at three California case study sites: Scott River, Elder Creek,
and Stony Creek.

## Repository Structure

- `Code/` — R scripts, organized by analysis workflow stage. See the
  [Code README](Code/README.md) for the full folder-by-folder breakdown and
  contributor credits.
- Raw input data and full simulation outputs are not included in this
  repository; see [Data Availability](#data-availability), below.

## Analysis Workflow

The pipeline runs in eight stages, from defining functional flow-informed
diversion criteria through cross-site comparison of results. See the
[Code README](Code/README.md) for the full stage-by-stage breakdown.

Two known gaps in full end-to-end reproducibility are documented in their
respective folder READMEs:

- An unscripted, manual Excel post-processing step between the FF scenario
  definition stage and the scenario simulation stage (see
  `Code/02_scenario_simulation/README.md`)
- The FF scenario definition folder (`Code/01_ff_scenario_definition/`)
  includes worked examples for Elder Creek and Stony Creek only. A
  corresponding script for Scott River is not included.

## Data Availability

Raw input flow data and full simulation outputs are available upon
request. Contact Kris Taniguchi-Quan (SCCWRP) at kristinetq@sccwrp.org.

## Citation

If you use this code or data, please cite:

Taniguchi-Quan, K., J. Wiener, J. Ayers, B. Gibbons, and A. Le
Compte-Santiago. 2026. *Operationalizing Functional Flows to Inform and
Evaluate Flood-Managed Aquifer Recharge (Flood-MAR) Diversion Approaches.*
Technical Report 1501. Southern California Coastal Water Research Project.
Costa Mesa, CA.

## Contact

Kris Taniguchi-Quan, Southern California Coastal Water Research Project
(SCCWRP) — kristinetq@sccwrp.org
