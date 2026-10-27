# 01_ff_scenario_definition

Functional flow-informed diversion scenario definition, contributed by
Jessi Ayers (The Nature Conservancy). These files are provided as
delivered and have not been modified.

## Role in the workflow

This is the first stage of the pipeline. Using functional flow metric
thresholds from the Natural Flows Database and daily gage flows, the code
calculates streamflow available for diversion while meeting functional
flow requirements, and returns available and instream flow for each
scenario in long format. Its output is the starting point for
`02_scenario_simulation/`.

## Before running

Scripts contain hardcoded paths to the author's local Box folders
(`C:\Users\jessica.ayers\Box\...`). Update these to your local project
directory before running.

## Scenarios in the code and in the report

The code computes more scenarios than the technical report uses. The
final analysis uses the 50th and 75th percentile wet-season baseflow
thresholds (FF-WS50, FF-WS75) and the 2-year peak flow threshold
(FF-Pk2), each with a 20 percent daily diversion cap. The 25th and 90th
percentile baseflow thresholds and the 40 percent diversion cap are
computed by the code but were not carried into the final analysis.

## Files

- `FloodMAR_functions.R`: core function, `calc_ffm_availability()`
- `Functions/`: helper functions for pulling gage data and calculating
  water year type
- `Run_function.R`: worked example for Elder Creek
- `FloodMAR_Stony.R`: worked example for Stony Creek

## Known gaps

- The output of this stage was postprocessed by hand in Excel before being read
  into `02_scenario_simulation/`. This manual step is not scripted (see
  `../02_scenario_simulation/README.md`).
- Worked examples are provided
  for Elder Creek and Stony Creek only. A script for Scott River is not
  included.

---

## Function documentation

(Author's original documentation follows.)

# Functions folder is to get discharge, gage data from usgs, pull it from the website and calculate water year type. All data from those outputs will be fed into the FloodMAR_Functions.R file

# FloodMAR_Functions.R is to read in the pulled gage data from before, and loop through many sites.

# Overview

#calc_ffm_availability() calculates the volume of streamflow potentially available for diversion while maintaining Functional Flow Metric (FFM) requirements. The function evaluates daily gage data #against FFM thresholds and returns available and instream flow volumes across multiple diversion scenarios.

#The function is designed to be looped across multiple gages, sites, and baseline flow metrics.

#Function Purpose

#This function:

#Reads daily stream gage discharge data.
#Reads Functional Flow Metric (FFM) targets.
#Calculates flow available above specified baseflow thresholds.
#Applies peak flow protection rules (2-year, 5-year, and 10-year recurrence interval events).
#Calculates available and instream flow under:
#FFM-based diversion limits
#20% diversion scenario
#40% diversion scenario
#Returns a long-format dataset suitable for analysis and plotting.


# FloodMAR_Stony.R is an example file for Stony Creek
