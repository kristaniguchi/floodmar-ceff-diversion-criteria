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
