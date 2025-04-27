# README: Exploratory Spatial Data Analysis & Kriging of Baltimore City Incivility

## Overview  
This repository contains all the code, data-processing steps, and output figures for **Assignment 1: Exploratory Spatial Data Analysis and Kriging** of Baltimore City block-level incivility (NIfETy) data. The goal is to  
1. **Characterize spatial variation** in incivility scores (Aim 1 / H1)  
2. **Interpolate** a continuous surface of incivility & quantify uncertainty via ordinary kriging (Aim 2 / H2)  
3. **Test whether incivility is higher near schools** (Aim 3 / H3 & H4)  

---

## Repository Structure  
├── data/
│ ├── NIFETY_cleaned_data.xlsx # Incivility point observations
│ ├── schools_data.xlsx # Baltimore City school coordinates
│ └── baltimore_city_boundary.shp # City boundary polygon
│
├── results/ # Generated figures & CSV summaries
│ ├── data_points_map.png # Fig 1: raw incivility scatter
│ ├── incivility_histogram.png # Fig 2: distribution & mean line
│ ├── incivility_qqplot.png # Fig 3: Q–Q normality check
│ ├── semivariogram_plot.png # Fig 4: experimental variogram
│ ├── fitted_semivariogram.png # Fig 5: best WLS model fit
│ ├── semivariogram_model_comparison.png # Fig 6: model SSE comparison
│ ├── cross_validation_plot.png # Fig 7: observed vs. predicted (RMSE & MAE)
│ ├── kriged_incivility_map.png # Fig 8: predicted surface
│ ├── kriging_std_error_map.png # Fig 9: standard‐error surface
│ ├── school_buffer_analysis_map.png# Fig 10: incivility inside vs. outside 500 m
│ ├── distance_to_school_plot.png # Fig 11: incivility vs. distance scatter
│ ├── buffer_analysis_results.csv # Table: means & p-values
│ └── distance_correlation_results.csv # Table: Pearson r & CI
│
└── Assignment-1.R # Full R script driving all steps

---

## Dependencies  
- **R** ≥ 4.0  
- Packages:  
  - `sf`, `readxl`, `geoR`, `dplyr`, `tibble`,  
  - `ggplot2`, `ggspatial`, `raster`, `viridis`,  
  - `scales`, `cowplot`  

---

## Usage & Workflow  

1. **Set file paths**  
   - In `Assignment1_analysis.R`, update `data_dir`, `baltimore_city_boundary_dir`, `results_dir`, etc.

2. **Run the script**  
   ```r
   source("Assignment1_analysis.R")
Reads, reprojects, and cleans the data

Builds a geoR object and explores distribution & normality (H1)

Computes & fits semivariogram (spherical model: nugget ≈ 20, partial sill ≈ 32, range ≈ 5 km)

Cross-validates variogram (RMSE ≈ 4.9, MAE ≈ 3.9)

Performs ordinary kriging over a 200×200 grid → prediction & SE rasters (H2)

Conducts school‐buffer t-tests (H3) & distance‐incivility correlation (H4)

Inspect outputs

All figures (.png) in results/ correspond to “Figure X” in your report

Summary tables (.csv) document buffer analysis and correlation results

Figures ↔ Hypotheses

Fig	Filename	Description	Hypothesis
1	data_points_map.png	Raw incivility & 500 m school buffers map	—
2	incivility_histogram.png	Incivility distribution & mean line	H1 (normality)
3	incivility_qqplot.png	Q–Q normality check	H1
4	semivariogram_plot.png	Experimental semivariogram	H1
5	fitted_semivariogram.png	Fitted spherical variogram	H1
6	semivariogram_model_comparison.png	SSE comparison of variogram models	H1
7	cross_validation_plot.png	Observed vs. predicted (RMSE & MAE)	H2
8	kriged_incivility_map.png	Predicted incivility surface	H2
9	kriging_std_error_map.png	Kriging standard‐error map	H2
10	school_buffer_analysis_map.png	500 m buffer inside vs. outside comparison map	H3
11	distance_to_school_plot.png	Scatter of incivility vs. distance & fit line	H4
Notes & Extensions
All spatial data are projected to NAD83 / UTM Zone 18N (meters).

You can vary buffer distances or variogram settings in the script for sensitivity analysis.

Consider adding demographic covariates for regression‐kriging or fitting GLS for the distance‐incivility relationship.

Author: Marc Risney mrisney1@jhu.edu
Course: Spatial Applications IV (Public Health GIS)
Semester: Spring 2025