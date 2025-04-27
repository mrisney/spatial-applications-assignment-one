# README: Exploratory Spatial Data Analysis & Kriging of Baltimore City Incivility

## Overview
This repository contains code, data-processing steps, and output figures for **Assignment 1: Exploratory Spatial Data Analysis and Kriging** of Baltimore City block-level incivility (NIfETy) data.  
**Goals:**
1. **Characterize spatial variation** in incivility scores (Aim 1 / H1)  
2. **Interpolate** a continuous surface of incivility and quantify uncertainty via ordinary kriging (Aim 2 / H2)  
3. **Test whether incivility is higher near schools** (Aim 3 / H3 & H4)  

## Dependencies
- **R** ≥ 4.0  
- R packages:
  - `sf`, `readxl`, `geoR`, `tibble`, `dplyr`
  - `ggplot2`, `ggspatial`, `raster`, `viridis`
  - `scales`, `cowplot`

---

## Usage & Workflow

1. **Configure paths**  
   In `Assignment-1.R`, set:
   ```r
   data_dir <- "path/to/data"
   baltimore_city_boundary_dir <- "path/to/boundary"
   results_dir <-       "path/to/results"
Run the analysis

r
Copy
Edit
source("Assignment1_analysis.R")
Reads, reprojects, and cleans data

Builds a geoR object & explores distribution/normality (H1)

Computes & fits semivariogram (spherical: nugget ≈ 20, sill ≈ 32, range ≈ 5 km)

Cross-validates variogram (RMSE ≈ 4.9, MAE ≈ 3.9)

Performs ordinary kriging on a 200×200 grid → prediction & SE rasters (H2)

Conducts buffer t-tests (H3) & distance–incivility correlation (H4)

Inspect outputs

Figures (.png) in results/ correspond to Figures 1–11.

Summary tables (.csv) document buffer analysis & correlation results.

Figures ↔ Hypotheses

Fig	File	Description	Hypothesis
1	data_points_map.png	Raw incivility + 500 m school buffers	—
2	incivility_histogram.png	Incivility distribution & mean line	H1 (normal)
3	incivility_qqplot.png	Q–Q plot normality check	H1
4	semivariogram_plot.png	Experimental semivariogram	H1
5	fitted_semivariogram.png	Fitted spherical variogram	H1
6	semivariogram_model_comparison.png	Model SSE comparison	H1
7	cross_validation_plot.png	Observed vs. predicted (RMSE & MAE)	H2
8	kriged_incivility_map.png	Kriged incivility surface	H2
9	kriging_std_error_map.png	Kriging standard-error map	H2
10	school_buffer_analysis_map.png	500 m buffer mean inside vs. outside	H3
11	distance_to_school_plot.png	Incivility vs. distance scatter & fit	H4
Notes & Extensions
All spatial data → NAD83 / UTM Zone 18N (meters) projection.

You can adjust buffer distances or variogram parameters for sensitivity.

Consider adding demographic covariates for regression-kriging or fitting GLS to account for spatial autocorrelation.

Author: Marc Risney <mrisney1@jhu.edu>
Course: Spatial Applications IV (Public Health GIS), Spring 2025