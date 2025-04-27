# ------------------------------------------------------------
# Assignment 1: Exploratory Spatial Data Analysis and Kriging
# for Baltimore City Incivility (NIfETy) Data
# ------------------------------------------------------------

# Load required libraries
library(sf)           # for spatial vector data handling
library(readxl)       # for reading Excel files
library(geoR)         # for geostatistical analysis and kriging
library(tibble)
library(dplyr)        # for data manipulation
library(ggplot2)      # for enhanced plotting
library(ggspatial)    # for north arrows and scale bars
library(raster)       # for raster operations
library(viridis)      # for better color palettes
library(scales)       # for scale formatting
library(cowplot)      # for combining plots
library(viridis)


library(ggplot2)
library(ggspatial)
library(sf)
library(viridis)

# ----------------------------
# 1. SET FILE PATHS AND WORKING DIRECTORY
# ----------------------------
# Set your working directory to where your files are stored

data_dir <- "/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment1/data"
balt_dir <- "/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment1/baltimore_county_msa_data"
balt_census_tracts_dir <- "/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment1/baltimore_census_tracts"
baltimore_city_boundary_dir <- "/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment1/baltimore_city_boundary"

results_dir <- "/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment1/results"

# Create results directory if it doesn't exist
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  cat("Created results directory at:", results_dir, "\n")
}

# ----------------------------
# 2. READ IN SPATIAL DATA
# ----------------------------
# Read Baltimore City shapefile
balt_shp_path <- file.path(baltimore_city_boundary_dir, "baltimore_city_boundary.shp")
balt_shp <- st_read(balt_shp_path)

# Check its current CRS
print(st_crs(balt_shp))  # Should show EPSG:3857

# Transform to match your other data (NAD83 / UTM Zone 18N)
new_balt_shp_proj <- st_transform(balt_shp, crs = 26918)

print(st_crs(new_balt_shp_proj))

# Read NIfETy incivility data from Excel
fety_path <- file.path(data_dir, "NIFETY_cleaned_data.xlsx")
fety_data <- read_excel(fety_path)
print(head(fety_data))
print(names(fety_data))

# Read schools data from Excel
schools_file <- file.path(data_dir, "schools_data.xlsx")
schools_data <- read_excel(schools_file)
print(head(schools_data))
print(names(schools_data))

# Read census tracts if they're used in the mapping
census_tracts_path <- file.path(balt_census_tracts_dir, "2020_Census_Tracts_(Census_TIGER).shp")
if (file.exists(census_tracts_path)) {
  census_tracts <- st_read(census_tracts_path, quiet = TRUE)
  census_tracts_proj <- st_transform(census_tracts, crs = 26918)
} else {
  # Create an empty placeholder if the file doesn't exist
  census_tracts_proj <- st_sf(geometry = st_sfc(), crs = 26918)
  cat("Census tracts shapefile not found. Maps will be created without tract boundaries.\n")
}

# ----------------------------
# 3. TRANSFORM COORDINATES TO PROJECTED CRS
# ----------------------------
# Create an sf object from fETy data (WGS84 coordinates, EPSG:4326)
fety_sf <- st_as_sf(fety_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to projected CRS (NAD83 / UTM Zone 18N, EPSG:26918)
fety_sf_proj <- st_transform(fety_sf, crs = 26918)

# Clean and transform schools data
schools_data_clean <- schools_data %>% 
  filter(!is.na(Longitude) & !is.na(Latitude))

schools_sf <- st_as_sf(schools_data_clean, coords = c("Longitude", "Latitude"), crs = 4326)
schools_sf_proj <- st_transform(schools_sf, crs = 26918)

# ----------------------------
# 4. CREATE A geoR GEODATA OBJECT FOR ANALYSIS
# ----------------------------
# Extract coordinates and incivility values
coords <- st_coordinates(fety_sf_proj)
incivility_vals <- fety_sf_proj$Incivility

# Check for any missing values and handle them
if(any(is.na(incivility_vals))) {
  warning("Missing values found in incivility data. Removing them for analysis.")
  valid_idx <- !is.na(incivility_vals)
  coords <- coords[valid_idx, ]
  incivility_vals <- incivility_vals[valid_idx]
}

# Check for duplicate locations and handle them
coords_df <- data.frame(x = coords[, 1], y = coords[, 2], incivility = incivility_vals)
duplicate_check <- duplicated(coords_df[, c("x", "y")])
n_duplicates <- sum(duplicate_check)

if(n_duplicates > 0) {
  cat("Found", n_duplicates, "duplicate locations in the data.\n")
  
  # Create a clean dataset by averaging values at the same locations
  coords_unique <- aggregate(incivility ~ x + y, data = coords_df, FUN = mean)
  cat("Reduced dataset from", nrow(coords_df), "to", nrow(coords_unique), "unique locations.\n")
  
  # Extract clean coordinates and values
  coords_clean <- as.matrix(coords_unique[, c("x", "y")])
  incivility_clean <- coords_unique$incivility
  
  # Create geodata object with clean data
  incivility_geo <- as.geodata(cbind(coords_clean, incivility_clean))
  cat("Created geodata object with unique locations.\n")
} else {
  # If no duplicates, use original data
  cat("No duplicate locations found in the data.\n")
  incivility_geo <- as.geodata(cbind(coords, incivility_vals))
}

# Create a plot to visualize the data points
png(file.path(results_dir, "data_points_map.png"), width = 8, height = 8, units = "in", res = 300)
points.geodata(incivility_geo, 
               main = "Baltimore Incivility Data Points",
               pt.divide = "equal", 
               col.seq = heat.colors(10),
               xlab = "Easting (m)", 
               ylab = "Northing (m)")
dev.off()

# Summarize the geodata object
summary(incivility_geo)

# ----------------------------
# 5. EXPLORATORY SPATIAL DATA ANALYSIS (ESDA)
# ----------------------------
# Compute and plot the experimental semivariogram
max_dist <- max(dist(coords))
incivility_vario <- variog(incivility_geo, max.dist = max_dist/2)

# Create a professional semivariogram plot
png(file.path(results_dir, "semivariogram_plot.png"), width = 8, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))
plot(incivility_vario, pch = 16, col = "darkblue",
     main = "Experimental Semivariogram for Incivility",
     xlab = "Distance (m)", ylab = "Semivariance",
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1,
     font.main = 2, font.lab = 2)
dev.off()

# Create a histogram of incivility values
png(file.path(results_dir, "incivility_histogram.png"), width = 8, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))
hist(incivility_vals, nclass = 20, col = "lightblue", border = "darkblue",
     main = "Distribution of Incivility Scores",
     xlab = "Incivility Score", ylab = "Frequency",
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1,
     font.main = 2, font.lab = 2)
abline(v = mean(incivility_vals), col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Mean =", round(mean(incivility_vals), 2)),
       col = "red", lty = 2, lwd = 2, cex = 1)
dev.off()

# Create a QQ-Plot to check normality
png(file.path(results_dir, "incivility_qqplot.png"), width = 8, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))
qqnorm(incivility_vals, main = "Q-Q Plot of Incivility Values",
       cex.main = 1.5, cex.lab = 1.2, cex.axis = 1,
       font.main = 2, font.lab = 2, 
       pch = 16, col = "darkblue")
qqline(incivility_vals, col = "red", lwd = 2)
dev.off()

# ----------------------------
# 6. FIT A SEMIVARIOGRAM MODEL
# ----------------------------
# Try different variogram models and select the best fit
initial_sill <- var(incivility_vals)
initial_range <- max_dist/3  # A common starting point is 1/3 of max distance

# Define models to test
models <- c("spherical", "exponential", "gaussian", "matern")
fits <- list()
fit_criteria <- data.frame(model = models, sse = NA)

# Fit each model and store results
for (i in seq_along(models)) {
  model <- models[i]
  vario_fit_temp <- variofit(incivility_vario,
                             ini.cov.pars = c(initial_sill, initial_range),
                             cov.model = model,
                             nugget = initial_sill/10,
                             weights = "cressie")
  fits[[model]] <- vario_fit_temp
  fit_criteria$sse[i] <- vario_fit_temp$value
  cat("Model:", model, "- SSErr:", vario_fit_temp$value, "\n")
}

# Select best model based on lowest SSErr
best_model_idx <- which.min(fit_criteria$sse)
best_model <- models[best_model_idx]
vario_fit <- fits[[best_model]]
cat("Best model:", best_model, "\n")

# Plot the fitted semivariogram model
png(file.path(results_dir, "fitted_semivariogram.png"), width = 8, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))
plot(incivility_vario, pch = 16, col = "darkblue",
     main = paste0("Fitted WLS Semivariogram for Incivility (", 
                   toupper(substr(best_model, 1, 1)), 
                   substr(best_model, 2, nchar(best_model)), " Model)"),
     xlab = "Distance (m)", ylab = "Semivariance",
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1,
     font.main = 2, font.lab = 2)
lines(vario_fit, col = "red", lwd = 2)
legend("bottomright", 
       legend = c("Experimental", paste0("Fitted ", 
                                         toupper(substr(best_model, 1, 1)), 
                                         substr(best_model, 2, nchar(best_model)), " Model")),
       col = c("darkblue", "red"), 
       pch = c(16, NA), 
       lty = c(NA, 1),
       lwd = c(NA, 2),
       cex = 1)
dev.off()

# Create a comparison plot of all models
png(file.path(results_dir, "semivariogram_model_comparison.png"), width = 10, height = 8, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))
plot(incivility_vario, pch = 16, col = "darkblue",
     main = "Comparison of Semivariogram Models",
     xlab = "Distance (m)", ylab = "Semivariance",
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1,
     font.main = 2, font.lab = 2)

# Define colors for different models
model_colors <- c("red", "green4", "orange", "purple")
model_ltys <- c(1, 2, 3, 4)

# Plot all model lines
for (i in seq_along(models)) {
  lines(fits[[models[i]]], col = model_colors[i], lwd = 2, lty = model_ltys[i])
}

# Add legend with SSE values
legend_text <- vector("character", length(models))
for (i in seq_along(models)) {
  legend_text[i] <- paste0(toupper(substr(models[i], 1, 1)), 
                           substr(models[i], 2, nchar(models[i])),
                           " (SSE = ", round(fit_criteria$sse[i], 2), ")")
}

legend("bottomright", 
       legend = c("Experimental", legend_text),
       col = c("darkblue", model_colors), 
       pch = c(16, rep(NA, length(models))), 
       lty = c(NA, model_ltys),
       lwd = c(NA, rep(2, length(models))),
       cex = 0.9)
dev.off()

# Print the fitted model parameters
cat("\nBest fitting model:", best_model, "\n")
cat("Nugget:", round(vario_fit$nugget, 4), "\n")
cat("Partial sill:", round(vario_fit$cov.pars[1], 4), "\n")
cat("Range:", round(vario_fit$cov.pars[2], 4), "meters\n")
cat("Sum of squared errors:", round(vario_fit$value, 4), "\n\n")

print(vario_fit)

# ----------------------------
# 7. CREATE A PREDICTION GRID
# ----------------------------
# Get the bounding box of the Baltimore shapefile
balt_bbox <- st_bbox(new_balt_shp_proj)

# Create a prediction grid with higher resolution
grid_size <- 200  # 200x200 grid for smoother results
buffer <- 0     # Buffer around the bounding box (in meters)

x_seq <- seq(balt_bbox["xmin"] - buffer, balt_bbox["xmax"] + buffer, length.out = grid_size)
y_seq <- seq(balt_bbox["ymin"] - buffer, balt_bbox["ymax"] + buffer, length.out = grid_size)
pred_grid <- expand.grid(easting = x_seq, northing = y_seq)

# ----------------------------
# 8. PERFORM ORDINARY KRIGING WITH CROSS-VALIDATION
# ----------------------------
# First, perform cross-validation to evaluate the selected variogram model
cat("\n== Cross-validation of variogram model ==\n")
cv_results <- xvalid(incivility_geo, model = vario_fit)
print(summary(cv_results))

# Calculate error metrics
mae <- mean(abs(cv_results$data - cv_results$predicted))
rmse <- sqrt(mean((cv_results$data - cv_results$predicted)^2))
cat("Mean Absolute Error:", round(mae, 3), "\n")
cat("Root Mean Square Error:", round(rmse, 3), "\n")

# Create cross-validation plot
png(file.path(results_dir, "cross_validation_plot.png"), width = 8, height = 7, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))
plot(cv_results$data, cv_results$predicted, 
     main = "Cross-Validation: Observed vs. Predicted Incivility",
     xlab = "Observed Incivility", ylab = "Predicted Incivility",
     pch = 16, col = "blue", cex = 0.8)
abline(0, 1, col = "red", lwd = 2)
# Add RMSE and MAE to plot
legend("topleft", 
       legend = c(paste("RMSE =", round(rmse, 2)),
                  paste("MAE =", round(mae, 2))),
       bty = "n", cex = 1.2)
dev.off()

# Rename columns to match the geodata object format
names(pred_grid) <- c("coords.x1", "coords.x2")

# Perform Ordinary Kriging with neighborhood parameters
krige_ok <- krige.conv(
  incivility_geo,
  locations = as.matrix(pred_grid),
  krige = krige.control(
    obj.model = vario_fit,
    type.krige = "ok",
    trend.d = "cte",
    trend.l = "cte"
  )
)

# Add predictions and kriging variance to the grid
pred_grid$prediction <- krige_ok$predict
pred_grid$krige_var <- krige_ok$krige.var
pred_grid$std_error <- sqrt(krige_ok$krige.var)

# Convert back to original column names for plotting
names(pred_grid)[1:2] <- c("easting", "northing")

# Convert prediction grid and results to raster
pred_raster <- rasterFromXYZ(cbind(pred_grid$easting, pred_grid$northing, pred_grid$prediction))
crs(pred_raster) <- st_crs(new_balt_shp_proj)$wkt

# Create standard error raster
se_raster <- rasterFromXYZ(cbind(pred_grid$easting, pred_grid$northing, pred_grid$std_error))
crs(se_raster) <- st_crs(new_balt_shp_proj)$wkt

# Mask predictions to Baltimore boundaries
balt_sp <- as(new_balt_shp_proj, "Spatial")
pred_raster_masked <- mask(pred_raster, balt_sp)
se_raster_masked <- mask(se_raster, balt_sp)

# Use the masked rasters for visualization
pred_raster <- pred_raster_masked
se_raster <- se_raster_masked

# Create diagnostic plots for kriging results
png(file.path(results_dir, "kriging_histogram.png"), width = 8, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))
hist(pred_grid$prediction, breaks = 30, col = "lightblue", border = "darkblue",
     main = "Distribution of Kriged Incivility Values",
     xlab = "Predicted Incivility", ylab = "Frequency",
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1)
abline(v = mean(pred_grid$prediction), col = "red", lwd = 2, lty = 2)
legend("topright", 
       legend = paste("Mean =", round(mean(pred_grid$prediction), 2)),
       col = "red", lty = 2, lwd = 2)
dev.off()

cat("\n== Kriging Complete ==\n")
cat("Prediction range:", round(range(pred_grid$prediction, na.rm = TRUE), 2), "\n")
cat("Standard error range:", round(range(pred_grid$std_error, na.rm = TRUE), 2), "\n")

# ----------------------------
# 9. HYPOTHESIS TESTING: IS INCIVILITY HIGHER AROUND SCHOOLS?
# ----------------------------
# Approach 1: Buffer Analysis
buffer_distances <- c(100, 250, 500, 1000)  # in meters
buffer_results <- data.frame(
  distance = buffer_distances,
  mean_inside = numeric(length(buffer_distances)),
  mean_outside = numeric(length(buffer_distances)),
  n_inside = numeric(length(buffer_distances)),
  n_outside = numeric(length(buffer_distances)),
  p_value = numeric(length(buffer_distances))
)

# Create citywide mean for reference
citywide_mean <- mean(incivility_vals)

for (i in seq_along(buffer_distances)) {
  dist <- buffer_distances[i]
  
  # Create buffer around schools
  schools_buffer <- st_buffer(schools_sf_proj, dist)
  schools_buffer_union <- st_union(schools_buffer)
  
  # Find which incivility points fall within the buffer
  in_buffer <- st_intersects(fety_sf_proj, schools_buffer_union, sparse = FALSE)
  
  # Extract incivility values inside and outside buffer
  incivility_in <- incivility_vals[in_buffer]
  incivility_out <- incivility_vals[!in_buffer]
  
  # Calculate statistics
  buffer_results$mean_inside[i] <- mean(incivility_in)
  buffer_results$mean_outside[i] <- mean(incivility_out)
  buffer_results$n_inside[i] <- length(incivility_in)
  buffer_results$n_outside[i] <- length(incivility_out)
  
  # Perform t-test to compare means
  t_test <- t.test(incivility_in, incivility_out)
  buffer_results$p_value[i] <- t_test$p.value
}

# Print buffer analysis results
print(buffer_results)

# Create the 500m buffer for mapping
schools_buffer_500 <- st_buffer(schools_sf_proj, 500)
schools_buffer_union_500 <- st_union(schools_buffer_500)

# Approach 2: Nearest School Distance Analysis
# Calculate distance from each incivility point to nearest school
nearest_dist <- st_distance(fety_sf_proj, schools_sf_proj)
min_dist <- apply(nearest_dist, 1, min)

# Create distance dataframe
dist_df <- data.frame(
  incivility = incivility_vals,
  dist_to_school = as.numeric(min_dist)
)

# Correlation analysis
cor_result <- cor.test(dist_df$incivility, dist_df$dist_to_school)
print(cor_result)

# ----------------------------
# 10. DEFINE VISUALIZATION PARAMETERS
# ----------------------------
# Define color schemes and other visualization parameters
school_color <- "black"
border_color <- "gray30"
buffer_color <- "steelblue"
buffer_dist <- 500  # Default buffer distance for mapping
error_palette <- viridis_pal(option = "magma", direction = -1)
incivility_palette <- viridis_pal(option = "plasma", direction = -1)

# Define a zoom bbox for consistent mapping
# If you want to focus on Baltimore City proper rather than the whole county
zoom_bbox <- st_bbox(new_balt_shp_proj)
zoom_bbox[1] <- zoom_bbox[1] - 500  # xmin
zoom_bbox[2] <- zoom_bbox[2] - 500  # ymin
zoom_bbox[3] <- zoom_bbox[3] + 500  # xmax
zoom_bbox[4] <- zoom_bbox[4] + 500  # ymax


# ----------------------------
# 11. CREATE MAPS USING ggplot2 AND ggspatial
# ----------------------------

# --- define your zoom window once, up top ---
zoom_bbox <- st_bbox(new_balt_shp_proj)
zoom_bbox[c("xmin","ymin","xmax","ymax")] <-
  c(zoom_bbox["xmin"] - 500,
    zoom_bbox["ymin"] - 500,
    zoom_bbox["xmax"] + 500,
    zoom_bbox["ymax"] + 500)

# --- a reusable minimal map theme ---
base_map_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.background  = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_line(color = "lightgrey", size = 0.3),
    panel.grid.minor  = element_line(color = "lightgrey", linetype = "dotted", size = 0.2),
    legend.position   = "left",
    legend.justification = c(0, 1),
    legend.margin     = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "white", color = "gray70"),
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# --- the study area map itself ---
study_area_map <- ggplot() +
  # 1) city footprint (semi-transparent green)
  geom_sf(data   = new_balt_shp_proj,
          fill   = "#AACB7A",
          alpha  = 0.6,
          color  = "black",
          linewidth = 0.5) +
  
  # 2) census-tract outlines
  geom_sf(data   = census_tracts_proj,
          fill   = NA,
          color  = "white",
          size   = 0.2,
          alpha  = 0.5) +
  
  # 3) 500 m school buffers
  geom_sf(data   = st_union(st_buffer(schools_sf_proj, 500)),
          fill   = "steelblue",
          alpha  = 0.3,
          color  = NA) +
  
  # 4) incivility points
  geom_sf(data   = fety_sf_proj,
          aes(color = Incivility),
          size   = 0.8,
          alpha  = 0.7) +
  scale_color_viridis_c(option    = "plasma",
                        direction = -1,
                        limits    = c(0, 35),
                        breaks    = seq(0, 35, 5),
                        name      = "Incivility\nScore",
                        guide     = guide_colorbar(reverse = TRUE)) +
  
  # 5) school symbols
  geom_sf(data   = schools_sf_proj,
          shape  = 25,
          fill   = "black",
          color  = "white",
          size   = 2,
          stroke = 0.3) +
  
  # 6) north arrow & scale bar (imperial)
  annotation_north_arrow(location    = "tl",
                         which_north = "true",
                         style       = north_arrow_fancy_orienteering(),
                         height      = unit(1, "cm"),
                         width       = unit(1, "cm")) +
  annotation_scale(location       = "bl",
                   unit_category = "imperial",
                   width_hint    = 0.3,
                   bar_cols      = c("black", "white")) +
  
  # 7) force our zoom window
  coord_sf(xlim   = c(zoom_bbox["xmin"], zoom_bbox["xmax"]),
           ylim   = c(zoom_bbox["ymin"], zoom_bbox["ymax"]),
           expand = FALSE,
           datum  = NA) +
  
  # 8) title + shared theme
  labs(title = "Baltimore City Incivility & 500 m School Buffers") +
  base_map_theme



# 2. Kriged Incivility Surface
# Convert raster to data.frame for ggplot
pred_raster_df <- as.data.frame(pred_raster, xy = TRUE)
names(pred_raster_df) <- c("x", "y", "value")

# Define the breaks and labels properly for the kriged incivility map
incivility_breaks <- c(4.02, 7.17, 9.45, 11.3, 13.1, 15.1, 17.5, 20.1, 24)
incivility_labels <- c("4.02 - 7.16", "7.17 - 9.44", "9.45 - 11.2", "11.3 - 13", 
                       "13.1 - 15", "15.1 - 17.4", "17.5 - 20", "20.1 - 24")

# Create a discrete version to match the reference image exactly
kriged_map_fixed <- ggplot() +
  # Raster layer for kriged surface - keep this the same
  geom_raster(data = pred_raster_df, aes(x = x, y = y, fill = value)) +
  
  # Baltimore boundaries
  geom_sf(data = new_balt_shp_proj, fill = NA, color = border_color, size = 0.8) +
  
  # Census tracts as subtle white lines
  geom_sf(data = census_tracts_proj, fill = NA, color = "white", size = 0.2, alpha = 0.5) +
  
  # Schools as triangles
  geom_sf(data = schools_sf_proj, shape = 25, fill = school_color, color = "white", 
          size = 1.2, stroke = 0.3) +
  
  # Use a simpler approach with continuous scale but reversed legend
  scale_fill_distiller(
    palette = "RdYlGn", 
    direction = -1,
    limits = c(4, 24),
    breaks = c(4, 8, 12, 16, 20, 24), 
    na.value = "white",
    guide = guide_colorbar(reverse = TRUE)  # This is the key part to reverse the legend
  ) +
  
  # Set map limits
  coord_sf(
    xlim = c(zoom_bbox["xmin"], zoom_bbox["xmax"]), 
    ylim = c(zoom_bbox["ymin"], zoom_bbox["ymax"]),
    expand = FALSE,
    datum = NA
  ) +
  
  # Rest of the code remains the same
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"), 
    pad_y = unit(0.2, "in"),
    style = north_arrow_minimal(
      fill = "black",
      line_col = "black"
    ),
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm")
  ) +
  
  annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    text_family = "sans",
    pad_y = unit(0.4, "in"),
    style = "ticks",
    unit_category = "imperial"
  ) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "black", size = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "right",
    legend.box.background = element_rect(color = "black", fill = "white", size = 0.2),
    legend.margin = margin(6, 6, 6, 6)
  ) +
  
  labs(
    title = "Kriged Incivility Surface",
    fill = "Incivility Score"
  )


# 3. Kriging Standard Errors
# Convert error raster to data.frame for ggplot
se_raster_df <- as.data.frame(se_raster, xy = TRUE)
names(se_raster_df) <- c("x", "y", "value")

# Before creating the map, check the SE raster values
print("SE raster summary:")
print(summary(values(se_raster)))
print("Number of non-NA values:")
print(sum(!is.na(values(se_raster))))

# If values need to be scaled to match the desired range (24-48)
# You might need this if your values are in the 3-7 range as per the original code
if(max(values(se_raster), na.rm = TRUE) < 10) {
  # Scale the values to the desired range
  scale_factor <- 7  # Approximate scale factor to go from range 3-7 to 24-48
  se_raster_scaled <- se_raster * scale_factor
  se_raster <- se_raster_scaled
  
  # Update the dataframe for plotting
  se_raster_df <- as.data.frame(se_raster, xy = TRUE)
  names(se_raster_df) <- c("x", "y", "value")
}

# Create the standard error map with adjusted color palette and ranges
# Before creating the map, check the distribution of values
summary_stats <- summary(se_raster_df$value[!is.na(se_raster_df$value)])
print(summary_stats)

# Create a more data-driven set of breaks based on actual distribution
# This will create a new standard error map with adjusted breaks

std_error_map <- ggplot() +
  # Raster layer for standard error
  geom_raster(data = se_raster_df, aes(x = x, y = y, fill = value), alpha = 0.8) +
  
  # Baltimore boundaries
  geom_sf(data = new_balt_shp_proj, fill = NA, color = "black", size = 0.5) +
  
  # Census tracts as subtle white lines
  geom_sf(data = census_tracts_proj, fill = NA, color = "gray30", size = 0.2, alpha = 0.7) +
  
  # Original data points as small black dots
  geom_sf(data = fety_sf_proj, color = "black", size = 0.3, alpha = 0.5) +
  
  # Use quantile-based breaks instead of evenly spaced breaks
  scale_fill_gradientn(
    colors = c("#2B803E", "#5F9F5F", "#A6C87F", "#D7E1A6", "#FFF6A6", 
               "#F4D1A6", "#E0A48F", "#C87A7F", "#AF547F", "#963090"),
    # Adjust these to match your actual data distribution
    breaks = c(23.8, 28, 31, 34, 37, 40, 43, 48, 53.4),
    labels = c("23.8 - 28", "28.1 - 31", "31.1 - 34", "34.1 - 37", 
               "37.1 - 40", "40.1 - 43", "43.1 - 48", "48.1 - 53.4", ""),
    na.value = "white",
    guide = guide_colorbar(reverse = TRUE)
  ) +
  
  # Set map limits
  coord_sf(
    xlim = c(zoom_bbox["xmin"], zoom_bbox["xmax"]), 
    ylim = c(zoom_bbox["ymin"], zoom_bbox["ymax"]),
    expand = FALSE,
    datum = NA
  ) +
  
  # Add north arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"), 
    pad_y = unit(0.2, "in"),
    style = north_arrow_minimal(
      fill = "black",
      line_col = "black"
    ),
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm")
  ) +
  
  # Add scale bar
  annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    text_family = "sans",
    pad_y = unit(0.4, "in"),
    style = "ticks",
    unit_category = "imperial"
  ) +
  
  # Add consistent theme elements
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "black", size = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "right",
    legend.box.background = element_rect(color = "black", fill = "white", size = 0.2),
    legend.margin = margin(6, 6, 6, 6)
  ) +
  
  # Add title and proper legend labels
  labs(
    title = "Kriging Standard Error Map",
    fill = "Standard Error"
  )


buffer_map <- ggplot() +
  geom_sf(data  = balt_shp,
          fill  = "#AACB7A",
          alpha = 0.6,
          color = "black",
          size  = 0.6) +
  geom_sf(data  = schools_buffer_union_500,
          fill  = "steelblue",
          alpha = 0.4,
          color = NA) +
  geom_sf(data = fety_sf_proj,
          aes(color = Incivility),
          size = 1.2, alpha = 0.8) +
  # zero = light yellow, 30 = red, legend reversed
  scale_color_gradient(
    low    = "#FFFFCC",       # very light
    high   = "#D73027",       # hot red
    limits = c(0, 30),
    guide  = guide_colorbar(
      reverse   = TRUE,
      title     = "Incivility\nScore",
      barwidth  = unit(0.8, "cm"),
      barheight = unit(6,   "cm")
    )
  ) +
  annotation_north_arrow(location    = "tl",
                         which_north = "true",
                         style       = north_arrow_fancy_orienteering()) +
  annotation_scale(location       = "bl",
                   unit_category = "imperial",
                   dist_unit     = "mi",
                   dist          = 5,
                   width_hint    = 0.3,
                   bar_cols      = c("black", "white")) +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background  = element_rect(fill = "white"),
    panel.grid.major  = element_line(color = "lightgrey", size = 0.3),
    panel.grid.minor  = element_line(color = "lightgrey", linetype = "dotted", size = 0.2),
    legend.position   = c(0.85, 0.3),
    legend.background = element_rect(fill = "white", color = "gray70"),
    plot.title        = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(title = "Incivility Scores Around 500 m School Buffers")


# 5. Distance to Schools Scatter Plot
dist_plot <- ggplot(dist_df, aes(x = dist_to_school, y = incivility)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_x_continuous(labels = comma, limits = c(0, 3000)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0)
  ) +
  labs(
    title = "Relationship Between Incivility and Distance to Nearest School",
    subtitle = paste0("Correlation = ", round(cor_result$estimate, 3), 
                      ", p-value = ", format.pval(cor_result$p.value, digits = 3)),
    x = "Distance to Nearest School (meters)",
    y = "Incivility Score",
    caption = "Note: Negative correlation indicates incivility decreases with distance from schools"
  )

# ----------------------------
# 12. SAVE OUTPUT MAPS
# ----------------------------
# Save each map as a high-quality PNG

kriged_map <- kriged_map_fixed
ggsave(
  file.path(results_dir, "kriged_incivility_map.png"),
  kriged_map,
  width = 10, height = 8, dpi = 300
)

ggsave(file.path(results_dir, "study_area_map.png"), study_area_map, width = 10, height = 8, dpi = 300)
ggsave(file.path(results_dir, "kriged_incivility_map.png"), kriged_map, width = 10, height = 8, dpi = 300)
ggsave(file.path(results_dir, "kriging_std_error_map.png"), std_error_map, width = 10, height = 8, dpi = 300)
ggsave(file.path(results_dir, "school_buffer_analysis_map.png"), buffer_map, width = 10, height = 8, dpi = 300)
ggsave(file.path(results_dir, "distance_to_school_plot.png"), dist_plot, width = 10, height = 6, dpi = 300)

# Create a summary table for buffer analysis
buffer_results <- as_tibble(buffer_results)

buffer_table <- buffer_results %>%
  # first compute significance *before* you turn p_value into text
  mutate(
    significant = p_value < 0.05,                # logical
    distance    = paste0(distance, "m"),        
    difference  = round(mean_inside - mean_outside, 2),
    # now format your columns
    mean_inside  = round(mean_inside, 2),
    mean_outside = round(mean_outside, 2),
    p_value      = format.pval(p_value, digits = 3)
  ) %>%
  dplyr::select(
    distance, mean_inside, mean_outside,
    difference, n_inside, n_outside,
    p_value, significant
  )

# Write buffer analysis results to CSV
write.csv(buffer_table, file.path(results_dir, "buffer_analysis_results.csv"), row.names = FALSE)

# Save correlation results
cor_summary <- data.frame(
  method = cor_result$method,
  correlation = cor_result$estimate,
  t_statistic = cor_result$statistic,
  p_value = cor_result$p.value,
  conf_int_low = cor_result$conf.int[1],
  conf_int_high = cor_result$conf.int[2]
)

write.csv(cor_summary, file.path(results_dir, "distance_correlation_results.csv"), row.names = FALSE)

# Print summary of results to console
cat("\n== ANALYSIS SUMMARY ==\n")
cat("Total NIfETy data points analyzed:", length(incivility_vals), "\n")
cat("Citywide mean incivility score:", round(mean(incivility_vals), 2), "\n")
cat("Range of incivility scores:", paste(round(range(incivility_vals), 2), collapse = " - "), "\n\n")

cat("Best semivariogram model:", best_model, "\n")
cat("Nugget:", round(vario_fit$nugget, 2), "\n")
cat("Partial sill:", round(vario_fit$cov.pars[1], 2), "\n")
cat("Range:", round(vario_fit$cov.pars[2], 2), "meters\n\n")

cat("Kriging cross-validation RMSE:", round(rmse, 2), "\n")
cat("Kriging prediction range:", paste(round(range(pred_grid$prediction, na.rm = TRUE), 2), collapse = " - "), "\n")
cat("Kriging standard error range:", paste(round(range(pred_grid$std_error, na.rm = TRUE), 2), collapse = " - "), "\n\n")

cat("School buffer analysis (500m):\n")
cat("  Mean incivility inside buffer:", round(buffer_results$mean_inside[buffer_results$distance == 500], 2), "\n")
cat("  Mean incivility outside buffer:", round(buffer_results$mean_outside[buffer_results$distance == 500], 2), "\n")
cat("  Difference:", round(buffer_results$mean_inside[buffer_results$distance == 500] - 
                             buffer_results$mean_outside[buffer_results$distance == 500], 2), "\n")
cat("  p-value:", format.pval(buffer_results$p_value[buffer_results$distance == 500], digits = 3), "\n\n")

cat("Distance correlation analysis:\n")
cat("  Correlation:", round(cor_result$estimate, 3), "\n")
cat("  p-value:", format.pval(cor_result$p.value, digits = 3), "\n\n")

cat("All results saved to:", results_dir, "\n")