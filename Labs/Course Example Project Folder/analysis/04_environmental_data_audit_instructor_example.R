# ECCB 689 Lab 04: Environmental Data Audit and Processing
# Instructor walkthrough script
#
# This script uses the Lab 3 course example. Students should create their own
# script called analysis/04_environmental_data_audit.R and type through the
# relevant steps during class.

# -----------------------------------------------------------------------------
# 1. Recreate the Lab 3 objects
# -----------------------------------------------------------------------------

library(sf)
library(terra)

source("analysis/03_environmental_space_background.R")

stopifnot(
  exists("occ"),
  exists("occ_sf"),
  exists("env"),
  exists("m_broad")
)

dir.create("output", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. Inspect the environmental raster stack
# -----------------------------------------------------------------------------

env
names(env)
crs(env)
res(env)
ext(env)
global(env, c("min", "max", "mean"), na.rm = TRUE)

# The two course layers are:
#   bio1_c    annual mean temperature in degrees C
#   bio12_mm  annual precipitation in mm

# -----------------------------------------------------------------------------
# 3. Restrict the layers to the broad M hypothesis and make a map
# -----------------------------------------------------------------------------

env_broad_lab4 <- mask(crop(env, vect(m_broad)), vect(m_broad))

png(
  "output/04_bio1_broad_M.png",
  width = 1800,
  height = 1400,
  res = 180
)

plot(
  env_broad_lab4[["bio1_c"]],
  main = "Annual mean temperature within the broad M hypothesis",
  axes = TRUE
)
plot(vect(m_broad), add = TRUE, border = "firebrick3", lwd = 2)
points(vect(occ_sf), pch = 16, cex = 0.45, col = "black")
dev.off()

# -----------------------------------------------------------------------------
# 4. Extract environmental values at occurrence locations
# -----------------------------------------------------------------------------

occ_env_lab4 <- extract(env_broad_lab4, vect(occ_sf))

occ_env_lab4 <- cbind(
  occ[, c("record_id", "species", "decimalLongitude", "decimalLatitude")],
  occ_env_lab4[, c("bio1_c", "bio12_mm")]
)

head(occ_env_lab4)
summary(occ_env_lab4[, c("bio1_c", "bio12_mm")])
colSums(is.na(occ_env_lab4[, c("bio1_c", "bio12_mm")]))

# -----------------------------------------------------------------------------
# 5. Transform precipitation and compare the distributions
# -----------------------------------------------------------------------------

# This is an example transformation, not a default requirement for every model.
# Retain the raw variable and document why any transformation is appropriate.

precip_log10 <- log10(env_broad_lab4[["bio12_mm"]])
names(precip_log10) <- "bio12_log10"

occ_env_lab4$bio12_log10 <- log10(occ_env_lab4$bio12_mm)

png(
  "output/04_precipitation_raw_and_log10.png",
  width = 1800,
  height = 900,
  res = 180
)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
hist(
  env_broad_lab4[["bio12_mm"]],
  main = "Annual precipitation",
  xlab = "mm",
  col = "grey80",
  border = "white"
)
hist(
  precip_log10,
  main = "Log10 annual precipitation",
  xlab = "log10(mm)",
  col = "grey80",
  border = "white"
)
dev.off()

# Save the final occurrence-by-predictor table, including the transformed value.
write.csv(
  occ_env_lab4,
  "output/04_occurrence_environment_values.csv",
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 6. Compare fine and coarse spatial grain
# -----------------------------------------------------------------------------

env_coarse <- aggregate(env_broad_lab4, fact = 2, fun = mean)

res(env_broad_lab4)
res(env_coarse)

occ_env_coarse <- extract(env_coarse, vect(occ_sf))

resolution_comparison <- data.frame(
  record_id = occ$record_id,
  bio1_fine_c = occ_env_lab4$bio1_c,
  bio1_coarse_c = occ_env_coarse$bio1_c,
  bio12_fine_mm = occ_env_lab4$bio12_mm,
  bio12_coarse_mm = occ_env_coarse$bio12_mm
)

write.csv(
  resolution_comparison,
  "output/04_resolution_extraction_comparison.csv",
  row.names = FALSE
)

png(
  "output/04_temperature_fine_and_coarse.png",
  width = 1800,
  height = 900,
  res = 180
)

par(mfrow = c(1, 2), mar = c(3.5, 3.5, 3, 5))
plot(
  env_broad_lab4[["bio1_c"]],
  main = "Fine grid",
  axes = TRUE
)
points(vect(occ_sf), pch = 16, cex = 0.45)
plot(
  env_coarse[["bio1_c"]],
  main = "Coarse grid",
  axes = TRUE
)
points(vect(occ_sf), pch = 16, cex = 0.45)
dev.off()

# Resampling makes the grid geometry match. It does not recreate information
# that was lost when the fine grid was aggregated.
env_coarse_resampled <- resample(
  env_coarse,
  env_broad_lab4,
  method = "bilinear"
)

res(env_coarse_resampled)

# -----------------------------------------------------------------------------
# 7. Independent extension: inspect one project environmental file
# -----------------------------------------------------------------------------

# In each student's own script, replace the file name below with an environmental
# file downloaded for the term project. The file must be inside the project data/
# folder. Use a .tif, .tiff, or .nc file that terra can read.
#
# project_env <- rast("data/replace_with_your_environmental_file.tif")
# project_env
# names(project_env)
# crs(project_env)
# res(project_env)
# ext(project_env)
# png(
#   "output/04_project_layer_preview.png",
#   width = 1600,
#   height = 1200,
#   res = 180
# )
# plot(project_env)
# dev.off()
