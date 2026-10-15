# ECCB 689 Lab 3: Environmental predictors, background, and robustness

# -----------------------------------------------------------------------------
# Part A - Guided workflow
# -----------------------------------------------------------------------------

#install.packages(c("sf","terra","ggplot2"))

library(sf)
library(terra)
library(ggplot2)

# set seed for random number generator
set.seed(689)

# use the standardized data
occ <- read.csv("data/crotalus_triseriatus_retained_lab3.csv")

# convert to spatial object
occ_sf <- st_as_sf(occ, coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326, remove = FALSE)

# load two WorldClim 2.1 predictors at 10 arc-minute resolution
env <- rast("data/crotalus_triseriatus_worldclim_10m.tif")

env # worldclim data bio1 is MAT and bio12 is AP
names(env)
crs(env)
res(env)
ext(env)
global(env, c("min", "max", "mean"), na.rm = TRUE)
plot(env)

# extract environmental values at retained occurrence locations
occ_env <- extract(env, vect(occ_sf))

dim(occ_env)
head(occ_env)

# cbind to merge ID and other info
occ_env <- cbind(
  occ[, c("record_id", "species", "decimalLongitude", "decimalLatitude")],
  occ_env[, c("bio1_c", "bio12_mm")]
)

dim(occ_env)
head(occ_env)

# -----------------------------------------------------------------------------
# Define two distances for accessible area M
# -----------------------------------------------------------------------------

small_buffer_km <- 50
broad_buffer_km <- 250

# transform the spatial data to apply the correct buffer in km
occ_projected <- st_transform(occ_sf, 32614)

# define a function to create the m area buffer
make_m_area <- function(buffer_km) {
  area_projected <- st_union(
    st_buffer(occ_projected, dist = buffer_km * 1000)
  )
  st_transform(area_projected, 4326)
}

m_small <- make_m_area(small_buffer_km)
m_broad <- make_m_area(broad_buffer_km)

plot(m_small)
plot(m_broad)

# crop and mask the environmental layers to each M hypothesis
env_small <- mask(crop(env, vect(m_small)), vect(m_small))
env_broad <- mask(crop(env, vect(m_broad)), vect(m_broad))

# background points represent available environments, not absences
n_background <- 1000

# use spatSample() to randomly select backgrounds
?spatSample
bg_small <- spatSample(env_small, size = n_background, 
  method = "random", na.rm = TRUE, as.points = TRUE, values = TRUE)

bg_broad <- spatSample(env_broad, size = n_background,
  method = "random", na.rm = TRUE, as.points = TRUE, values = TRUE)

# -----------------------------------------------------------------------------
# Compare the two M areas in geographic space
# -----------------------------------------------------------------------------

# I commented out the png() function that saves the plot to a file
# png(
#   "output/background_M_comparison.png",
#   width = 1800,
#   height = 900,
#   res = 180
# )
# 
# par(mfrow = c(1, 2), mar = c(3.5, 3.5, 3, 5))

plot(env_small[["bio1_c"]],
  main = paste0(small_buffer_km, "-km M"),
  axes = TRUE)
plot(vect(m_small), add = TRUE, border = "blue3", lwd = 2)
points(bg_small, pch = 3, cex = 0.45, col = "dodgerblue4")
points(vect(occ_sf), pch = 16, cex = 0.45, col = "black")

plot(env_broad[["bio1_c"]],
  main = paste0(broad_buffer_km, "-km M"),
  axes = TRUE)
plot(vect(m_broad), add = TRUE, border = "firebrick3", lwd = 2)
points(bg_broad, pch = 3, cex = 0.45, col = "firebrick4")
points(vect(occ_sf), pch = 16, cex = 0.45, col = "black")

# this function below closes the png() file
# dev.off()

# -----------------------------------------------------------------------------
# Compare the assumptions in environmental space
# -----------------------------------------------------------------------------

bg_small_df <- as.data.frame(bg_small, geom = "XY")
bg_small_df$scenario <- paste0(small_buffer_km, "-km M")

bg_broad_df <- as.data.frame(bg_broad, geom = "XY")
bg_broad_df$scenario <- paste0(broad_buffer_km, "-km M")

background_env <- rbind(bg_small_df, bg_broad_df)

occurrence_env <- rbind(
  transform(occ_env, scenario = paste0(small_buffer_km, "-km M")),
  transform(occ_env, scenario = paste0(broad_buffer_km, "-km M"))
)

environmental_space_plot <- ggplot(
  background_env,
  aes(x = bio1_c, y = bio12_mm)
) +
  geom_point(color = "grey65", alpha = 0.35, size = 1.2) +
  geom_point(
    data = occurrence_env,
    color = "black",
    alpha = 0.75,
    size = 1.4
  ) +
  facet_wrap(~scenario) +
  labs(
    title = "Occurrence environments under two assumptions about M",
    subtitle = "Black points are occurrences; grey points sample available environments",
    x = "Annual mean temperature (degrees C)",
    y = "Annual precipitation (mm)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "output/environmental_space_M_comparison.png",
  plot = environmental_space_plot,
  width = 9,
  height = 5,
  dpi = 300
)

# -----------------------------------------------------------------------------
# Summarize the environmental samples
# -----------------------------------------------------------------------------

summarize_environment <- function(x, label) {
  data.frame(
    scenario = label,
    n = nrow(x),
    mean_bio1_c = mean(x$bio1_c, na.rm = TRUE),
    min_bio1_c = min(x$bio1_c, na.rm = TRUE),
    max_bio1_c = max(x$bio1_c, na.rm = TRUE),
    mean_bio12_mm = mean(x$bio12_mm, na.rm = TRUE),
    min_bio12_mm = min(x$bio12_mm, na.rm = TRUE),
    max_bio12_mm = max(x$bio12_mm, na.rm = TRUE)
  )
}

environment_summary <- rbind(
  summarize_environment(occ_env, "Occurrences"),
  summarize_environment(bg_small_df, paste0(small_buffer_km, "-km M background")),
  summarize_environment(bg_broad_df, paste0(broad_buffer_km, "-km M background"))
)

print(environment_summary)

write.csv(environment_summary,
  "output/background_environment_summary.csv",
  row.names = FALSE)

# -----------------------------------------------------------------------------
# Part B - Interpretation for README.md
# -----------------------------------------------------------------------------


# 1. Describe the environmental layers, including units, spatial extent, and spatial grain.
# 2. What does a background point represent? Why is it not an absence?
# 3. Which broad-M distance did you select, and what biological assumption does it represent?
# 4. How did changing M alter the sampled environmental space?
# 5. Which interpretation is stable, conditional, or unstable, and what additional assumption would you test next?

