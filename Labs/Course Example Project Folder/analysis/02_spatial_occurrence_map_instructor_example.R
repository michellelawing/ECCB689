# ECCB 689 Lab 2: Spatial Occurrence Data, Mapping, and BAM Reasoning
# Keep your script runnable from the project root

# -----------------------------------------------------------------------------
# Install libraries
# -----------------------------------------------------------------------------
# Run once, then comment out
install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata"))

# -----------------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------------
library(sf)
library(ggplot2)
library(rnaturalearth)

# -----------------------------------------------------------------------------
# Read the raw occurrence data and recreate the baseline Lab 1 decisions
# -----------------------------------------------------------------------------
# source() is a function to read and fully run another script
source("analysis/01_occurrence_audit.R")

# stopifnot() will throw an error if input is false
# exists() asks if an object is defined, input as character string
stopifnot(exists("occ_raw"))
stopifnot(exists("occ_retained"))

# return the number of rows with each dataset
nrow(occ_raw)
nrow(occ_retained)

# -----------------------------------------------------------------------------
# Convert occurrence tables into simple feature (sf) point objects
# -----------------------------------------------------------------------------

# Convert ordinary data frames to sf point objects
occ_raw_sf <- st_as_sf(occ_raw, 
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326, remove = FALSE)

# coords tells R which columns contain x and y coordinates
# crs = 4326 identifies the coordinates as longitude/latitude in WGS 84
# remove = FALSE keeps the original longitude and latitude columns

occ_sf <- st_as_sf(occ_retained,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326, remove = FALSE)

# -----------------------------------------------------------------------------
# Inspect the spatial object
# -----------------------------------------------------------------------------

# an sf object contains both an attribute table and a geometry column
head(occ_sf)

# look directly at the coordinates stored in the geometry
# in EPSG:4326 these values are degrees longitude and latitude
head(st_coordinates(occ_sf))

# check the coordinate reference system
st_crs(occ_sf)

# check the type of geometry
st_geometry_type(occ_sf)

# check the geographic extent of the object
st_bbox(occ_sf)

# -----------------------------------------------------------------------------
# Quick spatial plotting with base R
# -----------------------------------------------------------------------------

# sf objects have their own plot method, but you can use base
# plot() for quickly inspecting spatial data
plot(occ_sf["year"])

# to plot only the geometry use st_geometry()
plot(st_geometry(occ_sf))

# no attribute information is displayed

# -----------------------------------------------------------------------------
# Build maps using ggplot2 with the grammar of graphics
# -----------------------------------------------------------------------------

# ggplot() creates the plotting canvas
ggplot()

# geom_sf() adds an sf spatial layer, use +
ggplot() +
  geom_sf(data = occ_sf)

# -----------------------------------------------------------------------------
# Set the appearance of a ggplot() plot
# -----------------------------------------------------------------------------

# use color and size to adjust the appearance of every feature in the layer
ggplot() +
  geom_sf(data = occ_sf, color = "red", size = 2)

# values inside aes() connect an appearance to a variable in the data
ggplot() +
  geom_sf(data = occ_sf, aes(color = year), size = 1.5)

# -----------------------------------------------------------------------------
# Add a geographic layer to set a better context (base map in GIS)
# -----------------------------------------------------------------------------

# use data available through XX package to download country 
# polygons as an sf object

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
st_crs(world)

# maps in ggplot2 are built as layers and drawn in order
ggplot() +
  geom_sf(data = world, fill = "grey95",
    color = "grey70", linewidth = 0.2) +
  geom_sf(data = occ_sf, color = "red",
    size = 1.2, alpha = 0.7) +
  labs(title = "C. triseriatus occurrence records") +
  theme_minimal(base_size = 12)

ggplot() +
  geom_sf(data = occ_sf, color = "red",
          size = 1.2, alpha = 0.7) +
  geom_sf(data = world, fill = "grey95",
          color = "grey70", linewidth = 0.2) +
  labs(title = "C. triseriatus occurrence records") +
  theme_minimal(base_size = 12)

# -----------------------------------------------------------------------------
# Compare the original and retained occurrence records
# -----------------------------------------------------------------------------

ggplot() +
  geom_sf(data = world, fill = "grey95", 
    color = "grey70", linewidth = 0.2) +
  geom_sf(data = occ_raw_sf,
          color = "red", size = 1, alpha = 0.7) +
  geom_sf(data = occ_sf,
    color = "blue3", size = 1, alpha = 0.7) +
  labs(
    title = "C. triseriatus occurrence records") +
  theme_minimal(base_size = 12)

# make sure the layers are in the right order for display

# -----------------------------------------------------------------------------
# Control the map extent
# -----------------------------------------------------------------------------

# save your map to an object
map_output <- ggplot() +
  geom_sf(data = world, fill = "grey95", 
          color = "grey70", linewidth = 0.2) +
  geom_sf(data = occ_raw_sf,
          color = "red", size = 1, alpha = 0.7) +
  geom_sf(data = occ_sf,
          color = "blue3", size = 1, alpha = 0.7) +
  labs(title = "C. triseriatus occurrence records") +
  theme_minimal(base_size = 12)

# use the bounding box of the raw occurrence records
bb <- st_bbox(occ_raw_sf)
bb

# add a small amount of geographic space around the records
x_pad <- max(0.25, 0.10 * (bb["xmax"] - bb["xmin"]))
y_pad <- max(0.25, 0.10 * (bb["ymax"] - bb["ymin"]))

# coord_sf() changes the viewing window
# it does NOT remove observations or alter their coordinates

map_output +
  coord_sf(
    xlim = c(bb["xmin"] - x_pad, bb["xmax"] + x_pad),
    ylim = c(bb["ymin"] - y_pad, bb["ymax"] + y_pad),
    expand = FALSE)

# -----------------------------------------------------------------------------
# Save the map
# -----------------------------------------------------------------------------

# a ggplot can be stored as an R object and then saved reproducibly

ggsave("output/occurrence_map_sf.png",
  plot = map_output, width = 7, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# Transform to a projected coordinate reference system
# -----------------------------------------------------------------------------

# the occurrence data are currently in WGS 84 (EPSG:4326)
# their coordinates are longitude and latitude measured in degrees
head(st_coordinates(occ_sf))

# EPSG:32614 is WGS 84 / UTM Zone 14N
# st_transform() changes the coordinate representation 

occ_projected <- st_transform(occ_sf, 32614)

# compare the CRS and coordinates before and after transformation
head(st_coordinates(occ_sf))
head(st_coordinates(occ_projected))

# the projected coordinates are now expressed in meters
st_bbox(occ_projected)

# -----------------------------------------------------------------------------
# Explore a simple hypothesis about M
# -----------------------------------------------------------------------------

# create a 100-km buffer around each retained occurrence point
# projected coordinates are in meters, so 100 km = 100,000 m
m_buffer <- st_buffer(occ_projected, dist = 100000)

# combine overlapping buffers into one accessible area
m_area <- st_union(m_buffer)

#plot
ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70") +
  geom_sf(data = st_transform(m_area, 4326),
    fill = "lightblue", color = "blue", alpha = 0.3) +
  geom_sf(data = occ_sf, color = "black") +
  coord_sf(xlim = c(bb["xmin"] - x_pad, bb["xmax"] + x_pad),
    ylim = c(bb["ymin"] - y_pad, bb["ymax"] + y_pad),
    expand = FALSE) +
  theme_minimal(base_size = 12)
