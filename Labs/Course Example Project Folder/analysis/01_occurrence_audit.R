# ECCB 689 Lab 1: Occurrence data, niche reasoning, and reproducible workflows
# Keep this script runnable from the project root.

# -----------------------------------------------------------------------------
# Part A — Guided setup: run these lines with the instructor.
# -----------------------------------------------------------------------------
getwd()
list.files()

occ_raw <- read.csv("data/crotalus_triseriatus_gbif_raw.csv", stringsAsFactors = FALSE)

dim(occ_raw)
names(occ_raw)
head(occ_raw[, c("species", "decimalLongitude", "decimalLatitude", "basisOfRecord")])

# -----------------------------------------------------------------------------
# Part B — Do it yourself: write code below each question.
# Do not replace the raw CSV. Keep the code that produces every result.
# -----------------------------------------------------------------------------

# Problem 1. What is in the raw file?
# Use str(), summary(), table(), and is.na() to inspect the data.
# Report: data types; missing coordinates; basisOfRecord; and one feature
# you would investigate before using the records in a model.



# Problem 2. Define two transparent quality-control rules.
# Rule 1 will check coordinates. Rule 2 will examine duplicate records,
# provenance/sampling, dates, uncertainty, or another defensible issue.
# State the scale at which each rule is appropriate and a possible consequence.

# Example only: replace the placeholders with your own logical rules (T/F rules).
# keep_coordinates <- ...
# keep_second_rule <- ...
# keep <- keep_coordinates & keep_second_rule
# occ_retained <- occ_raw[keep, ]

# Guided baseline for keep_coordinates: first run this code unchanged.
keep_coordinates <- !is.na(occ_raw$decimalLongitude) &
  !is.na(occ_raw$decimalLatitude) &
  occ_raw$decimalLongitude >= -180 &
  occ_raw$decimalLongitude <= -95 &
  occ_raw$decimalLatitude >= -90 &
  occ_raw$decimalLatitude <= 35 &
  occ_raw$decimalLongitude != 0 &
  occ_raw$decimalLatitude != 0

# Guided baseline for keep_second_rule
# round(..., 2) is approximately 1 km
# round(..., 1) is approximately 10 km

coordinate_cell <- paste(
  round(occ_raw$decimalLongitude, 2),
  round(occ_raw$decimalLatitude, 2),
  sep = "_"
)

keep_second_rule <- !duplicated(coordinate_cell)

keep <- keep_coordinates & keep_second_rule
occ_retained <- occ_raw[keep, ]

# Problem 3. Quantify the effect of your decisions.
# Report how many records are retained and excluded by each rule and together.



# Problem 4. Create a diagnostic map.
# After you define occ_retained, use the following code to put a map in your output folder.

#png("output/occurrence_audit_map.png", width = 1600, height = 1200, res = 180)
plot(occ_raw$decimalLongitude, occ_raw$decimalLatitude,
     pch = 16, col = "grey70", xlab = "Longitude", ylab = "Latitude",
     main = "C. triseriatus occurrence records")
points(occ_retained$decimalLongitude, occ_retained$decimalLatitude,
       pch = 16, col = "black")
legend("topleft", legend = c("Raw", "Retained"),
       pch = 16, col = c("grey70", "black"), bty = "n", cex = 0.75)
#dev.off()



# Problem 5. Draft your README interpretation.
# What does the point cloud represent and not represent? Which two environmental
# predictors are next? What movement, dispersal-history, or geographic-barrier 
# process might help explain why a species is not observed in every environmentally 
# favorable place?


