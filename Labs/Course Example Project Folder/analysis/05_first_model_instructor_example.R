# Lab 05 course demonstration
# Run from the course R Project root
library(sf)
library(terra)
set.seed(689)
dir.create("output", showWarnings = FALSE)

out <- function(x) file.path("output", paste0("05_", x))
occ <- read.csv("data/crotalus_triseriatus_retained_lab3.csv")
env <- rast("data/crotalus_triseriatus_worldclim_10m.tif")
env <- env[[c("bio1_c", "bio12_mm")]] # select only the predictors that you will use
occ_sf <- st_as_sf(occ, coords = c("decimalLongitude", "decimalLatitude"),
                   crs = 4326, remove = FALSE)

# Course M from Lab 03
# UTM zone 14N and 250 km are not general defaults for other regions/species
m <- st_transform(st_union(st_buffer(st_transform(occ_sf, 32614), dist = 250000)), 4326)
m_env <- project(vect(m), crs(env))
env_m <- mask(crop(env, m_env), m_env)
occ_v <- project(vect(occ_sf), crs(env_m))
inside <- lengths(st_intersects(occ_sf, st_as_sf(m))) > 0
x <- extract(env_m, occ_v, ID = FALSE)
finite <- apply(as.matrix(x), 1, function(z) all(is.finite(z)))
prep <- data.frame(record_id = occ$record_id, inside_M = inside,
                   complete_predictors = finite)
write.csv(prep, out("model_preparation_audit.csv"), row.names = FALSE)
use <- inside & finite
print(table(inside_M = inside, complete_predictors = finite))
if (any(!use)) warning("Investigate failed rows in model_preparation_audit.csv")
x <- x[use, , drop = FALSE]

# This minimum prevents a trivial number of data points
stopifnot(nrow(x) >= 5)

# Background can overlap occurrence cells: it describes available environments
# This is grid-cell sampling, not equal-area sampling on a lon/lat grid
# Each grid can represents a slightly different area
bg <- spatSample(env_m, size = 1000, method = "random", na.rm = TRUE,
                 as.points = TRUE, values = TRUE)
b <- as.data.frame(bg)[, names(env_m), drop = FALSE]
stopifnot(nrow(b) > 0, all(is.finite(as.matrix(b))))
bxy <- crds(bg)
oxy <- crds(occ_v)[use, , drop = FALSE]
model_data <- rbind(
  data.frame(record_id = as.character(occ$record_id[use]),
             sample_type = "occurrence", presence = 1,
             x = oxy[,1], y = oxy[,2], x),
  data.frame(record_id = paste0("bg_", seq_len(nrow(b))),
             sample_type = "background", presence = 0,
             x = bxy[,1], y = bxy[,2], b))
write.csv(model_data, out("model_data.csv"), row.names = FALSE)

# Print number of occurrences and backgroun points
cat("Occurrences:", nrow(x), " Background:", nrow(b), "\n")

# Print correlation of predictor variables
print(cor(model_data[, names(env_m)]))

# 0 means background
first_model <- glm(presence ~ bio1_c + bio12_mm,
                   data = model_data, family = binomial())
print(summary(first_model))
if (!first_model$converged || any(!is.finite(coef(first_model)))) {
  stop("Unstable fit: inspect separation, sample size, predictor variation")
}

# Investigate any glm warning even if the model converged
saveRDS(first_model, out("first_model.rds"))
capture.output(summary(first_model), file = out("model_summary.txt"))
response <- data.frame(
  bio1_c = seq(min(model_data$bio1_c), max(model_data$bio1_c), length.out = 100),
  bio12_mm = median(model_data$bio12_mm))
response$score <- predict(first_model, newdata = response, type = "response")
write.csv(response, out("response_values.csv"), row.names = FALSE)
png(out("response.png"), width = 1500, height = 1000, res = 160)
plot(response$bio1_c, response$score, type = "l", lwd = 2,
     xlab = "Annual mean temperature (degrees C)",
     ylab = "Occurrence versus background score",
     main = "Exploratory first model")
rug(model_data$bio1_c[model_data$presence == 1])
mtext(paste("Precipitation held at", round(response$bio12_mm), "mm"), cex = .8)
dev.off()

# Conditional curve combinations may have little support in observed data
pred <- predict(env_m, first_model, type = "response", na.rm = TRUE)
writeRaster(pred, out("first_model_score.tif"), overwrite = TRUE)
png(out("first_model_map.png"), width = 1500, height = 1100, res = 160)
plot(pred, main = "Exploratory occurrence versus background score")
plot(m_env, add = TRUE, border = "firebrick3", lwd = 2)
points(occ_v[use, ], pch = 16, cex = .4)
dev.off()

# Capture session information
capture.output(sessionInfo(), file = out("session_info.txt"))
