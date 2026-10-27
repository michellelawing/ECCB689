# ECCB 689 Lab 06: Comparing and Evaluating Models
# Instructor example -- run from the course .Rproj root in a fresh R session.
# Outputs: output/06_* (same filenames required in the Lab 06 handout).
# Packages: mgcv (install once in the Console if necessary).

# PART A -- Load and inspect the Lab 05 modeling table
library(mgcv)
set.seed(689)
d <- read.csv("output/05_model_data.csv")
stopifnot(all(c("presence", "bio1_c", "bio12_mm", "x", "y") %in% names(d)))
vars <- c("bio1_c", "bio12_mm")
stopifnot(all(d$presence %in% c(0, 1)))
stopifnot(all(is.finite(as.matrix(d[, vars]))))
print(table(d$presence))

#checks
stopifnot(all(c("x", "y") %in% names(d)))
stopifnot(all(is.finite(d$x)), all(is.finite(d$y)))

# PART B -- Hold out locations BEFORE fitting either model
location <- paste(d$x, d$y, sep = "_")
sites <- unique(location)
train_sites <- sample(sites, floor(0.7 * length(sites)))
in_train <- location %in% train_sites
train <- d[in_train, ]
test <- d[!in_train, ]
stopifnot(length(unique(train$presence)) == 2,
          length(unique(test$presence)) == 2)
print(table(train$presence))
print(table(test$presence))
stopifnot(!any(unique(location[in_train]) %in% location[!in_train]))

write.csv(data.frame(row = seq_len(nrow(d)), in_train),
  "output/06_data_split.csv", row.names = FALSE)

# Fit a simple GLM and a modest GAM to TRAINING data only.
# k = 4 limits basis size; REML estimates smoothness using the training data.
glm_fit <- glm(presence ~ bio1_c + bio12_mm, data = train, family = binomial())
gam_fit <- gam(presence ~ s(bio1_c, k = 4) + s(bio12_mm, k = 4),
  data = train, family = binomial(), method = "REML")
print(summary(glm_fit))
print(summary(gam_fit))
stopifnot(glm_fit$converged, gam_fit$converged)

# PAUSE: inspect warnings and convergence before interpreting

# PART C -- Evaluate the SAME held-out rows
test$glm_score <- predict(glm_fit, test, type = "response")
test$gam_score <- predict(gam_fit, test, type = "response")
stopifnot(all(is.finite(test$glm_score)),
          all(is.finite(test$gam_score)))
write.csv(test, "output/06_test_predictions.csv",
  row.names = FALSE)

# AUC is the probability a random occurrence outranks a random background row.
# helper function
auc_score <- function(y, score) {
  stopifnot(length(y) == length(score), all(y %in% c(0, 1)),
            all(is.finite(score)), length(unique(y)) == 2)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
comparison <- data.frame(
  model = c("GLM", "GAM"),
  test_auc = c(auc_score(test$presence, test$glm_score),
               auc_score(test$presence, test$gam_score)))
print(comparison)

# Background region, sampling bias, and train/test dependence affect evaluation.
write.csv(comparison, "output/06_model_comparison.csv", row.names = FALSE)
saveRDS(list(GLM = glm_fit, GAM = gam_fit), "output/06_models.rds")

# PART D -- Compare conditional responses on the SAME predictor grid
# Hold precipitation at the training median; vary temperature over training range.
curve <- data.frame(
  bio1_c = seq(min(train$bio1_c), max(train$bio1_c), length.out = 100),
  bio12_mm = median(train$bio12_mm))
cat("Precipitation held at", median(train$bio12_mm), "mm\n")
curve$GLM <- predict(glm_fit, curve, type = "response")
curve$GAM <- predict(gam_fit, curve, type = "response")
png("output/06_response_comparison.png",
    width = 1600, height = 1100, res = 170)
plot(curve$bio1_c, curve$GLM, type = "l", lwd = 2,
  ylim = c(0, 1), xlab = "Annual mean temperature (degrees C)",
  ylab = "Occurrence versus background score")
lines(curve$bio1_c, curve$GAM, col = "firebrick3", lwd = 2)
legend("topright", c("GLM", "GAM"),
  col = c("black", "firebrick3"), lwd = 2, bty = "n")
rug(train$bio1_c[train$presence == 1])
dev.off()

# Message
cat("\nSaved Lab 06 outputs in output/.\n")
