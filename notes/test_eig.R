#!/usr/bin/env Rscript
# Test script for EIG functions

# Load the package functions
source("R/eig_functions.R")

# Load example data
load("data/lineup_example.rda")

cat("=== Testing EIG Functions ===\n\n")

# Check the structure of the data
cat("Dataset structure:\n")
str(lineup_example)
cat("\n")

cat("First few rows:\n")
print(head(lineup_example))
cat("\n")

cat("Summary of identification decisions:\n")
print(table(lineup_example$identification, lineup_example$target_present))
cat("\n")

# Test 1: Basic EIG computation (no binning)
cat("\n--- Test 1: Basic EIG (no confidence binning) ---\n")
eig_result1 <- compute_eig(lineup_example, prior_guilt = 0.5)
print(eig_result1)

# Test 2: EIG with confidence binning
cat("\n--- Test 2: EIG with confidence bins ---\n")
cat("Confidence range:", range(lineup_example$confidence), "\n")
conf_bins <- c(0, 60, 80, 100)
eig_result2 <- compute_eig(lineup_example,
                            prior_guilt = 0.5,
                            confidence_bins = conf_bins)
print(eig_result2)

# Test 3: Summary method
cat("\n--- Test 3: Summary method ---\n")
summary(eig_result2)

# Test 4: Different priors
cat("\n--- Test 4: Effect of different priors ---\n")
priors <- c(0.3, 0.5, 0.7)
for (p in priors) {
  result <- compute_eig(lineup_example, prior_guilt = p, confidence_bins = conf_bins)
  cat(sprintf("Prior = %.1f: EIG = %.4f bits\n", p, result$eig))
}

# Test 5: Plotting functions
cat("\n--- Test 5: Plotting functions ---\n")
library(ggplot2)
cat("Creating information gain plot...\n")
p1 <- plot_eig(eig_result2)
print(p1)
ggsave("test_eig_plot.png", p1, width = 8, height = 6)
cat("Saved to test_eig_plot.png\n\n")

cat("Creating posterior probability plot...\n")
p2 <- plot_eig_posteriors(eig_result2)
print(p2)
ggsave("test_posteriors_plot.png", p2, width = 8, height = 6)
cat("Saved to test_posteriors_plot.png\n\n")

# Test 6: Main wrapper function
cat("\n--- Test 6: make_eig() wrapper function ---\n")
eig_full <- make_eig(lineup_example,
                     prior_guilt = 0.5,
                     confidence_bins = c(0, 60, 80, 100),
                     show_plot = TRUE,
                     plot_type = "both")
cat("Plots available in $plot_ig and $plot_posteriors\n")

cat("\n=== All tests completed successfully! ===\n")
