#!/usr/bin/env Rscript
# Test script for PPV range functions

# Load the package functions
source("R/ppv_range_functions.R")
source("R/ppv_range_plots.R")
source("R/esize_T.R")

# Load example data
load("data/lineup_example.rda")

cat("=== Testing PPV Range Functions ===\n\n")

# Check the structure of the data
cat("Dataset structure:\n")
str(lineup_example)
cat("\n")

cat("Summary of identification decisions:\n")
print(table(lineup_example$identification, lineup_example$target_present))
cat("\n")

# Test 1: Individual correction methods
cat("\n--- Test 1: PPV with nominal correction ---\n")
ppv_nominal <- ppv_by_confidence(lineup_example,
                                  lineup_size = 6,
                                  confidence_bins = c(0, 60, 80, 100),
                                  correction = "nominal")
print(ppv_nominal)

cat("\n--- Test 2: PPV with effective size correction ---\n")
ppv_effective <- ppv_by_confidence(lineup_example,
                                    lineup_size = 6,
                                    confidence_bins = c(0, 60, 80, 100),
                                    correction = "effective")
print(ppv_effective)

cat("\n--- Test 3: PPV with no correction ---\n")
ppv_none <- ppv_by_confidence(lineup_example,
                               lineup_size = 6,
                               confidence_bins = c(0, 60, 80, 100),
                               correction = "none")
print(ppv_none)

# Test 4: PPV range (all three at once)
cat("\n--- Test 4: PPV Range (all three corrections) ---\n")
ppv_range <- ppv_range_by_confidence(lineup_example,
                                      lineup_size = 6,
                                      confidence_bins = c(0, 60, 80, 100))
print(ppv_range)

# Test 5: Visualizations
cat("\n--- Test 5: Creating visualizations ---\n")
library(ggplot2)

cat("Creating PPV range plot...\n")
p1 <- plot_ppv_range(ppv_range)
print(p1)
ggsave("test_ppv_range_plot.png", p1, width = 10, height = 6)
cat("Saved to test_ppv_range_plot.png\n\n")

cat("Creating effective size plot...\n")
p2 <- plot_effective_size_conf(ppv_range)
print(p2)
ggsave("test_effective_size_plot.png", p2, width = 8, height = 6)
cat("Saved to test_effective_size_plot.png\n\n")

cat("Creating error rate plot...\n")
p3 <- plot_error_rate_conf(ppv_range)
print(p3)
ggsave("test_error_rate_plot.png", p3, width = 8, height = 6)
cat("Saved to test_error_rate_plot.png\n\n")

# Test 6: Main wrapper function
cat("\n--- Test 6: make_ppv_range() wrapper function ---\n")
ppv_full <- make_ppv_range(lineup_example,
                            lineup_size = 6,
                            confidence_bins = c(0, 60, 80, 100),
                            show_plots = TRUE,
                            plot_type = "all")
cat("Plots available in $plot_ppv_range, $plot_effective_size, $plot_error_rate\n")

# Test 7: Compare PPV ranges
cat("\n--- Test 7: PPV Range Comparison ---\n")
cat("Confidence bin: PPV_nominal, PPV_effective, PPV_none, Effective_Size\n")
for (i in 1:nrow(ppv_range$ppv_range_data)) {
  row <- ppv_range$ppv_range_data[i, ]
  cat(sprintf("%s: %.3f, %.3f, %.3f, %.2f\n",
             row$confidence,
             row$ppv_nominal,
             row$ppv_effective,
             row$ppv_none,
             row$effective_size))
}

cat("\n=== All tests completed successfully! ===\n")
